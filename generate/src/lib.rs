use anyhow::Result;
use if_chain::if_chain;
use once_cell::sync::Lazy;
use public_api::tokens::Token;
use regex::Regex;
use rustdoc_types::{Crate, Function, Id, Impl, ItemEnum, Type};
use std::{
    cell::RefCell,
    collections::{BTreeMap, BTreeSet, HashSet},
    fs::{File, OpenOptions},
    io::Write,
    path::{Path, PathBuf},
};

mod public_item_map;
use public_item_map::PublicItemMap;

mod source_builder;
use source_builder::Module;

mod util;
use util::{
    path_prefix, qualified_function, qualified_type, qualified_type_function, simplified_self,
    FunctionExt, GenericBoundsExt, TokenExt, TokensExt,
};

pub const TOOLCHAIN: &str = "nightly-2024-10-13";
pub const COMMIT: &str = "6b9676b45431a1e531b9c5f7bd289fc36a312749";

#[derive(Clone, Copy)]
enum MapKind {
    Value { pair: bool },
    Err,
}

#[derive(Default)]
struct StructImpls {
    is_copy: bool,
    is_deref: bool,
    is_unsized: bool,
}

#[cfg_attr(dylint_lib = "general", allow(abs_home_path))]
static STD_JSON: Lazy<PathBuf> =
    Lazy::new(|| Path::new(env!("CARGO_MANIFEST_DIR")).join("assets/std.json"));

static IGNORED_PATHS: Lazy<Vec<Vec<Token>>> = Lazy::new(|| {
    const IGNORED_PATH_PREFIXES: &[&[&str]] = &[
        &["std", "io", "prelude"],
        &["std", "os", "unix", "prelude"],
        &["std", "os", "wasi", "prelude"],
        &["std", "os", "windows", "prelude"],
    ];

    std::iter::once({
        // smoelius: Many different `try_from` functions get lumped under the same `Id`, which
        // causes an "ambiguous parent" error. Revisit this.
        qualified_type(&["core", "convert"], "TryFrom")
    })
    .chain(
        IGNORED_PATH_PREFIXES
            .iter()
            .map(|prefix| path_prefix(prefix)),
    )
    .chain(std::iter::once({
        // smoelius: `std::os::unix::process::ExitStatusExt' has functions `from_raw` and
        // `into_raw` that cause compile errors when wrapped. Revisit this.
        qualified_type(&["std", "os", "unix", "process"], "ExitStatusExt")
    }))
    .chain(std::iter::once({
        // smoelius: `catch_unwind`'s return type is `std::thread::Result`, whose error type is
        // `Box<dyn Any + Send + 'static>`. This type does not satisfy `std::error::Error`, which
        // `anyhow::Result` requires.
        qualified_function(&["std", "panic"], "catch_unwind")
    }))
    .collect()
});

// smoelius: Structs with generic params are currently unsupported.
static GENERIC_STRUCTS: Lazy<Vec<Vec<Token>>> = Lazy::new(|| {
    const INNER: &[(&[&str], &str)] = &[
        (&["std", "collections", "hash_map"], "HashMap"),
        (&["std", "collections", "hash_map"], "RawEntryBuilder"),
        (&["std", "collections", "hash_set"], "HashSet"),
        (&["std", "io"], "BufReader"),
        (&["std", "panic"], "PanicHookInfo"),
        (&["std", "sync"], "LazyLock"),
        (&["std", "sync"], "MappedMutexGuard"),
        (&["std", "sync"], "MappedRwLockReadGuard"),
        (&["std", "sync"], "MappedRwLockWriteGuard"),
        (&["std", "sync"], "Mutex"),
        (&["std", "sync"], "MutexGuard"),
        (&["std", "sync"], "OnceLock"),
        (&["std", "sync"], "RwLock"),
        (&["std", "sync"], "RwLockReadGuard"),
        (&["std", "sync"], "RwLockWriteGuard"),
        (&["std", "sync", "mpmc"], "Receiver"),
        (&["std", "sync", "mpmc"], "Sender"),
        (&["std", "sync", "mpsc"], "Receiver"),
        (&["std", "sync", "mpsc"], "Sender"),
        (&["std", "sync", "mpsc"], "SyncSender"),
        (&["std", "thread"], "JoinHandle"),
        (&["std", "thread"], "LocalKey"),
        (&["std", "thread"], "ScopedJoinHandle"),
    ];

    INNER
        .iter()
        .map(|(path, ty)| qualified_type(path, ty))
        .collect()
});

static PATHS_REQUIRING_MAP_PAIR: Lazy<Vec<Vec<Token>>> = Lazy::new(|| {
    const INNER: &[(&[&str], &str, &str)] = &[
        (&["std", "os", "unix", "net"], "UnixDatagram", "pair"),
        (&["std", "os", "unix", "net"], "UnixStream", "pair"),
    ];

    INNER
        .iter()
        .map(|(path, ty, name)| qualified_type_function(path, ty, name))
        .collect()
});

static REWRITABLE_PATHS: Lazy<Vec<(Vec<Token>, Vec<Token>)>> = Lazy::new(|| {
    const SHORTENABLE_PATHS: &[&[&str]] = &[
        &["core", "net", "ip_addr"],
        &["core", "net", "socket_addr"],
        &["core", "num", "nonzero"],
        &["core", "ops", "function"],
        &["core", "panic", "unwind_safe"],
        &["std", "ffi", "os_str"],
    ];

    const OTHER_REWRITABLE_PATHS: &[(&[&str], &[&str])] = &[
        (&["alloc"], &["std"]),
        (&["core", "iter", "traits", "collect"], &["core", "iter"]),
    ];

    SHORTENABLE_PATHS
        .iter()
        .map(|prefix| {
            (
                path_prefix(prefix),
                path_prefix(&prefix[..prefix.len() - 1]),
            )
        })
        .chain(
            OTHER_REWRITABLE_PATHS
                .iter()
                .map(|(from, to)| (path_prefix(from), path_prefix(to))),
        )
        .collect()
});

pub fn generate(root: impl AsRef<Path>) -> Result<()> {
    let generator = Generator::new()?;

    generator.generate(root)?;

    generator.write_clippy_toml()?;

    Ok(())
}

struct Generator {
    krate: Crate,
    public_item_map: PublicItemMap,
    disallowed: RefCell<BTreeSet<String>>,
}

impl Generator {
    fn new() -> Result<Self> {
        let file = File::open(&*STD_JSON)?;

        let krate = serde_json::from_reader::<_, Crate>(file)?;
        let public_api = public_api::Builder::from_rustdoc_json(&*STD_JSON).build()?;
        let mut generator = Self {
            krate,
            public_item_map: PublicItemMap::default(),
            disallowed: RefCell::new(BTreeSet::new()),
        };
        generator
            .public_item_map
            .populate_from_public_api(public_api, |tokens| {
                if IGNORED_PATHS
                    .iter()
                    .any(|path| tokens.position(path).is_some())
                {
                    return Some("ignored_path");
                }
                if tokens.iter().any(|token| matches!(token, Token::Function(s) if s == "into_inner" || s == "to_inner")) {
                    return Some("overridden_function");
                }
                None
            })?;
        Ok(generator)
    }

    #[allow(clippy::too_many_lines)]
    fn generate(&self, root: impl AsRef<Path>) -> Result<()> {
        let mut module = Module::new(&self.public_item_map);

        let parents_of_instrumentable_functions = self.parents_of_instrumentable_functions();

        for (&id, public_items) in self.public_item_map.iter() {
            let Some((function, has_result_output, has_option_output)) = self.is_function(id)
            else {
                continue;
            };

            // smoelius: An `Id` may correspond to multiple `PublicItem`s. If _any_ of the
            // `PublicItem`s' parents are in `parents_of_instrumentable_functions`, proceed.
            if !public_items
                .iter()
                .filter_map(|&(parent_id, _)| parent_id)
                .any(|parent_id| parents_of_instrumentable_functions.contains(&parent_id))
            {
                continue;
            };

            let parent_id = self.public_item_map.parent_id(id).unwrap();

            let parent_item = self.krate.index.get(&parent_id).unwrap();

            let has_sealed_trait_bound = match &parent_item.inner {
                ItemEnum::Trait(trait_) => trait_.bounds.has_trait_bound_with_name("Sealed"),
                ItemEnum::Impl(impl_) => {
                    // smoelius: Ignore impls for primitive types.
                    if matches!(impl_.for_, Type::Primitive(_)) {
                        continue;
                    }
                    // smoelius: Ignore `impl Trait for Struct` constructs.
                    if impl_.trait_.is_some() {
                        continue;
                    }
                    false
                }
                ItemEnum::Module(_) => false,
                _ => panic!(),
            };

            // smoelius: Fetch the parent's tokens first so that if they contain a
            // `qualified_struct`, that `qualified_struct` can be passed to `patch_tokens`.
            let (parent_tokens, _) = patch_tokens(
                self.public_item_map.tokens(parent_id),
                &[],
                has_sealed_trait_bound,
                false,
                false,
            );

            let qualified_trait = parent_tokens.extract_trait();
            let qualified_struct = parent_tokens.extract_struct();

            // smoelius: Structs with generic params are currently unsupported.
            let (generic_params, qualified_struct) =
                qualified_struct.extract_initial_generic_params();
            if !generic_params.is_empty() {
                assert!(
                    GENERIC_STRUCTS
                        .iter()
                        .any(|path| qualified_struct.starts_with(path)),
                    "{qualified_struct:?}"
                );
                continue;
            }

            let (tokens, map_kind) = patch_tokens(
                self.public_item_map.tokens(id),
                qualified_struct,
                false,
                has_result_output,
                has_option_output,
            );

            // smoelius: Fetch the parent's attributes first. If the function does not belong to a
            // trait or struct, they will append to the function's attributes so that they are not
            // lost.
            let parent_attrs = self.item_attrs(parent_id, true, &parent_tokens);

            let (attrs, struct_is_copy) = match parent_item.inner {
                ItemEnum::Trait(_) => {
                    assert!(!qualified_trait.is_empty());
                    assert!(qualified_struct.is_empty());
                    let (trait_path, _) = qualified_trait.extract_initial_path();
                    module.add_trait(&trait_path, &parent_attrs, qualified_trait);
                    (self.item_attrs(id, false, &tokens), false)
                }
                ItemEnum::Impl(_) => {
                    assert!(qualified_trait.is_empty());
                    assert!(!qualified_struct.is_empty());
                    let (struct_path, struct_tokens) = qualified_struct.extract_initial_path();
                    let (qualified_struct, replaced_in_qualified_struct) =
                        qualified_struct.deanonymize_lifetimes();
                    let (struct_tokens, replaced_in_struct_tokens) =
                        struct_tokens.deanonymize_lifetimes();
                    assert_eq!(replaced_in_qualified_struct, replaced_in_struct_tokens);
                    let struct_impls = self.struct_impls(parent_id);
                    module.add_struct_def(
                        &struct_path,
                        &parent_attrs,
                        &qualified_struct,
                        &struct_def_and_impls(
                            &qualified_struct,
                            &struct_tokens,
                            replaced_in_struct_tokens,
                            &struct_impls,
                        ),
                    );
                    (self.item_attrs(id, false, &tokens), struct_impls.is_copy)
                }
                ItemEnum::Module(_) => {
                    assert!(qualified_trait.is_empty());
                    assert!(qualified_struct.is_empty());
                    let mut attrs = self.item_attrs(id, false, &tokens);
                    attrs.extend(parent_attrs);
                    (attrs, false)
                }
                _ => panic!(),
            };

            let fn_suffix = tokens.strip_leading_qualifiers_and_fn();

            let (fn_path, fn_suffix) = fn_suffix.extract_initial_path();

            let (_, fn_suffix) = fn_suffix.extract_initial_trait_or_type();

            let fn_def = fn_def(
                function,
                qualified_trait,
                qualified_struct,
                &fn_path,
                fn_suffix,
                map_kind,
                struct_is_copy,
            );

            module.add_fn_def(
                &fn_path,
                // smoelius: At most one of `qualified_trait` and `qualified_struct` is non-empty.
                // Concatenating them has the effect of select the non-empty one.
                &[qualified_trait, qualified_struct].concat(),
                &attrs,
                &fn_def,
            );

            if matches!(map_kind, Some(MapKind::Value { .. })) {
                self.disallow(qualified_trait, qualified_struct, &fn_path, fn_suffix);
            }
        }

        module.write(root)?;

        Ok(())
    }

    fn parents_of_instrumentable_functions(&self) -> HashSet<Id> {
        let mut parent_ids = HashSet::new();
        for (&id, public_items) in self.public_item_map.iter() {
            for (parent_id, _) in public_items {
                let &Some(parent_id) = parent_id else {
                    continue;
                };
                if let Some((_, true, _) | (_, _, true)) = self.is_function(id) {
                    parent_ids.insert(parent_id);
                }
            }
        }
        parent_ids
    }

    fn is_function(&self, id: Id) -> Option<(&Function, bool, bool)> {
        let function = self.is_function_inner(id)?;

        let has_result_output = if_chain! {
            if let Some(Type::ResolvedPath(path)) = &function.sig.output;
            if path.name.ends_with("Result");
            // smoelius: `std::sync::BarrierWaitResult` is not a `std::result::Result`.
            if path.name != "BarrierWaitResult";
            // smoelius: The problem is not `std::sync::LockResult` itself, but how it is used. In
            // many cases, it is instantiated with a type that is not `Send`, which
            // `anyhow::Result` requires. `std::sync::Mutex::lock` provides an example:
            // https://doc.rust-lang.org/beta/std/sync/struct.Mutex.html#method.lock
            if path.name != "LockResult";
            then {
                true
            } else {
                false
            }
        };

        let has_option_output = if_chain! {
            if let Some(Type::ResolvedPath(path)) = &function.sig.output;
            if path.name == "Option";
            then {
                true
            } else {
                false
            }
        };

        Some((function, has_result_output, has_option_output))
    }

    fn is_function_inner(&self, id: Id) -> Option<&Function> {
        if_chain! {
            if let Some(item) = self.krate.index.get(&id);
            if let ItemEnum::Function(function) = &item.inner;
            then {
                Some(function)
            } else {
                None
            }
        }
    }

    /// Gets a [`rustdoc_types::Item`]'s attributes, including its [`TokensExt::required_gates`] as
    /// determined by the `tokens` argument
    fn item_attrs(&self, id: Id, walk_parents: bool, tokens: &[Token]) -> Vec<String> {
        let mut attrs = self.item_attrs_inner(id, walk_parents);
        attrs.extend(tokens.required_gates());
        attrs
    }

    fn item_attrs_inner(&self, id: Id, walk_parents: bool) -> Vec<String> {
        let item = self.krate.index.get(&id).unwrap();
        let mut attrs = rewrite_attrs(&item.attrs);
        if walk_parents {
            let mut iter = self.public_item_map.parent_ids(id);
            if let Some(parent_id) = iter.next() {
                let mut parent_attrs = self.item_attrs_inner(parent_id, walk_parents);
                for parent_id in iter {
                    let other_attrs = self.item_attrs_inner(parent_id, walk_parents);
                    parent_attrs.retain(|x| other_attrs.iter().any(|y| x == y));
                }
                attrs.extend(parent_attrs);
            }
        }
        attrs
    }

    fn struct_impls(&self, struct_impl_id: Id) -> StructImpls {
        let struct_id = self.public_item_map.parent_id(struct_impl_id).unwrap();
        let ItemEnum::Struct(strukt) = &self.krate.index.get(&struct_id).unwrap().inner else {
            panic!();
        };
        let mut struct_impls = StructImpls::default();
        for &id in &strukt.impls {
            let ItemEnum::Impl(Impl {
                trait_: Some(path),
                is_negative,
                ..
            }) = &self.krate.index.get(&id).unwrap().inner
            else {
                continue;
            };
            if path.name == "Copy" {
                struct_impls.is_copy = !is_negative;
            } else if path.name == "Deref" {
                struct_impls.is_deref = !is_negative;
            } else if path.name == "Sized" {
                // smoelius: N.B. The lack of negation.
                struct_impls.is_unsized = *is_negative;
            }
        }
        struct_impls
    }

    fn disallow(
        &self,
        qualified_trait: &[Token],
        qualified_struct: &[Token],
        fn_path: &[&str],
        fn_suffix: &[Token],
    ) {
        let mut disallowed = self.disallowed.borrow_mut();

        let disallowable_qualified_fn =
            disallowable_qualified_fn(qualified_trait, qualified_struct, fn_path, fn_suffix);

        // smoelius: Disallowing `std::io::Write::write_fmt` causes Clippy to warn about `writeln!`.
        if disallowable_qualified_fn
            == qualified_type_function(&["std", "io"], "Write", "write_fmt")
        {
            return;
        }

        // smoelius: `disallowed` is only for the clippy.toml file, so yolo.
        disallowed.insert(disallowable_qualified_fn.to_string_unchecked());
    }

    fn write_clippy_toml(&self) -> Result<()> {
        let disallowed = self.disallowed.borrow();

        #[cfg_attr(dylint_lib = "general", allow(abs_home_path))]
        let path = Path::new::<str>(env!("CARGO_MANIFEST_DIR")).join("../clippy_example.toml");

        let mut file = OpenOptions::new()
            .create(true)
            .truncate(true)
            .write(true)
            .open(path)?;

        writeln!(file, "disallowed-methods = [")?;
        for tokens in disallowed.iter() {
            let tokens = tokens.to_string().replace(' ', "");
            writeln!(
                file,
                r#"    {{ path = "{tokens}", reason = "use `elaborate::{tokens}`"}},"#
            )?;
        }
        writeln!(file, "]")?;

        Ok(())
    }
}

static UNSTABLE_RE: Lazy<Regex> =
    Lazy::new(|| Regex::new(r#"#!?\[unstable\([^)]*(\<feature = "[^"]*")[^)]*\)]"#).unwrap());

fn rewrite_attrs(attrs: &[String]) -> Vec<String> {
    attrs
        .iter()
        .filter_map(|attr| {
            if let Some(caps) = UNSTABLE_RE.captures(attr) {
                assert_eq!(2, caps.len());
                Some(format!("#[cfg({})]\n", &caps[1]))
            } else {
                None
            }
        })
        .collect()
}

/// Performs various fix-ups on `tokens`. Argument `qualified_struct` is used to identify places
/// where `Self` can be introduced.
fn patch_tokens(
    tokens: &[Token],
    qualified_struct: &[Token],
    has_sealed_trait_bound: bool,
    has_result_output: bool,
    has_option_output: bool,
) -> (Vec<Token>, Option<MapKind>) {
    let mut tokens = tokens.to_vec();

    for (from, to) in &*REWRITABLE_PATHS {
        (tokens, _) = tokens.replace(from, to);
    }

    if has_sealed_trait_bound {
        tokens = tokens.remove_sealed();
    }

    if !qualified_struct.is_empty() {
        tokens = tokens.selectively_collapse_self(qualified_struct);
    }

    let map_kind = if has_result_output || has_option_output {
        Some(if tokens.error_type_is_self() {
            MapKind::Err
        } else {
            tokens = tokens.rewrite_output_type();
            let pair = PATHS_REQUIRING_MAP_PAIR
                .iter()
                .any(|path| tokens.position(path).is_some());
            MapKind::Value { pair }
        })
    } else {
        None
    };

    (tokens, _) = tokens.replace(&[Token::primitive("never")], &[Token::primitive("!")]);

    (tokens, map_kind)
}

static MANUAL_TRAIT_IMPLEMENTATIONS: Lazy<BTreeMap<Vec<Token>, Vec<String>>> = Lazy::new(|| {
    [(
        qualified_type(&["std", "path"], "Path"),
        &[
            "\
impl From<&std::path::Path> for &Path {
    fn from(value: &std::path::Path) -> Self {
        unsafe { &*(std::ptr::from_ref::<std::path::Path>(value) as *const Path) }
    }
}",
            "\
impl<'a> crate::Elaborate for &'a std::path::Path {
    type Output = &'a Path;
    fn elaborate(self) -> Self::Output {
        self.into()
    }
}",
        ],
    )]
    .into_iter()
    .map(|(qualified_type, impls)| {
        (
            qualified_type,
            impls.iter().map(|&s| s.to_owned()).collect::<Vec<_>>(),
        )
    })
    .collect()
});

fn struct_def_and_impls(
    qualified_struct: &[Token],
    struct_tokens: &[Token],
    needs_lifetime: bool,
    struct_impls: &StructImpls,
) -> Vec<String> {
    let manual_trait_impls = MANUAL_TRAIT_IMPLEMENTATIONS
        .get(qualified_struct)
        .cloned()
        .unwrap_or_default();

    let (struct_params, trait_params) = if needs_lifetime {
        (String::from("<'a>"), String::from("<'a, T: ?Sized>"))
    } else {
        (String::new(), String::from("<T: ?Sized>"))
    };
    let qualified_struct = qualified_struct.to_string();
    let struct_tokens = struct_tokens.to_string();

    [
        format!(
            "\
#[repr(transparent)]
pub struct {struct_tokens} {{
    pub(crate) inner: {qualified_struct},
}}"
        ),
        format!(
            "\
impl{struct_params} {struct_tokens} {{
    pub fn to_inner(&self) -> &{qualified_struct} {{
        &self.inner
    }}
}}"
        ),
        if struct_impls.is_unsized {
            String::new()
        } else {
            format!(
                "\
impl{struct_params} {struct_tokens} {{
    pub fn into_inner(self) -> {qualified_struct} {{
        self.inner
    }}
}}"
            )
        },
        format!(
            "\
impl{trait_params} AsRef<T> for {struct_tokens}
where
    {qualified_struct}: AsRef<T>,
{{
    fn as_ref(&self) -> &T {{
        <{qualified_struct} as AsRef<T>>::as_ref(&self.inner)
    }}
}}"
        ),
        if struct_impls.is_deref {
            format!(
                "\
impl{trait_params} std::ops::Deref for {struct_tokens}
where
    {qualified_struct}: std::ops::Deref<Target = T>,
{{
    type Target = T;
    fn deref(&self) -> &T {{
        <{qualified_struct} as std::ops::Deref>::deref(&self.inner)
    }}
}}"
            )
        } else {
            String::new()
        },
        if struct_impls.is_unsized {
            String::new()
        } else {
            format!(
                "\
impl{struct_params} From<{qualified_struct}> for {struct_tokens} {{
    fn from(value: {qualified_struct}) -> Self {{
        Self {{ inner: value }}
    }}
}}"
            )
        },
        if struct_impls.is_unsized {
            String::new()
        } else {
            format!(
                "\
impl{struct_params} crate::Elaborate for {qualified_struct} {{
    type Output = {struct_tokens};
    fn elaborate(self) -> Self::Output {{
        self.into()
    }}
}}"
            )
        },
    ]
    .into_iter()
    .chain(manual_trait_impls)
    .collect()
}

static MANUAL_FUNCTION_IMPLEMENTATIONS: Lazy<Vec<(Vec<Token>, &str)>> = Lazy::new(|| {
    const MANUAL_OS_STR_FUNCTION_IMPLEMENTATIONS: &[(&str, &str)] = &[
        (
            "from_encoded_bytes_unchecked",
            "
    let os_str = std::ffi::OsStr::from_encoded_bytes_unchecked(bytes);
    &*(std::ptr::from_ref::<std::ffi::OsStr>(os_str) as *const Self)
",
        ),
        (
            "into_os_string",
            "
    let boxed_os_str = unsafe {
        std::mem::transmute::<std::boxed::Box<Self>, std::boxed::Box<std::ffi::OsStr>>(self)
    };
    std::ffi::OsStr::into_os_string(boxed_os_str)
",
        ),
        (
            "new",
            "
    let os_str = std::ffi::OsStr::new(s);
    unsafe { &*(std::ptr::from_ref::<std::ffi::OsStr>(os_str) as *const Self) }
",
        ),
    ];

    const MANUAL_PATH_FUNCTION_IMPLEMENTATIONS: &[(&str, &str)] = &[
        (
            "into_path_buf",
            "
    let boxed_path = unsafe {
        std::mem::transmute::<std::boxed::Box<Self>, std::boxed::Box<std::path::Path>>(self)
    };
    std::path::Path::into_path_buf(boxed_path)
",
        ),
        (
            "new",
            "
    let path = std::path::Path::new(s);
    unsafe { &*(std::ptr::from_ref::<std::path::Path>(path) as *const Self) }
",
        ),
        (
            "parent",
            r#"
    std::path::Path::parent(&self.inner)
        .map(|path| unsafe { &*(std::ptr::from_ref::<std::path::Path>(path) as *const Self) })
        .with_context(|| crate::call_failed!(Some(&self.inner), "parent"))
"#,
        ),
        (
            "strip_prefix",
            r#"
    let base = base.as_ref();
    std::path::Path::strip_prefix(&self.inner, base)
        .map(|path| unsafe { &*(std::ptr::from_ref::<std::path::Path>(path) as *const Self) })
        .with_context(|| crate::call_failed!(Some(&self.inner), "strip_prefix", base))
"#,
        ),
    ];

    MANUAL_OS_STR_FUNCTION_IMPLEMENTATIONS
        .iter()
        .map(|&(name, implementation)| {
            (
                qualified_type_function(&["std", "ffi"], "OsStr", name),
                implementation,
            )
        })
        .chain(
            MANUAL_PATH_FUNCTION_IMPLEMENTATIONS
                .iter()
                .map(|&(name, implementation)| {
                    (
                        qualified_type_function(&["std", "path"], "Path", name),
                        implementation,
                    )
                }),
        )
        .collect()
});

fn fn_def(
    function: &Function,
    qualified_trait: &[Token],
    qualified_struct: &[Token],
    fn_path: &[&str],
    fn_suffix: &[Token],
    map_kind: Option<MapKind>,
    struct_is_copy: bool,
) -> String {
    let callable_qualified_fn =
        callable_qualified_fn(qualified_trait, qualified_struct, fn_path, fn_suffix);

    for (needle, implementation) in &*MANUAL_FUNCTION_IMPLEMENTATIONS {
        if callable_qualified_fn == *needle {
            return format!(
                "{}fn {} {{{implementation}}}",
                if function.header.is_unsafe {
                    "unsafe "
                } else {
                    ""
                },
                fn_suffix.to_string()
            );
        }
    }

    let (call, call_failed) = call_and_call_failed(
        function,
        qualified_trait,
        qualified_struct,
        &callable_qualified_fn,
        struct_is_copy,
        fn_suffix.output_contains_ref(),
    );

    let mut redeclarations = Vec::new();
    for (i, (input_name, _)) in function.sig.inputs.iter().enumerate() {
        if let Some(method_name) = function.input_is_redeclarable(i) {
            redeclarations.push(format!(
                "    let {input_name} = {input_name}.{method_name}();\n"
            ));
        }
    }

    let instrumentation = match map_kind {
        Some(MapKind::Value { pair }) => format!(
            "
        .map({})
        .with_context(|| {call_failed})",
            if pair {
                "|(x, y)| (x.into(), y.into())"
            } else {
                "Into::into"
            }
        ),
        Some(MapKind::Err) => String::from(
            "
        .map_err(Into::into)",
        ),
        None => String::new(),
    };

    let output_adjustment = fn_suffix.required_output_adjustment();

    format!(
        "\
{}fn {} {{
{}
    {call}{instrumentation}{output_adjustment}
}}",
        if function.header.is_unsafe {
            "unsafe "
        } else {
            ""
        },
        fn_suffix.to_string(),
        redeclarations.join(""),
    )
}

fn call_and_call_failed(
    function: &Function,
    qualified_trait: &[Token],
    qualified_struct: &[Token],
    callable_qualified_fn: &[Token],
    struct_is_copy: bool,
    output_contains_ref: bool,
) -> (String, String) {
    let simplified_self = function
        .sig
        .inputs
        .first()
        .and_then(|(name, ty)| simplified_self(name, ty))
        .map(|tokens| {
            // smoelius: Because the tokens come directly from `public-api`, it is safe to use
            // `to_string_unchecked` here.
            tokens.to_string_unchecked()
        });

    let self_inner = simplified_self.clone().map(|s| {
        if qualified_struct.is_empty() {
            String::from("self")
        } else {
            s + ".inner"
        }
    });

    let mut call = format!("{}(", callable_qualified_fn.to_string());

    let mut call_failed = String::from("crate::call_failed!(");
    if let Some(self_inner) = &self_inner {
        if (simplified_self.as_deref() == Some("self")
            && (!qualified_trait.is_empty() || !struct_is_copy))
            || (simplified_self.as_deref() == Some("&mut self") && output_contains_ref)
        {
            call_failed.push_str(&format!(
                r#"Some(crate::CustomDebugMessage("value of type {}"))"#,
                self_type_name(qualified_trait, qualified_struct)
            ));
        } else {
            call_failed.push_str(&format!("Some({self_inner})"));
        }
        call_failed.push_str(&format!(
            r#", "{}""#,
            callable_qualified_fn.last().unwrap().text()
        ));
    } else {
        call_failed.push_str(&format!(
            r#"None::<()>, "{}""#,
            callable_qualified_fn.to_string()
        ));
    }

    for (i, (input_name, _)) in function.sig.inputs.iter().enumerate() {
        if let Some(self_inner) = (i == 0).then_some(()).and(self_inner.as_deref()) {
            call.push_str(self_inner);

            // smoelius: `call_failed` was handled above, outside of the loop.
            continue;
        }

        if i != 0 {
            call.push_str(", ");
        }

        call.push_str(input_name);
        if function.input_requires_clone(i) {
            call.push_str(".clone()");
        }

        call_failed.push_str(", ");
        if function.input_is_redeclarable(i).is_some() {
            call_failed.push_str(input_name);
        } else if let Some(trait_name) = function.input_is_uncloneble(i) {
            call_failed.push_str(&format!(
                r#"crate::CustomDebugMessage("value of type impl {trait_name}")"#
            ));
        } else {
            call_failed.push_str(input_name);
        }
    }

    call.push(')');
    call_failed.push(')');

    (call, call_failed)
}

fn self_type_name(qualified_trait: &[Token], qualified_struct: &[Token]) -> String {
    assert!(qualified_trait.is_empty() || qualified_struct.is_empty());
    // smoelius: These strings are used only for debug messages.
    if qualified_trait.is_empty() {
        qualified_struct.to_string_unchecked()
    } else {
        format!("impl {}", qualified_trait.to_string_unchecked())
    }
}

fn callable_qualified_fn(
    qualified_trait: &[Token],
    qualified_struct: &[Token],
    fn_path: &[&str],
    fn_suffix: &[Token],
) -> Vec<Token> {
    assert!(!fn_suffix.is_empty());
    assert!(matches!(fn_suffix[0], Token::Function(_)));
    let fn_name = fn_suffix[0].clone();
    let tokens = if !qualified_trait.is_empty() {
        [
            &[
                Token::symbol("<"),
                Token::generic("Self"),
                Token::keyword("as"),
                Token::symbol("::"),
            ],
            qualified_trait,
            &[Token::symbol(">"), Token::symbol("::"), fn_name],
        ]
        .concat()
    } else if !qualified_struct.is_empty() {
        [qualified_struct, &[Token::symbol("::"), fn_name]].concat()
    } else {
        [path_prefix(fn_path).as_slice(), &[fn_name]].concat()
    };
    tokens.turbofish()
}

fn disallowable_qualified_fn(
    qualified_trait: &[Token],
    qualified_struct: &[Token],
    fn_path: &[&str],
    fn_suffix: &[Token],
) -> Vec<Token> {
    assert!(!fn_suffix.is_empty());
    assert!(matches!(fn_suffix[0], Token::Function(_)));
    let fn_name = fn_suffix[0].clone();
    if !qualified_trait.is_empty() || !qualified_struct.is_empty() {
        [
            qualified_trait,
            qualified_struct,
            &[Token::symbol("::"), fn_name],
        ]
        .concat()
    } else {
        [path_prefix(fn_path).as_slice(), &[fn_name]].concat()
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use similar_asserts::SimpleDiff;
    use std::{
        env::var,
        fs::{exists, read_to_string, write},
        path::Path,
        process::Command,
        str::FromStr,
    };

    const RUST_URL: &str = "https://github.com/rust-lang/rust";
    const RUST_DIR: &str = "checkouts/rust";
    const SUBMODULES: &[&str] = &["library/backtrace", "library/stdarch"];

    #[test]
    fn generated_is_current() {
        if repo_is_dirty() {
            #[allow(clippy::explicit_write)]
            writeln!(
                std::io::stderr(),
                "Skipping `generated_is_current` test as repository is dirty",
            )
            .unwrap();
            return;
        }
        assert_cmd::Command::cargo_bin("generate")
            .unwrap()
            .assert()
            .success();
        assert!(!repo_is_dirty());
    }

    #[test]
    fn version_commit() {
        let short_commit = &COMMIT[..9];
        let pat = format!("({short_commit} ");
        let output = Command::new("rustc")
            .arg("--version")
            .env("RUSTUP_TOOLCHAIN", TOOLCHAIN)
            .output()
            .unwrap();
        let stdout = std::str::from_utf8(&output.stdout).unwrap();
        assert!(stdout.contains(&pat));
    }

    #[test]
    fn std_json() {
        clone_or_update_rust();

        let status = Command::new("git")
            .args(["checkout", COMMIT])
            .current_dir(RUST_DIR)
            .status()
            .unwrap();
        assert!(status.success());

        for submodule in SUBMODULES {
            let status = Command::new("git")
                .args(["submodule", "update", "--init", submodule])
                .current_dir(RUST_DIR)
                .status()
                .unwrap();
            assert!(status.success());
        }

        // smoelius:
        // https://github.com/cargo-public-api/cargo-public-api/blob/4b0482ef18ef564f088855320871fcedd159e384/public-api/src/lib.rs#L15-L17
        // ```sh
        // cargo +nightly rustdoc -- -Z unstable-options --output-format json
        // ```
        let status = Command::new("cargo")
            .args([
                &format!("+{TOOLCHAIN}"),
                "rustdoc",
                "--",
                "-Z",
                "unstable-options",
                "--output-format=json",
            ])
            .current_dir(Path::new(RUST_DIR).join("library/std"))
            .status()
            .unwrap();
        assert!(status.success());

        let path = Path::new(RUST_DIR).join("library/target/doc/std.json");

        let json_generated = read_to_string(&path).unwrap();

        let mut value = serde_json::Value::from_str(&json_generated).unwrap();

        strip_manifest_dir_from_filenames(&mut value);

        let json_normalized = serde_json::to_string_pretty(&value).unwrap();

        if enabled("BLESS") {
            write("assets/std.json", json_normalized).unwrap();
        } else {
            let json_assets = read_to_string("assets/std.json").unwrap();

            assert!(
                json_assets == json_normalized,
                "{}",
                SimpleDiff::from_str(&json_assets, &json_normalized, "left", "right")
            );
        }
    }

    fn clone_or_update_rust() {
        if !exists(RUST_DIR).unwrap() {
            let status = Command::new("git")
                .args(["clone", RUST_URL, RUST_DIR])
                .status()
                .unwrap();
            assert!(status.success());
            return;
        }

        let status = Command::new("git")
            .args(["checkout", "master"])
            .current_dir(RUST_DIR)
            .status()
            .unwrap();
        assert!(status.success());

        let status = Command::new("git")
            .args(["pull"])
            .current_dir(RUST_DIR)
            .status()
            .unwrap();
        assert!(status.success());
    }

    fn strip_manifest_dir_from_filenames(value: &mut serde_json::Value) {
        match value {
            serde_json::Value::Null
            | serde_json::Value::Bool(_)
            | serde_json::Value::Number(_)
            | serde_json::Value::String(_) => {}
            serde_json::Value::Array(array) => {
                for value in array {
                    strip_manifest_dir_from_filenames(value);
                }
            }
            serde_json::Value::Object(object) => {
                object.retain(|key, value| {
                    #[cfg_attr(dylint_lib = "general", allow(abs_home_path))]
                    if key == "filename" {
                        let s = value.as_str().unwrap();
                        let path = Path::new(s);
                        *value = serde_json::Value::from(
                            path.strip_prefix(env!("CARGO_MANIFEST_DIR"))
                                .unwrap_or(path)
                                .to_string_lossy(),
                        );
                    }
                    strip_manifest_dir_from_filenames(value);
                    true
                });
            }
        }
    }

    fn repo_is_dirty() -> bool {
        let status = Command::new("git")
            .args(["diff", "--quiet"])
            .status()
            .unwrap();
        !status.success()
    }

    pub fn enabled(key: &str) -> bool {
        var(key).map_or(false, |value| value != "0")
    }
}
