use anyhow::Result;
use if_chain::if_chain;
use once_cell::sync::Lazy;
use public_api::tokens::Token;
use regex::Regex;
use rustdoc_types::{Crate, Function, Id, ItemEnum, Type};
use std::{
    collections::{BTreeMap, HashSet},
    fs::{create_dir_all, File, OpenOptions},
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

pub const TOOLCHAIN: &str = "nightly-2024-10-22";
pub const COMMIT: &str = "4392847410ddd67f6734dd9845f9742ff9e85c83";

#[cfg_attr(dylint_lib = "general", allow(abs_home_path))]
static STD_JSON: Lazy<PathBuf> =
    Lazy::new(|| Path::new(env!("CARGO_MANIFEST_DIR")).join("../assets/std.json"));

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
        (&["std", "io"], "BufWriter"),
        (&["std", "io"], "LineWriter"),
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

static REWRITABLE_PATHS: Lazy<Vec<(Vec<Token>, Vec<Token>)>> = Lazy::new(|| {
    const SHORTENABLE_PATHS: &[&[&str]] = &[
        &["core", "io", "borrowed_buf"],
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
    let mut generator = Generator::default();

    generator.import_path(&STD_JSON)?;

    generator.write_clippy_toml()?;

    generator.write_discarded()?;

    generator.generate(root)?;

    Ok(())
}

#[derive(Default)]
struct Generator {
    module: Module,
    disallowed: BTreeMap<Vec<Token>, Vec<Token>>,
    discarded: BTreeMap<&'static str, Vec<Vec<Token>>>,
}

impl Generator {
    fn import_path(&mut self, path: &Path) -> Result<()> {
        let file = File::open(path)?;
        let krate = serde_json::from_reader::<_, Crate>(file)?;

        let public_api = public_api::Builder::from_rustdoc_json(path).build()?;

        self.import(&krate, public_api);

        Ok(())
    }

    fn import(&mut self, krate: &Crate, public_api: public_api::PublicApi) {
        let mut public_item_map = PublicItemMap::default();

        public_item_map.populate_from_public_api(public_api, |tokens| {
            if IGNORED_PATHS
                .iter()
                .any(|path| tokens.position(path).is_some())
            {
                self.discarded
                    .entry("ignored_path")
                    .or_default()
                    .push(tokens.to_vec());
                return true;
            }
            false
        });

        let parents_of_wrappable_functions =
            Self::parents_of_wrappable_functions(krate, &public_item_map);

        for (&id, public_items) in public_item_map.iter() {
            let Some((function, true, _) | (function, _, true)) = Self::is_function(krate, id)
            else {
                continue;
            };

            // smoelius: An `Id` may correspond to multiple `PublicItem`s. If _any_ of the
            // `PublicItem`s' parents are in `parents_of_wrappable_functions`, proceed.
            if !public_items
                .iter()
                .filter_map(|&(parent_id, _)| parent_id)
                .any(|parent_id| parents_of_wrappable_functions.contains(&parent_id))
            {
                continue;
            };

            let docs = krate.index.get(&id).and_then(|item| item.docs.as_deref());

            self.import_function(krate, &public_item_map, id, docs, function);
        }
    }

    #[allow(clippy::too_many_lines)]
    fn import_function(
        &mut self,
        krate: &Crate,
        public_item_map: &PublicItemMap,
        id: Id,
        docs: Option<&str>,
        function: &Function,
    ) {
        let parent_id = public_item_map.parent_id(id).unwrap();

        let parent_item = krate.index.get(&parent_id).unwrap();

        let has_sealed_trait_bound = match &parent_item.inner {
            ItemEnum::Trait(trait_) => trait_.bounds.has_trait_bound_with_name("Sealed"),
            ItemEnum::Impl(impl_) => {
                // smoelius: Ignore impls for primitive types.
                if matches!(impl_.for_, Type::Primitive(_)) {
                    return;
                }
                // smoelius: Ignore `impl Trait for Struct` constructs.
                if impl_.trait_.is_some() {
                    return;
                }
                false
            }
            ItemEnum::Module(_) => false,
            _ => panic!(),
        };

        // smoelius: Fetch the parent's tokens first so that if they contain a
        // `qualified_struct`, that `qualified_struct` can be passed to `patch_tokens`.
        let (parent_tokens, false) = patch_tokens(
            public_item_map.tokens(parent_id),
            &[],
            has_sealed_trait_bound,
            false,
        ) else {
            panic!();
        };

        let qualified_trait = parent_tokens.extract_trait();
        let qualified_struct = parent_tokens.extract_struct();

        // smoelius: Structs with generic params are currently unsupported.
        let (generic_params, qualified_struct) = qualified_struct.extract_initial_generic_params();
        if !generic_params.is_empty() {
            assert!(
                GENERIC_STRUCTS
                    .iter()
                    .any(|path| qualified_struct.starts_with(path)),
                "{qualified_struct:?}"
            );
            return;
        }

        let (tokens, false) =
            patch_tokens(public_item_map.tokens(id), qualified_struct, false, true)
        else {
            return;
        };

        // smoelius: Fetch the parent's attributes first. If the function does not belong to a
        // trait or struct, they will append to the function's attributes so that they are not
        // lost.
        let parent_attrs =
            Self::item_attrs(krate, parent_id, Some(public_item_map), &parent_tokens);

        let attrs = match parent_item.inner {
            ItemEnum::Trait(_) => {
                assert!(!qualified_trait.is_empty());
                assert!(qualified_struct.is_empty());
                let (trait_path, trait_tokens) = qualified_trait.extract_initial_path();
                let trait_wrapper_tokens = type_wrapper_tokens(trait_tokens);
                self.module.add_trait_wrapper(
                    &trait_path,
                    &parent_attrs,
                    &[path_prefix(&trait_path), trait_wrapper_tokens.clone()].concat(),
                    qualified_trait,
                );
                Self::item_attrs(krate, id, None, &tokens)
            }
            ItemEnum::Impl(_) => {
                assert!(qualified_trait.is_empty());
                assert!(!qualified_struct.is_empty());
                let (struct_path, struct_tokens) = qualified_struct.extract_initial_path();
                let struct_wrapper_tokens = type_wrapper_tokens(struct_tokens);
                self.module.add_struct_wrapper(
                    &struct_path,
                    &parent_attrs,
                    &[path_prefix(&struct_path), struct_wrapper_tokens.clone()].concat(),
                    qualified_struct,
                    tokens.output_contains_non_ref_self(),
                    // smoelius: `BufWriter` requires that its argument implement `Write`:
                    // https://doc.rust-lang.org/nightly/std/fs/struct.File.html#method.create_buffered
                    struct_tokens == [Token::type_("File")],
                );
                Self::item_attrs(krate, id, None, &tokens)
            }
            ItemEnum::Module(_) => {
                assert!(qualified_trait.is_empty());
                assert!(qualified_struct.is_empty());
                let mut attrs = Self::item_attrs(krate, id, None, &tokens);
                attrs.extend(parent_attrs);
                attrs
            }
            _ => panic!(),
        };

        // smoelius: At most one of `qualified_trait` and `qualified_struct` is non-empty.
        // Concatenating them has the effect of select the non-empty one.
        let qualified_type = [qualified_trait, qualified_struct].concat();

        let qualified_type_wrapper = qualified_type_wrapper_tokens(&qualified_type);

        let fn_tokens = tokens.strip_leading_qualifiers_and_fn();

        let (fn_path, fn_tokens) = fn_tokens.extract_initial_path();

        let (_, fn_tokens) = fn_tokens.extract_initial_type();

        let fn_wrapper_tokens = fn_wrapper_tokens(fn_tokens);

        let sig = fn_sig(function, &qualified_type, &fn_wrapper_tokens);

        let body = fn_body(
            function,
            qualified_trait,
            qualified_struct,
            &fn_path,
            fn_tokens,
        );

        self.module
            .add_fn(&fn_path, &qualified_type_wrapper, docs, &attrs, &sig, &body);

        self.disallow(&qualified_type, &fn_path, fn_tokens);
    }

    pub fn generate(self, root: impl AsRef<Path>) -> Result<()> {
        self.module.write(root)
    }

    fn parents_of_wrappable_functions(
        krate: &Crate,
        public_item_map: &PublicItemMap,
    ) -> HashSet<Id> {
        let mut parent_ids = HashSet::new();
        for (&id, public_items) in public_item_map.iter() {
            for (parent_id, _) in public_items {
                let &Some(parent_id) = parent_id else {
                    continue;
                };
                if let Some((_, true, _) | (_, _, true)) = Self::is_function(krate, id) {
                    parent_ids.insert(parent_id);
                }
            }
        }
        parent_ids
    }

    fn is_function(krate: &Crate, id: Id) -> Option<(&Function, bool, bool)> {
        let function = Self::is_function_inner(krate, id)?;

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

    fn is_function_inner(krate: &Crate, id: Id) -> Option<&Function> {
        if_chain! {
            if let Some(item) = krate.index.get(&id);
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
    fn item_attrs(
        krate: &Crate,
        id: Id,
        public_item_map: Option<&PublicItemMap>,
        tokens: &[Token],
    ) -> Vec<String> {
        let mut attrs = Self::item_attrs_inner(krate, id, public_item_map);
        attrs.extend(tokens.required_gates());
        attrs
    }

    fn item_attrs_inner(
        krate: &Crate,
        id: Id,
        public_item_map: Option<&PublicItemMap>,
    ) -> Vec<String> {
        let item = krate.index.get(&id).unwrap();
        let mut attrs = rewrite_attrs(&item.attrs);
        if let Some(public_item_map) = public_item_map {
            let mut iter = public_item_map.parent_ids(id);
            if let Some(parent_id) = iter.next() {
                let mut parent_attrs =
                    Self::item_attrs_inner(krate, parent_id, Some(public_item_map));
                for parent_id in iter {
                    let other_attrs =
                        Self::item_attrs_inner(krate, parent_id, Some(public_item_map));
                    parent_attrs.retain(|x| other_attrs.iter().any(|y| x == y));
                }
                attrs.extend(parent_attrs);
            }
        }
        attrs
    }

    fn disallow(&mut self, qualified_type: &[Token], fn_path: &[&str], fn_tokens: &[Token]) {
        let (disallowable_qualified_fn, disallowable_qualified_fn_wrapper) =
            disallowable_qualified_fn(qualified_type, fn_path, fn_tokens);

        // smoelius: Disallowing `std::io::Write::write_fmt` causes Clippy to warn about `writeln!`.
        if disallowable_qualified_fn
            == qualified_type_function(&["std", "io"], "Write", "write_fmt")
        {
            return;
        }

        self.disallowed
            .insert(disallowable_qualified_fn, disallowable_qualified_fn_wrapper);
    }

    fn write_clippy_toml(&self) -> Result<()> {
        #[cfg_attr(dylint_lib = "general", allow(abs_home_path))]
        let path = Path::new::<str>(env!("CARGO_MANIFEST_DIR")).join("../clippy_conf/clippy.toml");

        let mut file = OpenOptions::new()
            .create(true)
            .truncate(true)
            .write(true)
            .open(path)?;

        writeln!(file, "disallowed-methods = [")?;
        for (disallowable_qualified_fn, disallowable_qualified_fn_wrapper) in &self.disallowed {
            writeln!(
                file,
                r#"    {{ path = "{}", reason = "use `elaborate::{}`" }},"#,
                disallowable_qualified_fn.to_string_compact().unwrap(),
                disallowable_qualified_fn_wrapper
                    .to_string_compact()
                    .unwrap()
            )?;
        }
        writeln!(file, "]")?;

        Ok(())
    }

    fn write_discarded(&self) -> Result<()> {
        #[cfg_attr(dylint_lib = "general", allow(abs_home_path))]
        let debug_output = Path::new(env!("CARGO_MANIFEST_DIR")).join("debug_output");

        create_dir_all(&debug_output).unwrap_or_default();

        for (reason, tokens) in &self.discarded {
            let mut open_options = OpenOptions::new();
            open_options.create(true).truncate(true).write(true);
            let mut file = open_options.open(debug_output.join(reason).with_extension("txt"))?;

            for tokens in tokens {
                writeln!(file, "{tokens:?}")?;
            }
        }

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
    has_output: bool,
) -> (Vec<Token>, bool) {
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

    let error_type_is_self = if has_output {
        if tokens.error_type_is_self() {
            true
        } else {
            tokens = tokens.rewrite_output_type();
            false
        }
    } else {
        false
    };

    (tokens, _) = tokens.replace(&[Token::primitive("never")], &[Token::primitive("!")]);

    (tokens, error_type_is_self)
}

fn qualified_type_wrapper_tokens(qualified_type_tokens: &[Token]) -> Vec<Token> {
    if qualified_type_tokens.is_empty() {
        return Vec::new();
    }
    let (path, type_tokens) = qualified_type_tokens.extract_initial_path();
    let type_wrapper_tokens = type_wrapper_tokens(type_tokens);
    [path_prefix(&path), type_wrapper_tokens].concat()
}

fn type_wrapper_tokens(type_tokens: &[Token]) -> Vec<Token> {
    let [Token::Type(type_name), ..] = type_tokens else {
        return Vec::new();
    };
    [
        &[Token::type_(format!("{type_name}Context"))],
        &type_tokens[1..],
    ]
    .concat()
    // smoelius: Having the call to `remove_anonymous_lifetimes` here is ugly.
    .remove_anonymous_lifetimes()
}

fn fn_wrapper_tokens(fn_tokens: &[Token]) -> Vec<Token> {
    let [Token::Function(fn_name), ..] = fn_tokens else {
        return Vec::new();
    };
    [&[Token::function(format!("{fn_name}_wc"))], &fn_tokens[1..]].concat()
}

fn fn_sig(function: &Function, qualified_type: &[Token], fn_wrapper_tokens: &[Token]) -> String {
    format!(
        "{}{}fn {}",
        if qualified_type.is_empty() {
            "pub "
        } else {
            ""
        },
        if function.header.is_unsafe {
            "unsafe "
        } else {
            ""
        },
        fn_wrapper_tokens.to_string()
    )
}

fn fn_body(
    function: &Function,
    qualified_trait: &[Token],
    qualified_struct: &[Token],
    fn_path: &[&str],
    fn_tokens: &[Token],
) -> String {
    let callable_qualified_fn =
        callable_qualified_fn(qualified_trait, qualified_struct, fn_path, fn_tokens);

    let (call, call_failed) = call_and_call_failed(
        function,
        qualified_trait,
        qualified_struct,
        &callable_qualified_fn,
        fn_tokens.output_contains_ref(),
    );

    let mut redeclarations = Vec::new();
    for (i, (input_name, _)) in function.sig.inputs.iter().enumerate() {
        if let Some(method_name) = function.input_is_redeclarable(i) {
            redeclarations.push(format!(
                "    let {input_name} = {input_name}.{method_name}();\n"
            ));
        }
    }

    format!(
        "
{}    {call}
        .with_context(|| {call_failed})
",
        redeclarations.join(""),
    )
}

// smoelius: The one case for which the `unwrap_or_else` is needed in `call_and_call_failed`.
static SOCKET_ADDR_EXT_FROM_ABSTRACT_NAME: Lazy<Vec<Token>> = Lazy::new(|| {
    vec![
        Token::symbol("<"),
        Token::generic("Self"),
        Token::keyword("as"),
        Token::symbol("::"),
        Token::identifier("std"),
        Token::symbol("::"),
        Token::identifier("os"),
        Token::symbol("::"),
        Token::identifier("linux"),
        Token::symbol("::"),
        Token::identifier("net"),
        Token::symbol("::"),
        Token::type_("SocketAddrExt"),
        Token::symbol(">"),
        Token::symbol("::"),
        Token::function("from_abstract_name"),
    ]
});

fn call_and_call_failed(
    function: &Function,
    qualified_trait: &[Token],
    qualified_struct: &[Token],
    callable_qualified_fn: &[Token],
    output_contains_ref: bool,
) -> (String, String) {
    let simplified_self = function
        .sig
        .inputs
        .first()
        .and_then(|(name, ty)| simplified_self(name, ty))
        .map(|tokens| {
            // smoelius: Because the tokens come directly from `public-api`, it is safe to use
            // `to_string_compact_unchecked` here.
            tokens.to_string_compact_unchecked()
        });

    let mut call = format!("{}(", callable_qualified_fn.to_string());

    let mut call_failed = String::from("crate::call_failed!(");
    if let Some(simplified_self) = simplified_self.as_deref() {
        if simplified_self == "self" || (simplified_self == "&mut self" && output_contains_ref) {
            call_failed.push_str(&format!(
                r#"Some(crate::CustomDebugMessage("value of type {}"))"#,
                self_type_name(qualified_trait, qualified_struct)
            ));
        } else {
            call_failed.push_str("Some(self)");
        }
        call_failed.push_str(&format!(
            r#", "{}""#,
            callable_qualified_fn.last().unwrap().text()
        ));
    } else {
        call_failed.push_str(&format!(
            r#"None::<()>, "{}""#,
            callable_qualified_fn
                .to_string_compact()
                .unwrap_or_else(|| {
                    assert_eq!(*SOCKET_ADDR_EXT_FROM_ABSTRACT_NAME, callable_qualified_fn);
                    callable_qualified_fn.to_string()
                })
        ));
    }

    for (i, (input_name, _)) in function.sig.inputs.iter().enumerate() {
        if i == 0 && simplified_self.is_some() {
            call.push_str("self");

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
        } else if let Some(ty) = function.input_is_uncloneble(i) {
            call_failed.push_str(&format!(
                r#"crate::CustomDebugMessage("value of type {ty}")"#
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
    if qualified_trait.is_empty() {
        qualified_struct.to_string_compact().unwrap()
    } else {
        format!("impl {}", qualified_trait.to_string_compact().unwrap())
    }
}

// smoelius: Keep `callable_qualified_fn` and `disallowable_qualified_fn` next to each other, since
// their implementations are similar.
fn callable_qualified_fn(
    qualified_trait: &[Token],
    qualified_struct: &[Token],
    fn_path: &[&str],
    fn_tokens: &[Token],
) -> Vec<Token> {
    assert!(!fn_tokens.is_empty());
    assert!(matches!(fn_tokens[0], Token::Function(_)));
    let fn_name = fn_tokens[0].clone();
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

// smoelius: Keep `callable_qualified_fn` and `disallowable_qualified_fn` next to each other, since
// their implementations are similar.
/// Returns both the "wrapped" and "wrapper" versions of the disallowed qualified function
fn disallowable_qualified_fn(
    qualified_type: &[Token],
    fn_path: &[&str],
    fn_tokens: &[Token],
) -> (Vec<Token>, Vec<Token>) {
    assert!(!fn_tokens.is_empty());
    assert!(matches!(fn_tokens[0], Token::Function(_)));
    let fn_name = fn_tokens[0].clone();
    let fn_wrapper_name = fn_wrapper_tokens(&[fn_name.clone()]);
    #[allow(clippy::if_not_else)]
    if !qualified_type.is_empty() {
        let qualified_type_wrapper = qualified_type_wrapper_tokens(qualified_type);
        (
            [qualified_type, &[Token::symbol("::")], &[fn_name]].concat(),
            [
                qualified_type_wrapper.as_slice(),
                &[Token::symbol("::")],
                &fn_wrapper_name,
            ]
            .concat(),
        )
    } else {
        (
            [path_prefix(fn_path).as_slice(), &[fn_name]].concat(),
            [path_prefix(fn_path).as_slice(), &fn_wrapper_name].concat(),
        )
    }
}

// smoelius: Run these tests on just Linux until the following questions are answered:
// https://github.com/rust-lang/rustdoc-types/issues/44
#[cfg(target_os = "linux")]
#[cfg(test)]
mod test {
    use once_cell::sync::Lazy;
    use similar_asserts::SimpleDiff;
    use std::{
        env::var,
        fs::{exists, read_to_string, write},
        io::Write,
        path::Path,
        process::Command,
        str::FromStr,
    };

    const RUST_URL: &str = "https://github.com/rust-lang/rust";
    const RUST_DIR: &str = "checkouts/rust";
    const SUBMODULES: &[&str] = &["library/backtrace", "library/stdarch"];
    const TARGET: &str = "x86_64-unknown-linux-gnu";

    static TOOLCHAIN: Lazy<&str> = Lazy::new(|| {
        let status = Command::new("rustup")
            .args(["toolchain", "install", super::TOOLCHAIN, "--no-self-update"])
            .status()
            .unwrap();
        assert!(status.success());
        super::TOOLCHAIN
    });

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
        let short_commit = &super::COMMIT[..9];
        let pat = format!("({short_commit} ");
        let output = Command::new("rustc")
            .arg("--version")
            .env("RUSTUP_TOOLCHAIN", *TOOLCHAIN)
            .output()
            .unwrap();
        let stdout = std::str::from_utf8(&output.stdout).unwrap();
        assert!(
            stdout.contains(&pat),
            "stdout does not contain `{pat}`: ```\n{stdout}```"
        );
    }

    #[test]
    fn target() {
        if var("CI").is_err() {
            return;
        }
        let output = Command::new("rustc").arg("-vV").output().unwrap();
        let stdout = std::str::from_utf8(&output.stdout).unwrap();
        let needle = format!("host: {TARGET}");
        assert!(
            stdout.lines().any(|line| line == needle),
            "stdout does not contain `{needle}`: ```\n{stdout}```"
        );
    }

    #[cfg_attr(dylint_lib = "general", allow(non_thread_safe_call_in_test))]
    #[test]
    fn std_json() {
        clone_or_update_rust();

        let status = Command::new("git")
            .args(["checkout", super::COMMIT])
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
                "rustdoc",
                "--target",
                TARGET,
                "--",
                "-Z",
                "unstable-options",
                "--output-format=json",
            ])
            .env("RUSTUP_TOOLCHAIN", *TOOLCHAIN)
            .current_dir(Path::new(RUST_DIR).join("library/std"))
            .status()
            .unwrap();
        assert!(status.success());

        let path = Path::new(RUST_DIR)
            .join("library/target")
            .join(TARGET)
            .join("doc/std.json");

        let json_generated = read_to_string(&path).unwrap();

        let mut value = serde_json::Value::from_str(&json_generated).unwrap();

        strip_manifest_dir_from_filenames(&mut value);

        let json_normalized = serde_json::to_string_pretty(&value).unwrap();

        if enabled("BLESS") {
            write(&*super::STD_JSON, json_normalized).unwrap();
        } else {
            let json_assets = read_to_string(&*super::STD_JSON).unwrap();

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
        var(key).is_ok_and(|value| value != "0")
    }
}
