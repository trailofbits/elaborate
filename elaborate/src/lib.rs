use ::std::{any::type_name, fmt::Debug};

#[expect(deprecated, clippy::module_name_repetitions)]
mod generated;
pub use generated::std;

#[macro_export]
macro_rules! rewrite_output_type {
    ( $_0:ident $(:: $_1:ident)* < $ty:ty $(, $_2:ty)? $(,)? > ) => {
        anyhow::Result< $ty >
    };
}

#[macro_export]
macro_rules! call_failed {
    ($this:expr, $name:literal $(,)?) => {{
        let mut s = $crate::__call_failed_common!($this, $name);
        s.push(')');
        s
    }};
    ($this:expr, $name:literal, $($args:expr),+ $(,)?) => {{
        let mut s = $crate::__call_failed_common!($this, $name);
        $crate::__call_failed_args!(s, $($args),*);
        s.push_str("\n    )");
        s
    }};
}

/// Pushes:
/// - "call failed:"
/// - newline
/// - one of:
///     - indented `self` argument and period (`.`)
///     - indentation
/// - function name
/// - left paren (`(`)
#[macro_export]
macro_rules! __call_failed_common {
    ($this:expr, $name:literal) => {{
        #[allow(unused_imports)]
        use $crate::MaybeDebugFallback;
        let mut s = String::from("call failed:\n");
        if let Some(this) = $this {
            s.push_str(&$crate::indent(
                4,
                &$crate::MaybeDebug(this).to_debug_string(),
            ));
            s.push('.');
        } else {
            s.push_str("    ");
        }
        s.push_str($name);
        s.push('(');
        s
    }};
}

#[macro_export]
macro_rules! __call_failed_args {
    // Base case:
    ($s:expr, $arg:expr) => {
        $crate::__call_failed_args_common!($s, $arg);
    };
    // Inductive case:
    ($s:expr, $arg:expr, $($args:expr),*) => {
        $crate::__call_failed_args_common!($s, $arg);
        $crate::__call_failed_args!($s, $($args),*);
    };
}

/// Pushes newline, argument, and trailing comma.
#[macro_export]
macro_rules! __call_failed_args_common {
    ($s:expr, $arg:expr) => {{
        #[allow(unused_imports)]
        use $crate::MaybeDebugFallback;
        $s.push('\n');
        $s.push_str(&$crate::indent(
            8,
            &$crate::MaybeDebug($arg).to_debug_string(),
        ));
        $s.push(',');
    }};
}

fn indent(width: usize, s: &str) -> String {
    const INDENTATION: &str = "        ";
    assert!(width <= INDENTATION.len());
    let mut buf = String::new();
    for line in s.split_inclusive('\n') {
        buf.push_str(&INDENTATION[..width]);
        buf.push_str(line);
    }
    buf
}

// smoelius: `MaybeDebug` uses Nikolai Vazquez's trick from `impls`.
// https://github.com/nvzqz/impls#how-it-works

pub struct MaybeDebug<T>(pub T);

impl<T> MaybeDebug<T> {
    pub fn new(value: T) -> Self {
        Self(value)
    }
}

impl<T> MaybeDebug<T>
where
    T: Debug,
{
    /// If `expr: MaybeDebug<T>` and `T: Debug`, then `expr.to_debug_string()` resolves to this
    /// inherent method.
    pub fn to_debug_string(&self) -> String {
        format!("{:#?}", self.0)
    }
}

pub trait MaybeDebugFallback {
    /// If `expr: MaybeDebug<T>` but not `T: Debug`, then `expr.to_debug_string()` resolves to this
    /// trait method.
    fn to_debug_string(&self) -> String;
}

impl<T> MaybeDebugFallback for T {
    fn to_debug_string(&self) -> String {
        const PAT: &str = "MaybeDebug<";
        let type_name = type_name::<T>();
        let pos = type_name.find(PAT).unwrap() + PAT.len();
        let generic_arg = type_name[pos..].strip_suffix('>').unwrap();
        format!("<value of type {generic_arg}>")
    }
}

struct CustomDebugMessage(&'static str);

impl Debug for CustomDebugMessage {
    fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
        let msg = self.0;
        write!(f, "<{msg}>")
    }
}
