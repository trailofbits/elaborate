#![cfg_attr(dylint_lib = "general", allow(crate_wide_allow))]
#![allow(stable_features)]
//
// Unstable features
#![cfg_attr(feature = "anonymous_pipe", feature(anonymous_pipe))]
#![cfg_attr(feature = "buf_read_has_data_left", feature(buf_read_has_data_left))]
#![cfg_attr(feature = "bufread_skip_until", feature(bufread_skip_until))]
#![cfg_attr(feature = "core_io_borrowed_buf", feature(core_io_borrowed_buf))]
#![cfg_attr(feature = "exit_status_error", feature(exit_status_error))]
#![cfg_attr(feature = "file_buffered", feature(file_buffered))]
#![cfg_attr(feature = "panic_backtrace_config", feature(panic_backtrace_config))]
#![cfg_attr(feature = "path_file_prefix", feature(path_file_prefix))]
#![cfg_attr(feature = "raw_os_error_ty", feature(raw_os_error_ty))]
#![cfg_attr(feature = "read_buf", feature(read_buf))]
#![cfg_attr(feature = "seek_stream_len", feature(seek_stream_len))]
#![cfg_attr(feature = "tcp_linger", feature(tcp_linger))]
#![cfg_attr(feature = "thread_spawn_unchecked", feature(thread_spawn_unchecked))]
#![cfg_attr(feature = "write_all_vectored", feature(write_all_vectored))]
//
// Linux-specific unstable features
#![cfg_attr(
    all(target_os = "linux", feature = "linux_pidfd"),
    feature(linux_pidfd)
)]
#![cfg_attr(
    all(target_os = "linux", feature = "tcp_deferaccept"),
    feature(tcp_deferaccept)
)]
#![cfg_attr(
    all(target_os = "linux", feature = "tcp_quickack"),
    feature(tcp_quickack)
)]
#![cfg_attr(
    all(target_os = "linux", feature = "unix_set_mark"),
    feature(unix_set_mark)
)]
#![cfg_attr(
    all(target_os = "linux", feature = "unix_socket_ancillary_data"),
    feature(unix_socket_ancillary_data)
)]
//
// Unix-specific unstable features
#![cfg_attr(
    all(unix, feature = "peer_credentials_unix_socket"),
    feature(peer_credentials_unix_socket)
)]
#![cfg_attr(
    all(unix, feature = "unix_file_vectored_at"),
    feature(unix_file_vectored_at)
)]
#![cfg_attr(all(unix, feature = "unix_socket_peek"), feature(unix_socket_peek))]
//
// Windows-specific unstable features
#![cfg_attr(all(windows, feature = "junction_point"), feature(junction_point))]
#![cfg_attr(
    all(windows, feature = "windows_by_handle"),
    feature(windows_by_handle)
)]
#![cfg_attr(
    all(windows, feature = "windows_change_time"),
    feature(windows_change_time)
)]
//
// WASI-specific unstable features
#![cfg_attr(all(target_os = "wasi", feature = "wasi_ext"), feature(wasi_ext))]

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
