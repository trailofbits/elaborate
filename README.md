# elaborate

Wrappers for standard library functions and types to produce more elaborate error messages

## Example

```rust
// Use `elaborate`'s wrapped version of `std::fs::create_dir`.
// `wc` is mnemonic for "with context".
use elaborate::std::fs::create_dir_wc;

fn main() -> anyhow::Result<()> {
    create_dir_wc("/dir")?;
    Ok(())
}
```

Error message:

```
Error: call failed:
    std::fs::create_dir(
        "/dir",
    )

Caused by:
    Permission denied (os error 13)
```

Compare this to the standard error message, which does not include the call that failed or the path involved:

```
Os { code: 13, kind: PermissionDenied, message: "Permission denied" }
```

## Clippy

This repository provides a [Clippy configuration file] to identify functions that could be replaced with wrapped ones. To use the file, clone this repository and run Clippy with the following command:

```sh
CLIPPY_CONF_DIR=path-to-elaborate-repo/clippy_conf cargo clippy
```

You should see warnings similar to the following:

```
warning: use of a disallowed method `std::fs::create_dir`
 --> src/main.rs:4:5
  |
4 |     create_dir(\"/dir\")?;
  |     ^^^^^^^^^^
  |
  = note: use `elaborate::std::fs::create_dir_wc`
  = help: for further information visit https://rust-lang.github.io/rust-clippy/master/index.html#disallowed_methods
  = note: `#[warn(clippy::disallowed_methods)]` on by default
```

## Alternative approaches considered

**Wrapper structs**, e.g., a struct `File` that wraps a [`std::fs::File`], so that calling a method on the wrapper struct calls the underlying method with [`anyhow::Context::with_context`]. This idea works to a degree but has several problems. Most significantly, the wrapper struct must implement every trait the wrapped struct does. There are many ways a wrapped struct could implement a trait. For example, if a trait provides a default implementation, the wrapped struct could use the default implementation or provide its own. Such facts complicate automatic code generation. Hence, this idea seems untenable.

## Credits

`elaborate` uses [`public-api`] and [`rustdoc-types`] to generate wrappers, and [`anyhow`] to generate error messages.

[Clippy configuration file]: https://doc.rust-lang.org/clippy/configuration.html
[`anyhow::Context::with_context`]: https://docs.rs/anyhow/latest/anyhow/trait.Context.html#tymethod.with_context
[`anyhow`]: https://github.com/dtolnay/anyhow
[`public-api`]: https://github.com/cargo-public-api/cargo-public-api/tree/main/public-api
[`rustdoc-types`]: https://github.com/aDotInTheVoid/rustdoc-types
[`std::fs::File`]: https://doc.rust-lang.org/std/fs/struct.File.html
