# Elaborate

Wrappers for standard library functions and types to produce more elaborate error messages

## Example

Error message produced by `create_dir_wc`, Elaborate's wrapper for `create_dir`:

```
Error: call failed:
    std::fs::create_dir(
        "/dir",
    )

Caused by:
    Permission denied (os error 13)
```

Compare this to `create_dir`'s error message, which does not mention the call that failed or the path involved:

```
Error: Permission denied (os error 13)
```

<details>

<summary>Rust programs used to produce the above (note that <tt>wc</tt> is mnemonic for "with context")</summary>

```rust
use elaborate::std::fs::create_dir_wc;

fn main() -> anyhow::Result<()> {
    create_dir_wc("/dir")?;
    Ok(())
}
```

```rust
use std::fs::create_dir;

fn main() -> anyhow::Result<()> {
    create_dir("/dir")?;
    Ok(())
}
```

</details>

## Traits and structs

Elaborate provides wrapper traits for standard library traits and structs. The name of each wrapper trait is the name of the wrapped trait or struct with `Context` appended. Like for normal functions, the name of each wrapper trait function is the name of the wrapped function with `_wc` appended.

The following example uses the wrapped versions of the `Write` trait and the `OpenOptions` struct.

```rust
use elaborate::std::{fs::OpenOptionsContext, io::WriteContext};
use std::fs::OpenOptions;

fn main() -> anyhow::Result<()> {
    let mut file = OpenOptions::new()
        .create(true)
        .write(true)
        .open_wc("greeting.txt")?;
    file.write_all_wc(b"Hello, world!")?;
    Ok(())
}
```

Running the above example in a read-only directory produces the following error message:

```
Error: call failed:
    OpenOptions(
        OpenOptions {
            read: false,
            write: true,
            append: false,
            truncate: false,
            create: true,
            create_new: false,
            custom_flags: 0,
            mode: 0o000666,
        },
    ).open(
        "greeting.txt",
    )

Caused by:
    Permission denied (os error 13)
```

## Clippy

This repository provides a [Clippy configuration] (`clippy.toml`) file to identify functions that could be replaced with wrapped ones. To use the file, clone this repository and run Clippy with the following command:

```sh
CLIPPY_CONF_DIR=path-to-elaborate-repo/clippy_conf cargo clippy
```

Note that `CLIPPY_CONF_DIR` names the directory containing the `clippy.toml` file, not the `clippy.toml` file itself.

When running the above command, you should see warnings like the following:

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

## Semantic versioning policy

We reserve the right to update the nightly toolchain with which `elaborate` is associated, and to release such changes with only a minor version bump.

Such a policy should not affect users of stable Rust.

However, if you are using nightly Rust and require a specific nightly toolchain, then we recommend using a [tilde requirement] to specify `elaborate` as a dependency.

Example:

```toml
[dependencies]
elaborate = "~1.0"
```

## Credits

Elaborate uses [`public-api`] and [`rustdoc-types`] to generate wrappers, and [`anyhow`] to generate error messages.

[Clippy configuration]: https://doc.rust-lang.org/clippy/configuration.html
[`anyhow::Context::with_context`]: https://docs.rs/anyhow/latest/anyhow/trait.Context.html#tymethod.with_context
[`anyhow`]: https://github.com/dtolnay/anyhow
[`public-api`]: https://github.com/cargo-public-api/cargo-public-api/tree/main/public-api
[`rustdoc-types`]: https://github.com/aDotInTheVoid/rustdoc-types
[`std::fs::File`]: https://doc.rust-lang.org/std/fs/struct.File.html
[tilde requirement]: https://doc.rust-lang.org/cargo/reference/specifying-dependencies.html#tilde-requirements
