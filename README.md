# elaborate

Wrappers for standard library functions and types to produce more elaborate error messages

## Example

```rust
use elaborate::std::fs::create_dir;

fn main() -> anyhow::Result<()> {
    create_dir("/dir")?;
    Ok(())
}
```

Error message

```
Error: call failed:
    std :: fs :: create_dir(
        "/dir",
    )

Caused by:
    Permission denied (os error 13)
```

Compare this to the standard error message, which does not include the call that failed or the path involved:

```
Os { code: 13, kind: PermissionDenied, message: "Permission denied" }
```

## Credits

`elaborate` uses [`public-api`] and [`rustdoc-types`] to generate wrappers, and [`anyhow`] to generate error messages.

[`anyhow`]: https://github.com/dtolnay/anyhow
[`public-api`]: https://github.com/cargo-public-api/cargo-public-api/tree/main/public-api
[`rustdoc-types`]: https://github.com/aDotInTheVoid/rustdoc-types
