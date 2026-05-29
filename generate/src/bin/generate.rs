//! To update the source code in elaborate/src/generated, run `cargo run` in this
//! directory.
//!
//! Note that the update will be performed using assets/std.json, not a checkout of the Rust
//! repository.
//!
//! To update assets/std.json, run `BLESS=1 cargo test std_json` in this directory.

use anyhow::Result;
use std::{
    fs::{copy, create_dir_all, read_dir, remove_dir_all},
    path::{Path, PathBuf},
    sync::LazyLock,
};

#[cfg_attr(dylint_lib = "general", allow(abs_home_path))]
static ROOT: LazyLock<PathBuf> =
    LazyLock::new(|| Path::new(env!("CARGO_MANIFEST_DIR")).join("../elaborate/src/generated"));

#[cfg_attr(dylint_lib = "general", allow(abs_home_path))]
static CLIPPY_TOML: LazyLock<PathBuf> = LazyLock::new(|| {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("../elaborate/clippy_conf/clippy.toml")
});

#[cfg_attr(dylint_lib = "general", allow(abs_home_path))]
static DEBUG_OUTPUT: LazyLock<PathBuf> =
    LazyLock::new(|| Path::new(env!("CARGO_MANIFEST_DIR")).join("debug_output"));

fn main() -> Result<()> {
    let output = generate::generate()?;

    remove_dir_all(&*ROOT).unwrap_or_default();
    copy_dir_all(&output.generated_root, &ROOT)?;

    copy(&output.clippy_toml, &*CLIPPY_TOML)?;

    remove_dir_all(&*DEBUG_OUTPUT).unwrap_or_default();
    copy_dir_all(&output.debug_output, &DEBUG_OUTPUT)?;

    Ok(())
}

fn copy_dir_all(from: &Path, to: &Path) -> Result<()> {
    create_dir_all(to)?;
    for entry in read_dir(from)? {
        let entry = entry?;
        let from = entry.path();
        let to = to.join(entry.file_name());
        let file_type = entry.file_type()?;
        if file_type.is_dir() {
            copy_dir_all(&from, &to)?;
        } else {
            copy(from, to)?;
        }
    }
    Ok(())
}
