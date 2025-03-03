use anyhow::Result;
use std::{
    fs::remove_dir_all,
    path::{Path, PathBuf},
    sync::LazyLock,
};

#[cfg_attr(dylint_lib = "general", allow(abs_home_path))]
static ROOT: LazyLock<PathBuf> =
    LazyLock::new(|| Path::new(env!("CARGO_MANIFEST_DIR")).join("../elaborate/src/generated"));

fn main() -> Result<()> {
    remove_dir_all(&*ROOT).unwrap_or_default();

    generate::generate(&*ROOT)?;

    Ok(())
}
