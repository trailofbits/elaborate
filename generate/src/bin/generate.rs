use anyhow::Result;
use once_cell::sync::Lazy;
use std::{
    fs::remove_dir_all,
    path::{Path, PathBuf},
};

#[cfg_attr(dylint_lib = "general", allow(abs_home_path))]
static ROOT: Lazy<PathBuf> =
    Lazy::new(|| Path::new(env!("CARGO_MANIFEST_DIR")).join("../elaborate/src/generated"));

fn main() -> Result<()> {
    remove_dir_all(&*ROOT).unwrap_or_default();

    generate::generate(&*ROOT)?;

    Ok(())
}
