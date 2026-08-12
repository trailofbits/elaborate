use std::{path::Path, process::Command};

#[test]
fn ci() {
    let workspace_root = Path::new(env!("CARGO_MANIFEST_DIR")).parent().unwrap();

    let status = Command::new("cargo")
        .current_dir(workspace_root)
        .args(["run", "--manifest-path", "ci/Cargo.toml"])
        .status()
        .unwrap();
    assert!(status.success());
}
