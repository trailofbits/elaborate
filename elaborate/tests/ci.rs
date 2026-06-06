use assert_cmd::Command;
use std::path::Path;

#[test]
fn ci() {
    let workspace_root = Path::new(env!("CARGO_MANIFEST_DIR")).parent().unwrap();

    Command::new("cargo")
        .current_dir(workspace_root)
        .args(["run", "--manifest-path", "ci/Cargo.toml"])
        .assert()
        .success();
}
