use std::process::Command;

#[test]
fn hack_feature_powerset_udeps() {
    let status = Command::new("cargo")
        .env("RUSTFLAGS", "-D warnings")
        .args(["hack", "--feature-powerset", "udeps", "--all-targets"])
        .status()
        .unwrap();
    assert!(status.success());
}
