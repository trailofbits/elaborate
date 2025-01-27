use assert_cmd::Command;

#[test]
fn hack_feature_powerset_udeps() {
    Command::new("cargo")
        .env("RUSTFLAGS", "-D warnings")
        .args(["hack", "--feature-powerset", "udeps"])
        .assert()
        .success();
}
