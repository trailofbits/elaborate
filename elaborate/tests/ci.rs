use assert_cmd::Command;
use once_cell::sync::Lazy;
use regex::Regex;
use std::{
    collections::BTreeSet,
    env::{remove_var, set_current_dir},
    fs::read_to_string,
    path::Path,
};
use walkdir::WalkDir;

#[ctor::ctor]
fn initialize() {
    remove_var("CARGO_TERM_COLOR");
    set_current_dir("..").unwrap();
}

#[test]
fn check_all_features() {
    Command::new("cargo")
        .args(["+nightly", "check", "--all-features"])
        .env("RUSTFLAGS", "--deny=warnings")
        .assert()
        .success();
}

#[test]
fn clippy() {
    Command::new("cargo")
        // smoelius: Remove `CARGO` environment variable to work around:
        // https://github.com/rust-lang/rust/pull/131729
        .env_remove("CARGO")
        .args([
            "+nightly",
            "clippy",
            "--all-targets",
            "--",
            "--deny=warnings",
        ])
        .assert()
        .success();
}

#[test]
fn clippy_toml() {
    let output = Command::new("cargo")
        .args(["clippy", "--quiet"])
        .env("CLIPPY_CONF_DIR", "../../clippy_conf")
        .current_dir("fixtures/create_dir")
        .output()
        .unwrap();
    assert!(output.status.success());
    let stderr = String::from_utf8(output.stderr).unwrap();
    assert_eq!("\
warning: use of a disallowed method `std::fs::create_dir`
 --> src/main.rs:4:5
  |
4 |     create_dir(\"/dir\")?;
  |     ^^^^^^^^^^
  |
  = note: use `elaborate::std::fs::create_dir_wc`
  = help: for further information visit https://rust-lang.github.io/rust-clippy/master/index.html#disallowed_methods
  = note: `#[warn(clippy::disallowed_methods)]` on by default

", stderr);
}

#[test]
fn dylint() {
    Command::new("cargo")
        .args(["dylint", "--all", "--", "--all-targets"])
        .env("DYLINT_RUSTFLAGS", "--deny warnings")
        .assert()
        .success();
}

#[test]
fn features_are_used() {
    let contents =
        read_to_string(Path::new(env!("CARGO_MANIFEST_DIR")).join("Cargo.toml")).unwrap();
    let table = contents.parse::<toml::Table>().unwrap();
    let features_table = table
        .get("features")
        .and_then(toml::Value::as_table)
        .unwrap();

    let features_used = collect_features_used();

    // smoelius: Rustc already checks that all used features are known, and warns otherwise. So we
    // just need to check that all known features are used. The `default` feature and features
    // beginning with `__` are excluded from this check.
    for feature in features_table.keys() {
        if feature == "default" || feature.starts_with("__") {
            continue;
        }
        assert!(features_used.contains(feature), "`{feature}` is unused");
    }
}

static FEATURE_RE: Lazy<Regex> =
    Lazy::new(|| Regex::new(r#"#\[cfg\(feature = "([^"]*)"\)\]"#).unwrap());

fn collect_features_used() -> BTreeSet<String> {
    let mut features = BTreeSet::new();
    #[cfg_attr(dylint_lib = "general", allow(abs_home_path))]
    for result in WalkDir::new(Path::new(env!("CARGO_MANIFEST_DIR")).join("src/generated")) {
        let entry = result.unwrap();
        let path = entry.path();
        if !path.is_file() {
            continue;
        }
        let contents = read_to_string(path).unwrap();
        for captures in FEATURE_RE.captures_iter(&contents) {
            assert_eq!(2, captures.len());
            features.insert(captures.get(1).unwrap().as_str().to_owned());
        }
    }
    features
}

#[cfg(unix)]
#[test]
fn markdown_link_check() {
    let tempdir = tempfile::tempdir().unwrap();

    // smoelius: Pin `markdown-link-check` to version 3.11 until the following issue is resolved:
    // https://github.com/tcort/markdown-link-check/issues/304
    Command::new("npm")
        .args(["install", "markdown-link-check@3.11"])
        .current_dir(&tempdir)
        .assert()
        .success();

    // smoelius: https://github.com/rust-lang/crates.io/issues/788
    let config = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/markdown_link_check.json");

    let readme_md = Path::new(env!("CARGO_MANIFEST_DIR")).join("../README.md");

    Command::new("npx")
        .args([
            "markdown-link-check",
            "--config",
            &config.to_string_lossy(),
            &readme_md.to_string_lossy(),
        ])
        .current_dir(&tempdir)
        .assert()
        .success();
}

#[test]
fn readme_reference_links_are_sorted() {
    let re = Regex::new(r"^\[[^\]]*\]:").unwrap();
    let readme = read_to_string("README.md").unwrap();
    let links = readme
        .lines()
        .filter(|line| re.is_match(line))
        .collect::<Vec<_>>();
    let mut links_sorted = links.clone();
    links_sorted.sort_unstable();
    assert_eq!(links_sorted, links);
}

#[test]
fn readme_reference_links_are_used() {
    let re = Regex::new(r"(?m)^(\[[^\]]*\]):").unwrap();
    let readme = read_to_string("README.md").unwrap();
    for captures in re.captures_iter(&readme) {
        assert_eq!(2, captures.len());
        let m = captures.get(1).unwrap();
        assert!(
            readme[..m.start()].contains(m.as_str()),
            "{} is unused",
            m.as_str()
        );
    }
}
