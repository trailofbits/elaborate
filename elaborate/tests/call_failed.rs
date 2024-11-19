#![allow(clippy::disallowed_methods)]
#![cfg_attr(
    dylint_lib = "general",
    allow(crate_wide_allow, non_thread_safe_call_in_test)
)]

#[ctor::ctor]
fn initialize() {
    // smoelius: `RUST_BACKTRACE` adds to the error messages and interferes with the tests.
    std::env::remove_var("RUST_BACKTRACE");
}

#[test]
fn call_failed_without_elaborate() {
    let error = std::fs::create_dir("/dir").unwrap_err();
    assert_eq!(
        "\
Os { code: 13, kind: PermissionDenied, message: \"Permission denied\" }",
        format!("{error:?}")
    );
}

#[test]
fn call_failed_with_elaborate() {
    let error = elaborate::std::fs::create_dir_wc("/dir").unwrap_err();
    assert_eq!(
        "\
call failed:
    std::fs::create_dir(
        \"/dir\",
    )

Caused by:
    Permission denied (os error 13)",
        format!("{error:?}")
    );
}

#[test]
fn struct_call_failed_without_elaborate() {
    let error = std::fs::OpenOptions::new()
        .read(true)
        .open("/nonexistent_file")
        .unwrap_err();
    assert_eq!(
        "\
Os { code: 2, kind: NotFound, message: \"No such file or directory\" }",
        format!("{error:?}")
    );
}

#[test]
fn struct_call_failed_with_elaborate() {
    use elaborate::std::fs::OpenOptionsContext;
    let error = std::fs::OpenOptions::new()
        .read(true)
        .open_wc("/nonexistent_file")
        .unwrap_err();
    assert_eq!(
        "\
call failed:
    OpenOptions(
        OpenOptions {
            read: true,
            write: false,
            append: false,
            truncate: false,
            create: false,
            create_new: false,
            custom_flags: 0,
            mode: 0o000666,
        },
    ).open(
        \"/nonexistent_file\",
    )

Caused by:
    No such file or directory (os error 2)",
        format!("{error:?}")
    );
}

#[cfg(unix)]
mod unix {
    use std::{
        fs::{set_permissions, File, OpenOptions},
        io::Result,
        os::unix::fs::PermissionsExt,
    };
    use tempfile::NamedTempFile;

    #[test]
    fn trait_call_failed_without_elaborate() {
        use std::os::unix::fs::FileExt;
        let tempfile = readonly_tempfile().unwrap();
        let error = tempfile.write_at(&[], 1).unwrap_err();
        assert_eq!(
            "\
Os { code: 9, kind: Uncategorized, message: \"Bad file descriptor\" }",
            format!("{error:?}")
        );
    }

    #[test]
    fn trait_call_failed_with_elaborate() {
        use elaborate::std::os::unix::fs::FileExtContext;
        let tempfile = readonly_tempfile().unwrap();
        let error = tempfile.write_at_wc(&[], 0).unwrap_err();
        assert_eq!(
            "\
call failed:
    <value of type &std::fs::File>.write_at(
        [],
        0,
    )

Caused by:
    Bad file descriptor (os error 9)",
            format!("{error:?}")
        );
    }

    fn readonly_tempfile() -> Result<File> {
        let tempfile = NamedTempFile::new()?;
        let temp_path = tempfile.into_temp_path();
        set_permissions(&temp_path, PermissionsExt::from_mode(0o444))?;
        OpenOptions::new().read(true).open(&temp_path)
    }
}
