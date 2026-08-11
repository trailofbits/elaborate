pub use dependency::bar;

pub fn foo() {
    let _ = std::env::current_dir().unwrap();
}

#[cfg(test)]
mod tests {
    // smoelius: This test should flag the call to `current_dir` above, but it should not flag the
    // call to `current_dir` in `dependency::bar`.
    #[test]
    fn elaborate_disallowed_methods() {
        assert!(elaborate::disallowed_methods().status().unwrap().success());
    }
}
