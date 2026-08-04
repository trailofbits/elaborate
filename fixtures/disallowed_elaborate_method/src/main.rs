use elaborate::std::path::PathContext;
use std::path::Path;

fn main() {
    let _parent = Path::new(env!("CARGO_MANIFEST_DIR")).parent_wc().unwrap();
}
