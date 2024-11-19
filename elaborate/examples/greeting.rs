use elaborate::std::{fs::OpenOptionsContext, io::WriteContext};
use std::fs::OpenOptions;

fn main() -> anyhow::Result<()> {
    let mut file = OpenOptions::new()
        .create(true)
        .write(true)
        .open_wc("greeting.txt")?;
    file.write_all_wc(b"Hello, world!")?;
    Ok(())
}
