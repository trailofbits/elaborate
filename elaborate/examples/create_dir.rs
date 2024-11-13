use std::fs::create_dir;

fn main() -> anyhow::Result<()> {
    create_dir("/dir")?;
    Ok(())
}
