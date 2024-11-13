use elaborate::std::fs::create_dir_wc;

fn main() -> anyhow::Result<()> {
    create_dir_wc("/dir")?;
    Ok(())
}
