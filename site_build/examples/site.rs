#[tokio::main(flavor = "current_thread")]
async fn main() {
    let root_path = camino::Utf8PathBuf::from(std::env::var("CARGO_MANIFEST_DIR").unwrap());
    let source_path = root_path.join("examples").join("web");
    let output_path = root_path.join("examples").join("web-out");
    site_build::SiteBuilder::new(&source_path, move |site| -> Result<(), std::io::Error> {
        site.copy_source_to_output("index.html", "index.html")?;
        Ok(())
    })
    .target_dir(&output_path)
    .run_from_cli()
    .await
}
