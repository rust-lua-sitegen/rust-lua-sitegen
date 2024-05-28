#[tokio::main(flavor = "current_thread")]
async fn main() {
    site_build::SiteBuilder::new("./web/", move |site| -> Result<(), std::io::Error> {
        site.copy_source_to_output("index.html", "index.html")?;
        Ok(())
    })
    .target_dir("./web-out/")
    .run_from_cli()
    .await
}
