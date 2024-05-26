#[tokio::main(flavor = "current_thread")]
async fn main() {
    site_build::SiteBuilder::new("./web/", move |site| -> Result<(), std::io::Error> {
        site.copy_source_to_output("index.html", "index.html")?;
        Ok(())
    })
    .serve("localhost:8080")
    .await
    .unwrap();
}
