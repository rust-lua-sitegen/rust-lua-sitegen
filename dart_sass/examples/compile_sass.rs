fn main() {
    let root_path = std::path::PathBuf::from(std::env::var("CARGO_MANIFEST_DIR").unwrap());
    let install_path = root_path.join("examples").join("install");
    let engine = dart_sass::Engine::install(&install_path).unwrap();
    let result = engine
        .compile(
            &dart_sass::Options::default(),
            r#"
                $color-var: #123456;
                body { color: $color-var; }
            "#,
        )
        .unwrap();
    println!("{}", result.css);
}
