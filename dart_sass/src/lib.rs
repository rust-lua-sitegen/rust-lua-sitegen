use std::{
    io::Write,
    path::{Path, PathBuf},
};

fn sass_release_suffix() -> &'static str {
    if cfg!(target_os = "linux") {
        if cfg!(all(target_arch = "arm", target_env = "musl")) {
            "linux-arm-musl.tar.gz"
        } else if cfg!(all(target_arch = "arm", not(target_env = "musl"))) {
            "linux-arm.tar.gz"
        } else if cfg!(all(target_arch = "aarch64", target_env = "musl")) {
            "linux-arm64-musl.tar.gz"
        } else if cfg!(all(target_arch = "aarch64", not(target_env = "musl"))) {
            "linux-arm64.tar.gz"
        } else if cfg!(all(target_arch = "x86_64", target_env = "musl")) {
            "linux-x64-musl.tar.gz"
        } else if cfg!(all(target_arch = "x86_64", not(target_env = "musl"))) {
            "linux-x64.tar.gz"
        } else {
            unreachable!("Unsupported Linux architecture");
        }
    } else if cfg!(target_os = "macos") {
        if cfg!(target_arch = "aarch64") {
            "macos-arm64.tar.gz"
        } else if cfg!(target_arch = "x86_64") {
            "macos-x64.tar.gz"
        } else {
            unreachable!("Unsupported macOS architecture");
        }
    } else if cfg!(target_os = "windows") {
        if cfg!(target_arch = "aarch64") {
            "windows-arm64.zip"
        } else if cfg!(target_arch = "x86_64") {
            "windows-x64.zip"
        } else {
            unreachable!("Unsupported Windows architecture");
        }
    } else {
        unreachable!("Unsupported operating system");
    }
}

fn sass_bin_name() -> &'static str {
    if cfg!(target_os = "windows") {
        "sass.bat"
    } else {
        "sass"
    }
}

fn ensure_bin(
    sass_version: &str,
    install_dir: impl AsRef<Path>,
) -> Result<PathBuf, std::io::Error> {
    let install_dir = install_dir.as_ref();
    let bin_dir = install_dir.join("dart-sass");
    let version_path = bin_dir.join("version.txt");
    if version_path.exists() && std::fs::read_to_string(&version_path)?.trim() == sass_version {
        return Ok(bin_dir.join(sass_bin_name()));
    }

    if bin_dir.exists() {
        std::fs::remove_dir_all(&bin_dir)?;
    }

    let suffix = sass_release_suffix();
    let release_url = format!("https://github.com/sass/dart-sass/releases/download/{sass_version}/dart-sass-{sass_version}-{suffix}");

    let response = reqwest::blocking::get(&release_url)
        .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, e))?;

    if !response.status().is_success() {
        return Err(std::io::Error::new(
            std::io::ErrorKind::Other,
            format!("Failed to download Dart Sass: {}", response.status()),
        ));
    }

    std::fs::create_dir_all(&install_dir)?;
    if suffix.ends_with(".tar.gz") {
        let mut archive = tar::Archive::new(flate2::read::GzDecoder::new(response));
        archive.unpack(&install_dir)?;
    } else if suffix.ends_with(".zip") {
        let response = std::io::Cursor::new(
            response
                .bytes()
                .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, e))?,
        );
        let mut archive = zip::ZipArchive::new(response)?;
        archive.extract(&install_dir)?;
    } else {
        unreachable!("Unsupported archive format");
    }
    std::fs::write(&version_path, sass_version)?;

    Ok(bin_dir.join(sass_bin_name()))
}

#[derive(Debug, Clone, Copy)]
pub enum OutputStyle {
    Compressed,
    Expanded,
}

#[derive(Debug, Clone)]
pub struct Options {
    indented: bool,
    load_path: Vec<PathBuf>,
    style: OutputStyle,
    charset: bool,
    error_css: bool,
    source_map: bool,
    quiet: bool,
    quiet_deps: bool,
    verbose: bool,
    fatal_deprecation: Vec<String>,
    future_deprecation: Vec<String>,
    stop_on_error: bool,
    color: bool,
    unicode: bool,
    trace: bool,
}

impl Default for Options {
    fn default() -> Self {
        Self::new()
    }
}

impl Options {
    pub fn new() -> Self {
        Self {
            indented: false,
            style: OutputStyle::Expanded,
            load_path: Vec::new(),
            charset: true,
            error_css: false,
            source_map: true,
            quiet: false,
            quiet_deps: false,
            verbose: false,
            fatal_deprecation: Vec::new(),
            future_deprecation: Vec::new(),
            stop_on_error: false,
            color: false,
            unicode: true,
            trace: false,
        }
    }

    /// Set whether to use the indented syntax for the input. Defaults to `false`.
    pub fn indented(&mut self, on: bool) -> &mut Self {
        self.indented = on;
        self
    }

    /// Add a path to use when resolving imports.
    pub fn load_path(&mut self, p: impl AsRef<Path>) -> &mut Self {
        self.load_path.push(p.as_ref().into());
        self
    }

    /// Set the output style. Defaults to `Expanded`.
    pub fn style(&mut self, style: OutputStyle) -> &mut Self {
        self.style = style;
        self
    }

    /// Set whether to emit a @charset or BOM for CSS with non-ASCII characters. Defaults to `true`.
    pub fn charset(&mut self, on: bool) -> &mut Self {
        self.charset = on;
        self
    }

    /// Set whether, when an error occurs, to emit a stylesheet describing it. Defaults to `false`.
    pub fn error_css(&mut self, on: bool) -> &mut Self {
        self.error_css = on;
        self
    }

    /// Set whether to generate source maps. Defaults to `true`.
    pub fn source_map(&mut self, on: bool) -> &mut Self {
        self.source_map = on;
        self
    }

    /// Set whether to suppress warnings. Defaults to `false`.
    pub fn quiet(&mut self, on: bool) -> &mut Self {
        self.quiet = on;
        self
    }

    /// Set whether to suppress warnings from dependencies. Defaults to `false`.
    pub fn quiet_deps(&mut self, on: bool) -> &mut Self {
        self.quiet_deps = on;
        self
    }

    /// Set whether to emit all deprecation warnings even when they're repetitive. Defaults to `false`.
    pub fn verbose(&mut self, on: bool) -> &mut Self {
        self.verbose = on;
        self
    }

    /// Add a deprecation warning which should be treated as an error. See
    /// https://sass-lang.com/documentation/cli/dart-sass/#fatal-deprecation for valid inputs.
    pub fn fatal_deprecation(&mut self, deprecation: &str) -> &mut Self {
        self.fatal_deprecation.push(deprecation.into());
        self
    }

    /// Opt in to a deprecation early. See https://pub.dev/documentation/sass/latest/sass/Deprecation.html.
    pub fn future_deprecation(&mut self, deprecation: &str) -> &mut Self {
        self.future_deprecation.push(deprecation.into());
        self
    }

    /// Set whether to stop compiling after encountering an error. Defaults to `false`.
    pub fn stop_on_error(&mut self, on: bool) -> &mut Self {
        self.stop_on_error = on;
        self
    }

    /// Set whether to use terminal colors for messages. Defaults to `false`.
    pub fn color(&mut self, on: bool) -> &mut Self {
        self.color = on;
        self
    }

    /// Set whether to use Unicode characters for messages. Defaults to `true`.
    pub fn unicode(&mut self, on: bool) -> &mut Self {
        self.unicode = on;
        self
    }

    /// Set whether to print full Dart stack traces for exceptions. Defaults to `false`.
    pub fn trace(&mut self, on: bool) -> &mut Self {
        self.trace = on;
        self
    }
}

#[derive(Debug)]
pub struct Engine {
    bin_path: PathBuf,
}

#[non_exhaustive]
#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("{0}")]
    Io(#[from] std::io::Error),
    #[error("{0}")]
    Sass(String),
}

#[derive(Debug, Clone)]
pub struct Output {
    pub css: String,
    pub warnings: String,
}

impl Engine {
    pub fn with_bin_path(bin_path: PathBuf) -> Self {
        Self { bin_path }
    }

    pub fn install(install_dir: impl AsRef<Path>) -> Result<Self, std::io::Error> {
        let bin_path = ensure_bin("1.77.4", install_dir)?;
        Ok(Self::with_bin_path(bin_path))
    }

    pub fn compile(&self, options: &Options, source: &str) -> Result<Output, Error> {
        let mut command = std::process::Command::new(&self.bin_path);
        command.arg("--stdin");
        if options.indented {
            command.arg("--indented");
        } else {
            command.arg("--no-indented");
        }
        for path in &options.load_path {
            command.arg("--load-path").arg(path);
        }
        match options.style {
            OutputStyle::Compressed => command.arg("--style").arg("compressed"),
            OutputStyle::Expanded => command.arg("--style").arg("expanded"),
        };
        if options.charset {
            command.arg("--charset");
        } else {
            command.arg("--no-charset");
        }
        if options.error_css {
            command.arg("--error-css");
        } else {
            command.arg("--no-error-css");
        }
        if options.source_map {
            command.arg("--source-map");
            command.arg("--embed-source-map");
            command.arg("--embed-sources");
        } else {
            command.arg("--no-source-map");
            command.arg("--no-embed-source-map");
            command.arg("--no-embed-sources");
        }
        if options.quiet {
            command.arg("--quiet");
        } else {
            command.arg("--no-quiet");
        }
        if options.quiet_deps {
            command.arg("--quiet-deps");
        } else {
            command.arg("--no-quiet-deps");
        }
        if options.verbose {
            command.arg("--verbose");
        } else {
            command.arg("--no-verbose");
        }
        for deprecation in &options.fatal_deprecation {
            command.arg("--fatal-deprecation").arg(deprecation);
        }
        for deprecation in &options.future_deprecation {
            command.arg("--future-deprecation").arg(deprecation);
        }
        if options.stop_on_error {
            command.arg("--stop-on-error");
        } else {
            command.arg("--no-stop-on-error");
        }
        if options.color {
            command.arg("--color");
        } else {
            command.arg("--no-color");
        }
        if options.unicode {
            command.arg("--unicode");
        } else {
            command.arg("--no-unicode");
        }
        if options.trace {
            command.arg("--trace");
        } else {
            command.arg("--no-trace");
        }

        let mut child = command
            .stdin(std::process::Stdio::piped())
            .stdout(std::process::Stdio::piped())
            .stderr(std::process::Stdio::piped())
            .spawn()?;

        let stdin = child.stdin.as_mut().unwrap();
        stdin.write_all(source.as_bytes())?;

        let output = child.wait_with_output()?;
        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr).to_string();
            return Err(Error::Sass(stderr));
        }

        let stdout = String::from_utf8(output.stdout)
            .map_err(|e| Error::Io(std::io::Error::new(std::io::ErrorKind::InvalidData, e)))?;

        let warnings = String::from_utf8(output.stderr)
            .map_err(|e| Error::Io(std::io::Error::new(std::io::ErrorKind::InvalidData, e)))?;

        Ok(Output {
            css: stdout,
            warnings,
        })
    }
}
