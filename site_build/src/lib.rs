use std::cell::RefCell;
use std::collections::HashMap;
use std::ffi::OsString;
use std::io::ErrorKind;
use std::io::{Error, Result};
use std::net::{IpAddr, SocketAddr, ToSocketAddrs};
use std::sync::{Arc, RwLock};

use axum::http::HeaderMap;
use camino::{Utf8Component, Utf8Path, Utf8PathBuf};
use clap::Parser;
use notify::Watcher;

#[derive(Clone, Debug)]
enum InMemoryFsEntry {
    Directory {
        children: HashMap<String, InMemoryFsEntry>,
    },
    File {
        content: Vec<u8>,
    },
}

fn non_rel_path_error(path: &Utf8Path) -> Error {
    std::io::Error::new(
        ErrorKind::InvalidInput,
        format!("non-relative path: {:?}", path),
    )
}

fn parent_dir_path_error(path: &Utf8Path) -> Error {
    std::io::Error::new(
        ErrorKind::InvalidInput,
        format!("parent directory component inpath: {:?}", path),
    )
}

impl InMemoryFsEntry {
    fn get<'a>(&self, path: &Utf8Path) -> Result<&InMemoryFsEntry> {
        let mut entry = self;
        for component in path.components() {
            match entry {
                InMemoryFsEntry::Directory { children } => match component {
                    Utf8Component::CurDir => {}
                    Utf8Component::Prefix(_) | Utf8Component::RootDir => {
                        return Err(non_rel_path_error(path));
                    }
                    Utf8Component::ParentDir => {
                        return Err(parent_dir_path_error(path));
                    }
                    Utf8Component::Normal(component) => {
                        entry = children.get(component).ok_or_else(|| {
                            std::io::Error::new(
                                ErrorKind::NotFound,
                                format!("file not found: {:?}", path.to_string()),
                            )
                        })?;
                    }
                },
                InMemoryFsEntry::File { content: _ } => {
                    return Err(std::io::Error::new(
                        ErrorKind::NotFound,
                        format!("file not found: {:?}", path.to_string()),
                    ));
                }
            }
        }
        Ok(entry)
    }

    fn ensure_directory<'a>(
        &'a mut self,
        path: &Utf8Path,
    ) -> Result<&'a mut HashMap<String, InMemoryFsEntry>> {
        let mut entry: &'a mut InMemoryFsEntry = self;
        for component in path.components() {
            entry = match (component, entry) {
                (Utf8Component::CurDir, entry) => entry,
                (Utf8Component::Prefix(_), _) | (Utf8Component::RootDir, _) => {
                    return Err(non_rel_path_error(path));
                }
                (Utf8Component::ParentDir, _) => {
                    return Err(parent_dir_path_error(path));
                }
                (Utf8Component::Normal(component), InMemoryFsEntry::Directory { children }) => {
                    children.entry(component.to_string()).or_insert_with(|| {
                        InMemoryFsEntry::Directory {
                            children: HashMap::new(),
                        }
                    })
                }
                (Utf8Component::Normal(_), InMemoryFsEntry::File { content: _ }) => {
                    return Err(std::io::Error::new(
                        ErrorKind::AlreadyExists,
                        format!("file already exists: {:?}", path.to_string()),
                    ));
                }
            }
        }
        match entry {
            InMemoryFsEntry::Directory { children } => Ok(children),
            _ => unreachable!(),
        }
    }
}

#[derive(Debug)]
enum OutputTarget {
    InMemory { root: InMemoryFsEntry },
    InDirectory { dir: Utf8PathBuf },
}

fn check_simple_rel_path(path: &Utf8Path) -> Result<()> {
    for component in path.components() {
        match component {
            Utf8Component::CurDir | Utf8Component::Normal(_) => {}
            Utf8Component::Prefix(_) | Utf8Component::RootDir => {
                return Err(non_rel_path_error(path));
            }
            Utf8Component::ParentDir => {
                return Err(parent_dir_path_error(path));
            }
        }
    }
    Ok(())
}

impl OutputTarget {
    fn read(&self, path: &Utf8Path) -> Result<Vec<u8>> {
        check_simple_rel_path(path)?;
        match self {
            OutputTarget::InMemory { root } => {
                let entry = root.get(path)?;
                match entry {
                    InMemoryFsEntry::File { content } => Ok(content.clone()),
                    InMemoryFsEntry::Directory { children: _ } => Err(std::io::Error::new(
                        ErrorKind::NotFound,
                        format!("file not found: {:?}", path.to_string()),
                    )),
                }
            }
            OutputTarget::InDirectory { dir } => {
                let abs_path = dir.join(path);
                std::fs::read(&abs_path)
            }
        }
    }

    fn read_dir(&self, path: &Utf8Path) -> Result<Vec<String>> {
        check_simple_rel_path(path)?;
        match self {
            OutputTarget::InMemory { root } => {
                let entry = root.get(path)?;
                match entry {
                    InMemoryFsEntry::Directory { children } => {
                        let mut entries = children.keys().cloned().collect::<Vec<_>>();
                        entries.sort();
                        Ok(entries)
                    }
                    InMemoryFsEntry::File { content: _ } => Err(std::io::Error::new(
                        ErrorKind::NotFound,
                        format!("directory not found: {:?}", path.to_string()),
                    )),
                }
            }
            OutputTarget::InDirectory { dir } => {
                let abs_path = dir.join(path);
                let entries = abs_path
                    .read_dir_utf8()?
                    .map(|entry| entry.map(|entry| entry.file_name().to_string()))
                    .collect::<Result<Vec<_>>>()?;
                Ok(entries)
            }
        }
    }

    fn write(&mut self, path: &Utf8Path, content: &[u8]) -> Result<()> {
        check_simple_rel_path(path)?;
        match self {
            OutputTarget::InMemory { root } => {
                let parent = path.parent().ok_or_else(|| {
                    std::io::Error::new(
                        ErrorKind::InvalidInput,
                        format!("cannot write to root directory: {:?}", path.to_string()),
                    )
                })?;
                let parent_dir = root.ensure_directory(parent)?;
                parent_dir.insert(
                    path.file_name().unwrap().to_string(),
                    InMemoryFsEntry::File {
                        content: content.to_vec(),
                    },
                );
                Ok(())
            }
            OutputTarget::InDirectory { dir } => {
                let abs_path = dir.join(path);
                std::fs::create_dir_all(abs_path.parent().unwrap())?;
                std::fs::write(&abs_path, content)
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FileType(std::fs::FileType);

impl FileType {
    pub fn is_dir(&self) -> bool {
        self.0.is_dir()
    }

    pub fn is_file(&self) -> bool {
        self.0.is_file()
    }
}

#[derive(Debug)]
pub struct DirEntry {
    inner: std::fs::DirEntry,
    path: Utf8PathBuf,
}

impl DirEntry {
    pub fn file_name(&self) -> &str {
        self.path.file_name().unwrap()
    }

    pub fn file_type(&self) -> Result<FileType> {
        self.inner.file_type().map(FileType)
    }

    pub fn into_path(self) -> Utf8PathBuf {
        self.path
    }

    pub fn path(&self) -> &Utf8Path {
        &self.path
    }
}

#[derive(Debug)]
struct SiteInner {
    source_dir: Utf8PathBuf,
    target: OutputTarget,
}

#[derive(Debug)]
pub struct Site<'a> {
    inner: &'a mut SiteInner,
}

#[derive(Debug)]
pub struct WalkDirEntry {
    pub full_path: Utf8PathBuf,
    pub rel_path: Utf8PathBuf,
}

fn annotate_source_path(path: &Utf8Path) -> (impl FnOnce(std::io::Error) -> std::io::Error + '_) {
    move |err| {
        std::io::Error::new(
            err.kind(),
            format!("error accessing source path {:?}: {}", path, err),
        )
    }
}

impl<'a> Site<'a> {
    pub fn reborrow<'b>(&'b mut self) -> Site<'b> {
        Site { inner: self.inner }
    }

    fn canonicalize_source_path_inner(&mut self, path: &Utf8Path) -> Result<Utf8PathBuf> {
        for component in path.components() {
            match component {
                Utf8Component::Prefix(_) | Utf8Component::RootDir => {
                    return Err(non_rel_path_error(path));
                }
                Utf8Component::ParentDir => {
                    return Err(parent_dir_path_error(path));
                }
                _ => {}
            }
        }
        let canon_path = self.inner.source_dir.join(path).canonicalize_utf8()?;
        let canon_source_dir = self.inner.source_dir.canonicalize_utf8()?;
        if !canon_path.starts_with(&canon_source_dir) {
            return Err(non_rel_path_error(path));
        }
        Ok(canon_path)
    }

    fn canonicalize_source_path(&mut self, path: &Utf8Path) -> Result<Utf8PathBuf> {
        self.canonicalize_source_path_inner(path)
            .map_err(annotate_source_path(path))
    }

    /// `path` must be an absolute path prefixed by the source directory.
    fn relativize_source_path<'b>(&mut self, path: &'b Utf8Path) -> &'b Utf8Path {
        path.strip_prefix(&self.inner.source_dir).unwrap()
    }

    pub fn raw_source_path(&mut self, path: impl AsRef<Utf8Path>) -> Result<Utf8PathBuf> {
        let path = path.as_ref();
        let canon_path = self.canonicalize_source_path(path)?;
        Ok(canon_path)
    }

    pub fn read_source_bytes(&mut self, path: impl AsRef<Utf8Path>) -> Result<Vec<u8>> {
        log::trace!("reading source file (binary): {:?}", path.as_ref());
        let canon_abs_path = self.canonicalize_source_path(path.as_ref())?;
        let bytes = std::fs::read(canon_abs_path)?;
        Ok(bytes)
    }

    pub fn read_source_str(&mut self, path: impl AsRef<Utf8Path>) -> Result<String> {
        log::trace!("reading source file (text  ): {:?}", path.as_ref());
        let canon_abs_path = self.canonicalize_source_path(path.as_ref())?;
        let s = std::fs::read_to_string(&canon_abs_path)?;
        Ok(s)
    }

    pub fn read_source_dir(&mut self, path: impl AsRef<Utf8Path>) -> Result<Vec<DirEntry>> {
        log::trace!("reading source directory: {:?}", path.as_ref());
        let canon_abs_path = self.canonicalize_source_path(path.as_ref())?;
        let entries = canon_abs_path
            .read_dir_utf8()?
            .map(|entry| {
                entry.map(|entry| {
                    let path = self.relativize_source_path(entry.path()).to_owned();
                    DirEntry {
                        inner: entry.into_inner(),
                        path,
                    }
                })
            })
            .collect::<Result<Vec<_>>>()?;
        Ok(entries)
    }

    pub fn walk_source_dir(&mut self, path: impl AsRef<Utf8Path>) -> Result<Vec<WalkDirEntry>> {
        fn walk_source_dir_rec(
            site: &mut Site,
            full_path: &Utf8Path,
            rel_path: &Utf8Path,
            results: &mut Vec<WalkDirEntry>,
        ) -> Result<()> {
            for entry in site.read_source_dir(&full_path)? {
                if entry.file_type()?.is_dir() {
                    walk_source_dir_rec(
                        site,
                        entry.path(),
                        &rel_path.join(entry.file_name()),
                        results,
                    )?;
                } else {
                    results.push(WalkDirEntry {
                        full_path: entry.path().to_owned(),
                        rel_path: rel_path.join(entry.file_name()),
                    });
                }
            }
            Ok(())
        }
        let mut results = Vec::new();
        walk_source_dir_rec(self, path.as_ref(), "".into(), &mut results)?;
        Ok(results)
    }

    pub fn source_is_file(&mut self, path: impl AsRef<Utf8Path>) -> Result<bool> {
        log::trace!("checking if source is a file: {:?}", path.as_ref());
        let canon_abs_path = self.canonicalize_source_path(path.as_ref())?;
        Ok(canon_abs_path.is_file())
    }

    pub fn source_is_dir(&mut self, path: impl AsRef<Utf8Path>) -> Result<bool> {
        log::trace!("checking if source is a directory: {:?}", path.as_ref());
        let canon_abs_path = self.canonicalize_source_path(path.as_ref())?;
        Ok(canon_abs_path.is_dir())
    }

    /// `src` must be an absolute path and `dst` must be a relative path.
    fn copy_source_to_output_rec(
        &mut self,
        src: impl AsRef<Utf8Path>,
        dst: impl AsRef<Utf8Path>,
    ) -> Result<()> {
        if src.as_ref().is_dir() {
            for entry in src.as_ref().read_dir_utf8()? {
                let entry = entry?;
                let entry_dst = dst.as_ref().join(entry.file_name());
                self.copy_source_to_output_rec(entry.path(), entry_dst)?;
            }
        } else {
            let content = std::fs::read(src.as_ref())?;
            self.inner.target.write(dst.as_ref(), &content)?;
        }
        Ok(())
    }

    pub fn copy_source_to_output(
        &mut self,
        src: impl AsRef<Utf8Path>,
        dst: impl AsRef<Utf8Path>,
    ) -> Result<()> {
        log::trace!(
            "copying source to output: {:?} -> {:?}",
            src.as_ref(),
            dst.as_ref()
        );
        let src = self.canonicalize_source_path(src.as_ref())?;
        self.copy_source_to_output_rec(src, dst)
    }

    pub fn write_output(
        &mut self,
        path: impl AsRef<Utf8Path>,
        content: impl AsRef<[u8]>,
    ) -> Result<()> {
        log::trace!("writing output file: {:?}", path.as_ref());
        let path = path.as_ref();
        self.inner.target.write(path, content.as_ref())
    }

    pub fn read_output_bytes(&self, path: impl AsRef<Utf8Path>) -> Result<Vec<u8>> {
        log::trace!("reading output file (binary): {:?}", path.as_ref());
        let path = path.as_ref();
        self.inner.target.read(path)
    }

    pub fn read_output_str(&self, path: impl AsRef<Utf8Path>) -> Result<String> {
        log::trace!("reading output file (text  ): {:?}", path.as_ref());
        let path = path.as_ref();
        let bytes = self.inner.target.read(path)?;
        let s = String::from_utf8(bytes).map_err(|_| {
            std::io::Error::new(
                ErrorKind::InvalidData,
                format!("invalid UTF-8 data in file: {:?}", path.to_string()),
            )
        })?;
        Ok(s)
    }

    pub fn read_output_dir(&self, path: impl AsRef<Utf8Path>) -> Result<Vec<String>> {
        log::trace!("reading output directory: {:?}", path.as_ref());
        let path = path.as_ref();
        self.inner.target.read_dir(path)
    }
}

pub struct SiteBuilder<F> {
    source_dir: Utf8PathBuf,
    build_fn: F,
    target_dir: Option<Utf8PathBuf>,
    bind_addrs: Vec<SocketAddr>,
    use_env_logger: bool,
}

enum SiteCache {
    Dirty,
    Clean { root: InMemoryFsEntry },
    Error { err: String },
}

struct SiteState<F> {
    builder: SiteBuilder<F>,
    cache: SiteCache,
}

enum RequestError {
    NotFound,
    InternalServerError(String),
}

thread_local! {
    static LAST_PANIC_MSG: RefCell<Option<String>> = RefCell::new(None);
}

fn register_panic_hook() {
    std::panic::set_hook(Box::new(|panic_info| {
        let msg = panic_info.to_string();
        eprintln!("\n{}\n", msg);
        LAST_PANIC_MSG.with(|cell| *cell.borrow_mut() = Some(msg));
    }));
}

fn catch_panic<R, F: FnOnce() -> R + std::panic::UnwindSafe>(
    f: F,
) -> std::result::Result<R, String> {
    let result = std::panic::catch_unwind(f);
    match result {
        Ok(result) => Ok(result),
        Err(_) => {
            let msg = LAST_PANIC_MSG.with(|cell| cell.borrow_mut().take());
            Err(msg.unwrap_or_else(|| "unknown panic".to_string()))
        }
    }
}

#[derive(clap::Parser, Debug)]
struct CliArgs {
    #[command(subcommand)]
    subcommand: Subcommand,
}

#[derive(clap::Subcommand, Debug)]
enum Subcommand {
    /// Build the site to a directory
    #[clap(name = "build")]
    Build(BuildArgs),

    /// Serve the site over HTTP
    #[clap(name = "serve")]
    Serve(ServeArgs),
}

#[derive(clap::Args, Debug)]
struct BuildArgs {
    /// Path to target directory
    #[arg(short, long)]
    target_dir: Option<Utf8PathBuf>,
}

#[derive(clap::Args, Debug)]
struct ServeArgs {
    /// Address at which to bind the HTTP server
    #[arg(short, long)]
    bind: Option<Vec<String>>,
}

const TARGET_DIR_SENTINEL_FILE: &str =
    ".site_build_target_dir_2e59a0e8_2f9c_4bd5_8a36_e371c2e73aa1";

impl<F, E> SiteBuilder<F>
where
    F: (for<'a, 'b> FnMut(&'a mut Site<'b>) -> std::result::Result<(), E>) + Send + Sync + 'static,
    E: From<std::io::Error> + std::error::Error + Send + Sync + 'static,
{
    pub fn new(source_dir: impl AsRef<Utf8Path>, build_fn: F) -> Self {
        Self {
            source_dir: source_dir.as_ref().to_path_buf(),
            build_fn,
            target_dir: None,
            bind_addrs: vec![SocketAddr::new(
                IpAddr::V4(std::net::Ipv4Addr::LOCALHOST),
                8080,
            )],
            use_env_logger: true,
        }
    }

    pub fn target_dir(mut self, target_dir: impl AsRef<Utf8Path>) -> Self {
        self.target_dir = Some(target_dir.as_ref().to_path_buf());
        self
    }

    pub fn bind_addrs(mut self, bind_addrs: impl std::net::ToSocketAddrs) -> Result<Self> {
        self.bind_addrs = bind_addrs.to_socket_addrs()?.collect();
        Ok(self)
    }

    pub fn use_env_logger(mut self, use_env_logger: bool) -> Self {
        self.use_env_logger = use_env_logger;
        self
    }

    pub fn build(mut self) -> std::result::Result<(), E> {
        env_logger::builder()
            .filter_level(log::LevelFilter::Info)
            .init();

        let Some(target_dir) = self.target_dir else {
            return Err(std::io::Error::new(
                ErrorKind::InvalidInput,
                "no target directory configured",
            )
            .into());
        };

        // Ensure target directory is deleted if it exists
        match target_dir.metadata() {
            Err(e) if e.kind() == ErrorKind::NotFound => {}
            Err(e) => return Err(e.into()),
            Ok(metadata) if metadata.is_dir() => {
                // If sentinel file found, then we can safely delete the directory
                let sentinel_path = target_dir.join(TARGET_DIR_SENTINEL_FILE);
                if sentinel_path.exists() {
                    std::fs::remove_dir_all(&target_dir)?;
                } else {
                    return Err(std::io::Error::new(
                        ErrorKind::AlreadyExists,
                        format!(
                            "directory already exists: {:?}\n\
                            \n\
                            'site_build' looks for a file called '.site_build_target_dir_2e59a0e8_2f9c_4bd5_8a36_e371c2e73aa1'\n\
                            the root of the target directory to determine if it is safe to delete it. If this file is absent,\n\
                            you must delete the directory manually before 'site_build' will build to it.",
                            target_dir
                        ),
                    )
                    .into());
                }
            }
            Ok(_) => {
                return Err(std::io::Error::new(
                    ErrorKind::InvalidInput,
                    format!("not a directory: {:?}", target_dir),
                )
                .into());
            }
        }

        std::fs::create_dir_all(&target_dir)?;
        std::fs::write(target_dir.join(TARGET_DIR_SENTINEL_FILE), "")?;

        let source_dir = self.source_dir.clone();
        let target = OutputTarget::InDirectory {
            dir: target_dir.clone(),
        };
        let mut inner = SiteInner { source_dir, target };
        let mut site = Site { inner: &mut inner };
        (self.build_fn)(&mut site)?;
        Ok(())
    }

    fn make_response(
        root: &InMemoryFsEntry,
        path: &Utf8Path,
    ) -> std::result::Result<(axum::http::HeaderMap, Vec<u8>), RequestError> {
        let mut headers = HeaderMap::new();
        let entry = root.get(path).map_err(|_| RequestError::NotFound)?;
        let content = match entry {
            InMemoryFsEntry::File { content } => {
                let mime = mime_guess::from_path(path).first_or_octet_stream();
                headers.insert(
                    "content-type",
                    axum::http::HeaderValue::from_str(mime.as_ref()).unwrap(),
                );
                content.clone()
            }
            InMemoryFsEntry::Directory { children } => {
                if let Some(InMemoryFsEntry::File { content }) = children.get("index.html") {
                    headers.insert(
                        "content-type",
                        axum::http::HeaderValue::from_static("text/html"),
                    );
                    content.clone()
                } else {
                    return Err(RequestError::NotFound);
                }
            }
        };
        headers.insert(
            "content-length",
            axum::http::HeaderValue::from_str(&content.len().to_string()).unwrap(),
        );
        headers.insert(
            "cache-control",
            axum::http::HeaderValue::from_static("no-store"),
        );
        Ok((headers, content))
    }

    fn build_in_memory(&mut self) -> std::result::Result<InMemoryFsEntry, String> {
        let mut inner = SiteInner {
            source_dir: self.source_dir.to_path_buf(),
            target: OutputTarget::InMemory {
                root: InMemoryFsEntry::Directory {
                    children: HashMap::new(),
                },
            },
        };
        let mut site = Site { inner: &mut inner };
        let build_result = catch_panic(std::panic::AssertUnwindSafe(|| (self.build_fn)(&mut site)));
        match build_result {
            Ok(Ok(())) => {
                let OutputTarget::InMemory { root } = inner.target else {
                    unreachable!()
                };
                Ok(root)
            }
            Ok(Err(err)) => Err(err.to_string()),
            Err(err) => Err(err),
        }
    }

    fn handle_request_inner(
        state: &RwLock<SiteState<F>>,
        path: &Utf8Path,
    ) -> std::result::Result<(axum::http::HeaderMap, Vec<u8>), RequestError> {
        log::trace!("serving GET request: {:?}", path);
        let read_guard = state.read().unwrap();
        match &read_guard.cache {
            SiteCache::Dirty => {
                std::mem::drop(read_guard);
                let mut write_guard = state.write().unwrap();
                let state = &mut *write_guard;
                match &mut state.cache {
                    cache @ SiteCache::Dirty => {
                        log::info!("build site: begin");
                        match state.builder.build_in_memory() {
                            Ok(root) => {
                                *cache = SiteCache::Clean { root };
                                log::info!("build site: success");
                            }
                            Err(err) => {
                                *cache = SiteCache::Error { err: err.clone() };
                                log::error!("build site: error: {}", err.to_string());
                                return Err(RequestError::InternalServerError(err));
                            }
                        }
                    }
                    SiteCache::Clean { .. } => {}
                    SiteCache::Error { err } => {
                        return Err(RequestError::InternalServerError(err.clone()));
                    }
                };
                let SiteCache::Clean { root } = &state.cache else {
                    unreachable!()
                };
                Self::make_response(&root, path)
            }
            SiteCache::Clean { root } => Self::make_response(&root, path),
            SiteCache::Error { err } => {
                return Err(RequestError::InternalServerError(err.clone()));
            }
        }
    }

    async fn handle_request(
        state: &RwLock<SiteState<F>>,
        path: &Utf8Path,
    ) -> (axum::http::StatusCode, axum::http::HeaderMap, Vec<u8>) {
        match Self::handle_request_inner(state, path) {
            Ok((headers, body)) => (axum::http::StatusCode::OK, headers, body),
            Err(err) => {
                let (status, body) = match err {
                    RequestError::InternalServerError(err) => (
                        axum::http::StatusCode::INTERNAL_SERVER_ERROR,
                        err.into_bytes(),
                    ),
                    RequestError::NotFound => {
                        (axum::http::StatusCode::NOT_FOUND, b"Not Found".to_vec())
                    }
                };
                let mut headers = axum::http::HeaderMap::new();
                headers.insert(
                    "content-type",
                    axum::http::HeaderValue::from_static("text/plain"),
                );
                (status, headers, body)
            }
        }
    }

    pub async fn serve(mut self) -> Result<()> {
        env_logger::builder()
            .filter_level(log::LevelFilter::Info)
            .init();

        let bind_addrs = std::mem::take(&mut self.bind_addrs);
        if bind_addrs.is_empty() {
            return Err(std::io::Error::new(
                ErrorKind::InvalidInput,
                "no bind addresses configured",
            ));
        }

        register_panic_hook();

        let source_dir = self.source_dir.clone();

        let state = Arc::new(RwLock::new(SiteState {
            builder: self,
            cache: SiteCache::Dirty,
        }));

        let mut watcher = notify::recommended_watcher({
            let state = Arc::clone(&state);
            move |event: std::result::Result<notify::Event, notify::Error>| {
                let Ok(event) = event else {
                    return;
                };
                match event.kind {
                    notify::EventKind::Access(_) => {}
                    _ => {
                        let mut write_guard = state.write().unwrap();
                        log::trace!("invalidated cache");
                        write_guard.cache = SiteCache::Dirty;
                    }
                }
            }
        })
        .map_err(|e| std::io::Error::new(ErrorKind::Other, e))?;

        watcher
            .watch(source_dir.as_std_path(), notify::RecursiveMode::Recursive)
            .map_err(|e| std::io::Error::new(ErrorKind::Other, e))?;

        let app = axum::Router::new()
            .route(
                "/*path",
                axum::routing::get({
                    let state = Arc::clone(&state);
                    move |path: axum::extract::Path<String>| async move {
                        SiteBuilder::handle_request(&state, path.as_ref()).await
                    }
                }),
            )
            .route(
                "/",
                axum::routing::get({
                    let state = Arc::clone(&state);
                    move |()| async move { SiteBuilder::handle_request(&state, "".as_ref()).await }
                }),
            );

        let listener = tokio::net::TcpListener::bind(&bind_addrs as &[_]).await?;

        log::info!("listening on: {:?}", bind_addrs);

        axum::serve(listener, app).await?;

        Ok(())
    }

    async fn run_from_args_object(mut self, args: CliArgs) -> std::result::Result<(), E> {
        match args.subcommand {
            Subcommand::Build(build) => {
                if let Some(target_dir) = build.target_dir {
                    self = self.target_dir(target_dir);
                }
                self.build()?;
            }
            Subcommand::Serve(serve) => {
                if let Some(bind) = serve.bind {
                    let mut bind_addrs = Vec::new();
                    for addr in bind {
                        bind_addrs.extend(addr.to_socket_addrs()?.collect::<Vec<_>>());
                    }
                    self.bind_addrs = bind_addrs;
                }
                self.serve().await?;
            }
        }
        Ok(())
    }

    pub async fn run_from_args<T: Into<OsString> + Clone>(
        self,
        args_iter: impl Iterator<Item = T>,
    ) -> std::result::Result<(), E> {
        let args = CliArgs::try_parse_from(args_iter).map_err(|e| {
            std::io::Error::new(
                ErrorKind::InvalidInput,
                format!("failed to parse arguments: {}", e),
            )
        })?;
        self.run_from_args_object(args).await
    }

    pub async fn try_run_from_cli(self) -> std::result::Result<(), E> {
        let args =
            CliArgs::try_parse().map_err(|e| std::io::Error::new(ErrorKind::InvalidInput, e))?;
        self.run_from_args_object(args).await
    }

    pub async fn run_from_cli(self) -> ! {
        let result = self.try_run_from_cli().await;
        match result {
            Ok(()) => std::process::exit(0),
            Err(err) => {
                eprintln!("{}", err);
                std::process::exit(1);
            }
        }
    }
}
