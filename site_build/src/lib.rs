use std::cell::RefCell;
use std::collections::HashMap;
use std::io::ErrorKind;
use std::io::{Error, Result};
use std::sync::{Arc, RwLock};

use axum::http::HeaderMap;
use camino::{Utf8Component, Utf8Path, Utf8PathBuf};
use notify::Watcher;
use tokio::net::ToSocketAddrs;

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
                std::fs::write(&abs_path, content)
            }
        }
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

impl<'a> Site<'a> {
    pub fn reborrow<'b>(&'b mut self) -> Site<'b> {
        Site { inner: self.inner }
    }

    fn canonicalize_abs_source_path(&mut self, path: &Utf8Path) -> Result<Utf8PathBuf> {
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

    pub fn read_source_bytes(&mut self, path: impl AsRef<Utf8Path>) -> Result<Vec<u8>> {
        log::trace!("reading source file (binary): {:?}", path.as_ref());
        let canon_abs_path = self.canonicalize_abs_source_path(path.as_ref())?;
        let bytes = std::fs::read(canon_abs_path)?;
        Ok(bytes)
    }

    pub fn read_source_str(&mut self, path: impl AsRef<Utf8Path>) -> Result<String> {
        log::trace!("reading source file (text  ): {:?}", path.as_ref());
        let canon_abs_path = self.canonicalize_abs_source_path(path.as_ref())?;
        let s = std::fs::read_to_string(&canon_abs_path)?;
        Ok(s)
    }

    pub fn read_source_dir(&mut self, path: impl AsRef<Utf8Path>) -> Result<Vec<String>> {
        log::trace!("reading source directory: {:?}", path.as_ref());
        let canon_abs_path = self.canonicalize_abs_source_path(path.as_ref())?;
        let entries = canon_abs_path
            .read_dir_utf8()?
            .map(|entry| entry.map(|entry| entry.file_name().to_string()))
            .collect::<Result<Vec<_>>>()?;
        Ok(entries)
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
        let src = self.canonicalize_abs_source_path(src.as_ref())?;
        let dst = dst.as_ref();
        let content = std::fs::read(&src)?;
        self.inner.target.write(dst, &content)
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

impl<F, E> SiteBuilder<F>
where
    F: (for<'a, 'b> FnMut(&'a mut Site<'b>) -> std::result::Result<(), E>) + Send + Sync + 'static,
    E: From<std::io::Error> + std::error::Error + Send + Sync + 'static,
{
    pub fn new(source_dir: impl AsRef<Utf8Path>, build_fn: F) -> Self {
        Self {
            source_dir: source_dir.as_ref().to_path_buf(),
            build_fn,
        }
    }

    pub fn build(mut self, target_dir: impl AsRef<Utf8Path>) -> std::result::Result<(), E> {
        let source_dir = self.source_dir.clone();
        let target = OutputTarget::InDirectory {
            dir: target_dir.as_ref().to_path_buf(),
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

    pub async fn serve(self, socket_addrs: impl ToSocketAddrs) -> Result<()> {
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

        let listener = tokio::net::TcpListener::bind(socket_addrs).await?;
        axum::serve(listener, app).await?;

        Ok(())
    }
}
