use std::io::Read;

use anyhow::{anyhow, bail};
use camino::Utf8Path;
use fxhash::FxHashMap;
use line_index::LineIndex;
pub use string32::{Str32 as str32, String32};
use triomphe::Arc;
pub use {camino, line_index};

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FileId(u32);

impl From<FileId> for u32 {
    fn from(val: FileId) -> Self { val.0 }
}

impl From<FileId> for usize {
    #[allow(clippy::use_self)]
    fn from(val: FileId) -> Self { val.0 as usize }
}

#[derive(Default)]
pub struct SourceMap {
    next_file_id: u32,
    path_to_file_id: FxHashMap<Arc<Utf8Path>, FileId>,
    file_id_to_files: Vec<SourceFile>,
}

pub struct SourceFile {
    /// path (absolute)
    pub path: Arc<Utf8Path>,
    pub contents: String32,
    pub line_index: LineIndex,
}

impl SourceFile {
    pub fn new(path: Arc<Utf8Path>, contents: String32, line_index: LineIndex) -> Self {
        Self {
            path,
            contents,
            line_index,
        }
    }

    pub fn read(path: &str) -> anyhow::Result<Self> {
        let path = Utf8Path::new(path);

        let path = path
            .canonicalize_utf8()
            .map_err(|err| anyhow!("cannot open file {path:?}: {err}"))?;

        let mut file = std::fs::File::open(&path)
            .map_err(|err| anyhow!("cannot open file {path:?}: {err}"))?;

        let metadata = file
            .metadata()
            .map_err(|err| anyhow!("cannot open file {path:?}: {err}"))?;

        if metadata.len() > u64::from(u32::MAX) {
            bail!("pion source files must be 4GB or less")
        }

        let contents = {
            let mut contents = String::new();
            file.read_to_string(&mut contents)
                .map_err(|err| anyhow!("cannot read file {path:?}: {err}"))?;
            contents
        };

        drop(file);

        let contents = String32::try_from(contents)
            .map_err(|_| anyhow!("pion source files must be 4GB or less"))?;

        let path: Arc<str> = Arc::from(String::from(path));
        // SAFETY: `Utf8Path` has the same representation as `str`
        let path: Arc<Utf8Path> = unsafe { std::mem::transmute(path) };

        Ok(Self {
            path,
            line_index: LineIndex::new(contents.as_str()),
            contents,
        })
    }
}

impl SourceMap {
    pub fn new() -> Self { Self::default() }

    /// Ok if file was not present, Err if it was
    pub fn insert_file(&mut self, file: SourceFile) -> Option<FileId> {
        match self.path_to_file_id.entry(file.path.clone()) {
            std::collections::hash_map::Entry::Occupied(entry) => {
                let file_id = entry.get();
                self.file_id_to_files[usize::from(*file_id)] = file;
                Some(*file_id)
            }
            std::collections::hash_map::Entry::Vacant(entry) => {
                let file_id = FileId(self.next_file_id);
                entry.insert(file_id);
                self.file_id_to_files.push(file);
                self.next_file_id += 1;
                None
            }
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = (FileId, &SourceFile)> {
        let ids = (0..self.next_file_id).map(FileId);
        let files = self.file_id_to_files.iter();
        ids.zip(files)
    }
}
