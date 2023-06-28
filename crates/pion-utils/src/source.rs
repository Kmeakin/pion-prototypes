use anyhow::{anyhow, bail};
use fxhash::FxHashMap;
pub use line_index;
use line_index::LineIndex;
use triomphe::Arc;

use crate::string32::String32;

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
    path_to_file_id: FxHashMap<Arc<str>, FileId>,
    file_id_to_files: Vec<SourceFile>,
}

pub struct SourceFile {
    /// path (absolute)
    pub path: Arc<str>,
    pub contents: String32<String>,
    pub line_index: LineIndex,
}

impl SourceFile {
    pub fn new(path: Arc<str>, contents: String32<String>, line_index: LineIndex) -> Self {
        Self {
            path,
            contents,
            line_index,
        }
    }

    pub fn read(path: &str) -> anyhow::Result<Self> {
        let path = std::fs::canonicalize(path)
            .map_err(|err| anyhow!("cannot open file {path:?}: {err}"))?;

        let path = path
            .to_str()
            .ok_or_else(|| anyhow!("path {path:?} is not utf8"))?;

        let file =
            std::fs::File::open(path).map_err(|err| anyhow!("cannot open file {path:?}: {err}"))?;

        let metadata = file
            .metadata()
            .map_err(|err| anyhow!("cannot open file {path:?}: {err}"))?;

        if metadata.len() > u64::from(u32::MAX) {
            bail!("pion source files must be 4GB or less")
        }

        let contents = std::fs::read_to_string(path)
            .map_err(|err| anyhow!("cannot read file {path:?}: {err}"))?;

        let contents = String32::try_from(contents)
            .map_err(|_| anyhow!("pion source files must be 4GB or less"))?;

        Ok(Self {
            line_index: LineIndex::new(&contents),
            path: path.into(),
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
