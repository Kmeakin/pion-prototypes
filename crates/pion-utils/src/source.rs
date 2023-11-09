use std::io::Read;

use anyhow::{anyhow, bail};
use camino::{self, Utf8Path, Utf8PathBuf};
use codespan_reporting::files as codespan_files;
use fxhash::FxHashMap;
use string32::{Str32 as str32, String32};

use crate::location::BytePos;

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FileId(u32);

impl From<FileId> for u32 {
    fn from(val: FileId) -> Self { val.0 }
}

impl From<FileId> for usize {
    fn from(val: FileId) -> Self { val.0 as Self }
}

#[derive(Default)]
pub struct SourceMap {
    next_file_id: u32,
    path_to_file_id: FxHashMap<Utf8PathBuf, FileId>,
    file_id_to_files: Vec<SourceFile>,
}

pub struct SourceFile {
    /// path (absolute)
    pub path: Utf8PathBuf,
    /// contents
    pub contents: String32,
    pub line_index: LineIndex,
}

pub struct LineIndex {
    /// starting byte indices in the source code
    line_starts: Box<[BytePos]>,
}

impl LineIndex {
    pub fn of_text(text: impl AsRef<str32>) -> Self {
        Self {
            line_starts: codespan_reporting::files::line_starts(text.as_ref().as_str())
                .map(BytePos::truncate_usize)
                .collect(),
        }
    }
}

impl SourceFile {
    pub fn new(path: Utf8PathBuf, contents: String32) -> Self {
        let line_index = LineIndex::of_text(&contents);
        Self {
            path,
            contents,
            line_index,
        }
    }

    pub fn read(path: Utf8PathBuf) -> anyhow::Result<Self> {
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

        Ok(Self::new(path, contents))
    }

    /// Return the starting byte index of the line with the specified line
    /// index. Convenience method that generates errors if necessary.
    fn line_start(&self, line_index: usize) -> Result<BytePos, codespan_files::Error> {
        match line_index.cmp(&self.line_index.line_starts.len()) {
            std::cmp::Ordering::Less => Ok(self
                .line_index
                .line_starts
                .get(line_index)
                .copied()
                .expect("failed despite previous check")),
            std::cmp::Ordering::Equal => Ok(self.contents.len().into()),
            std::cmp::Ordering::Greater => Err(codespan_files::Error::LineTooLarge {
                given: line_index,
                max: self.line_index.line_starts.len() - 1,
            }),
        }
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

    pub fn get(&self, file_id: FileId) -> Option<&SourceFile> {
        self.file_id_to_files.get(usize::from(file_id))
    }

    pub fn try_get(&self, file_id: FileId) -> Result<&SourceFile, codespan_files::Error> {
        self.get(file_id)
            .ok_or(codespan_reporting::files::Error::FileMissing)
    }
}

impl<'a> codespan_files::Files<'a> for SourceFile {
    type FileId = ();
    type Name = &'a Utf8Path;
    type Source = &'a str32;

    fn name(&'a self, (): Self::FileId) -> Result<Self::Name, codespan_files::Error> {
        Ok(&self.path)
    }

    fn source(&'a self, (): Self::FileId) -> Result<Self::Source, codespan_files::Error> {
        Ok(&self.contents)
    }

    fn line_index(
        &'a self,
        (): Self::FileId,
        byte_index: usize,
    ) -> Result<usize, codespan_files::Error> {
        Ok(self
            .line_index
            .line_starts
            .binary_search(&BytePos::truncate_usize(byte_index))
            .unwrap_or_else(|next_line| next_line - 1))
    }

    fn line_range(
        &'a self,
        (): Self::FileId,
        line_index: usize,
    ) -> Result<std::ops::Range<usize>, codespan_files::Error> {
        let line_start = self.line_start(line_index)?.into();
        let next_line_start = self.line_start(line_index + 1)?.into();

        Ok(line_start..next_line_start)
    }
}

impl<'a> codespan_files::Files<'a> for SourceMap {
    type FileId = FileId;
    type Name = &'a Utf8Path;
    type Source = &'a str32;

    fn name(&'a self, file_id: Self::FileId) -> Result<Self::Name, codespan_files::Error> {
        Ok(&self.try_get(file_id)?.path)
    }

    fn source(&'a self, file_id: Self::FileId) -> Result<Self::Source, codespan_files::Error> {
        Ok(&self.try_get(file_id)?.contents)
    }

    fn line_index(
        &'a self,
        file_id: Self::FileId,
        byte_index: usize,
    ) -> Result<usize, codespan_files::Error> {
        self.try_get(file_id)?.line_index((), byte_index)
    }

    fn line_range(
        &'a self,
        file_id: Self::FileId,
        line_index: usize,
    ) -> Result<std::ops::Range<usize>, codespan_files::Error> {
        self.try_get(file_id)?.line_range((), line_index)
    }
}
