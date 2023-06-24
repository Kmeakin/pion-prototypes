use anyhow::{anyhow, bail};
use ariadne::{Label, Report, ReportKind, Source};
use fxhash::FxHashMap;
use pion_lexer::token::TokenError;
use pion_utils::string32::String32;
use triomphe::Arc;

#[derive(clap::Args)]
pub struct Args {
    files: Vec<String>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FileId(u32);

#[derive(Default)]
pub struct SourceMap {
    next_file_id: u32,
    path_to_file_id: FxHashMap<Arc<str>, FileId>,
    file_id_to_path: Vec<Arc<str>>,
    file_id_to_contents: Vec<String32<String>>,
}

impl SourceMap {
    pub fn new() -> Self { Self::default() }

    pub fn add_file(&mut self, path: &str) -> anyhow::Result<FileId> {
        let (path, contents) = read_file(path)?;
        let path: Arc<str> = path.into();

        let file_id = FileId(self.next_file_id);
        self.path_to_file_id.insert(path.clone(), file_id);
        self.file_id_to_path.push(path);
        self.file_id_to_contents.push(contents);

        self.next_file_id += 1;
        Ok(file_id)
    }

    pub fn iter(&self) -> impl Iterator<Item = (FileId, &Arc<str>, &String32<String>)> {
        let ids = (0..self.next_file_id).map(FileId);
        let paths = self.file_id_to_path.iter();
        let contents = self.file_id_to_contents.iter();
        ids.zip(paths)
            .zip(contents)
            .map(|((id, path), contents)| (id, path, contents))
    }
}

fn read_file(path: &str) -> anyhow::Result<(String, String32<String>)> {
    let path =
        std::fs::canonicalize(path).map_err(|err| anyhow!("cannot open file {path:?}: {err}"))?;

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

    let contents =
        std::fs::read_to_string(path).map_err(|err| anyhow!("cannot read file {path:?}: {err}"))?;

    Ok((path.to_owned(), contents.try_into().unwrap()))
}

fn stop_if_errors(error_count: u32) -> anyhow::Result<()> {
    match error_count {
        0 => Ok(()),
        1 => bail!("aborting due to 1 previous error"),
        _ => bail!("aborting due to {error_count} previous errors"),
    }
}

pub fn run(args: Args) -> anyhow::Result<()> {
    let mut source_map = SourceMap::new();
    let mut error_count = 0;

    for file in args.files {
        if let Err(error) = source_map.add_file(&file) {
            eprintln!("{error}");
            error_count += 1;
        }
    }

    stop_if_errors(error_count)?;

    for (_file_id, file_path, file_contents) in source_map.iter() {
        let tokens = pion_lexer::token::lex(file_contents.as_deref());
        for (result, span) in tokens {
            if let Err(error) = result {
                let offset = span.start.into();
                let span: std::ops::Range<usize> = span.into();

                let label = match error {
                    TokenError::UnknownCharacter => {
                        Label::new((file_path, span)).with_message("unknown character")
                    }
                    TokenError::BlockComment { .. } => {
                        Label::new((file_path, span)).with_message("unclosed block comment")
                    }
                };

                Report::build(ReportKind::Error, file_path, offset)
                    .with_message("parse error")
                    .with_label(label)
                    .finish()
                    .eprint((file_path, Source::from(file_contents)))?;

                error_count += 1;
            }
        }
    }

    stop_if_errors(error_count)?;

    Ok(())
}
