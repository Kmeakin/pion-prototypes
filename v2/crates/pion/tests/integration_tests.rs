use std::path::{Path, PathBuf};

use libtest_mimic::Failed;
use walkdir::{DirEntry, WalkDir};

const WORKSPACE_DIR: &str = env!("CARGO_WORKSPACE_DIR");
const PION_EXE: &str = env!("CARGO_BIN_EXE_pion");
const TESTS_DIR: &str = "test-data";

fn main() {
    let args = libtest_mimic::Arguments::from_args();

    let workspace_dir = PathBuf::from(WORKSPACE_DIR);
    std::env::set_current_dir(workspace_dir).unwrap();

    let update_snapshots = std::env::var_os("UPDATE_EXPECT").is_some();

    let tests = (find_source_files(format!("{TESTS_DIR}/elab"))
        .map(|path| elab_test(&path, update_snapshots)))
    .collect();

    libtest_mimic::run(&args, tests).exit()
}

fn elab_test(input_path: &Path, update: bool) -> libtest_mimic::Trial {
    let test_name = input_path.strip_prefix(TESTS_DIR).unwrap();
    let test_name = test_name.display().to_string().replace('/', "::");
    let expected_path = input_path.with_extension("snapshot");

    let input_path = input_path.to_owned();
    libtest_mimic::Trial::test(test_name, move || {
        let mut command = std::process::Command::new(PION_EXE);
        command.args(["check", &input_path.display().to_string()]);
        let output = command.output()?;
        let exit_status = output.status;

        let actual = format!(
            r#"{exit_status}

stdout = """
{}
"""

stderr = """
{}
"""
"#,
            String::from_utf8(output.stdout).unwrap().trim_end(),
            String::from_utf8(output.stderr).unwrap().trim_end(),
        );

        let expected = if expected_path.exists() {
            std::fs::read_to_string(&expected_path)?
        } else {
            String::new()
        };

        if actual != expected {
            if update {
                std::fs::write(&expected_path, &actual).map_err(|error| {
                    format!("cannot write to `{}`: {error}", expected_path.display())
                })?;
                return Ok(());
            }

            let diff =
                similar_asserts::SimpleDiff::from_str(&expected, &actual, "expected", "actual");
            return Err(Failed::from(diff));
        }

        Ok(())
    })
}

/// Recursively walk over test files under a file path.
fn find_source_files(root: impl AsRef<Path>) -> impl Iterator<Item = PathBuf> {
    WalkDir::new(root)
        .into_iter()
        .filter_map(Result::ok)
        .filter(|entry| entry.file_type().is_file())
        .filter(|entry| matches!(entry.path().extension(), Some(ext) if ext == "pion"))
        .map(DirEntry::into_path)
}
