use std::path::{Path, PathBuf};

const CRATE_DIR: &str = env!("CARGO_MANIFEST_DIR");
const PION_EXE: &str = env!("CARGO_BIN_EXE_pion");

#[test]
// REASON: more readable
#[allow(clippy::items_after_statements)]
fn elab() {
    let workspace_dir = PathBuf::from(CRATE_DIR);
    let workspace_dir = workspace_dir.ancestors().nth(2).unwrap();
    std::env::set_current_dir(workspace_dir).unwrap();

    snapbox::harness::Harness::new("tests/elab", setup, test)
        .select(["*.pion"])
        .action_env("PION_SNAPSHOTS")
        .test();

    fn setup(input_path: PathBuf) -> snapbox::harness::Case {
        let name = input_path.strip_prefix("tests").unwrap();
        let name = name.display().to_string().replace('/', "::");
        let expected = input_path.with_extension("snapshot");
        snapbox::harness::Case {
            name,
            fixture: input_path,
            expected,
        }
    }

    fn test(input_path: &Path) -> Result<String, Box<dyn std::error::Error>> {
        let path = input_path.to_str().unwrap();
        let mut command = std::process::Command::new(PION_EXE);
        command.args(["--dump=core", "check", path]);
        let output = command.output()?;
        let exit_status = output.status;
        Ok(format!(
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
        ))
    }
}
