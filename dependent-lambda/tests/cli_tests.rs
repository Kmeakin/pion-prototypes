#![feature(iter_intersperse)]

use expect_test::*;

const DEPENDENT_LAMBDA: &str = env!("CARGO_BIN_EXE_dependent-lambda");

fn check(
    command: &str,
    mut expected_stdout: expect_test::Expect,
    mut expected_stderr: expect_test::Expect,
) {
    let mut shell = std::process::Command::new("/bin/sh");
    let shell = shell.arg("-c");
    let command = shell.arg(command);
    let output = command.output().unwrap();
    let stdout = String::from_utf8(output.stdout).unwrap();
    let stderr = String::from_utf8(output.stderr).unwrap();

    let stdout: String = stdout
        .lines()
        .map(|line| line.trim_end())
        .intersperse("\n")
        .collect();
    let stderr: String = stderr
        .lines()
        .map(|line| line.trim_end())
        .intersperse("\n")
        .collect();

    expected_stdout.indent(false);
    expected_stderr.indent(false);

    expected_stdout.assert_eq(&stdout);
    expected_stderr.assert_eq(&stderr);
}

#[test]
fn cli_no_args() {
    check(
        DEPENDENT_LAMBDA,
        expect![[""]],
        expect![[r#"
Usage: dependent-lambda <COMMAND>

Commands:
  check
  eval
  help   Print this message or the help of the given subcommand(s)

Options:
  -h, --help  Print help"#]],
    );
}

#[test]
fn cli_incorrect_args() {
    check(
        &format!("{DEPENDENT_LAMBDA} check"),
        expect![[""]],
        expect![[r#"
error: the following required arguments were not provided:
  <PATH>

Usage: dependent-lambda check <PATH>

For more information, try '--help'."#]],
    );
    check(
        &format!("{DEPENDENT_LAMBDA} eval"),
        expect![[""]],
        expect![[r#"
error: the following required arguments were not provided:
  <PATH>

Usage: dependent-lambda eval <PATH>

For more information, try '--help'."#]],
    );
}

#[test]
fn consts() {
    check(
        &format!("{DEPENDENT_LAMBDA} check <(echo true)"),
        expect!["true : Bool"],
        expect![""],
    );
    check(
        &format!("{DEPENDENT_LAMBDA} check <(echo false)"),
        expect!["false : Bool"],
        expect![""],
    );
    check(
        &format!("{DEPENDENT_LAMBDA} check <(echo 5)"),
        expect!["5 : Int"],
        expect![""],
    );
}

#[test]
fn fun_arrow() {
    check(
        &format!("{DEPENDENT_LAMBDA} check <(echo 'Int -> Bool')"),
        expect!["(forall(_: Int) -> Bool) : Type"],
        expect![""],
    );
}

#[test]
fn fun_type() {
    check(
        &format!("{DEPENDENT_LAMBDA} check <(echo 'forall(x: Int) -> Bool')"),
        expect!["(forall(x: Int) -> Bool) : Type"],
        expect![""],
    );
    check(
        &format!("{DEPENDENT_LAMBDA} check <(echo 'forall (A: Type) -> A -> A')"),
        expect!["(forall(A: Type) -> forall(_: A) -> A) : Type"],
        expect![""],
    );
}

#[test]
fn fun_lit() {
    check(
        &format!("{DEPENDENT_LAMBDA} check <(echo 'fun(x: Int) => x')"),
        expect!["(fun(x: Int) => x) : forall(x: Int) -> Int"],
        expect![""],
    );
    check(
        &format!("{DEPENDENT_LAMBDA} check <(echo 'fun x => x')"),
        expect!["(fun(x: ?0) => x) : forall(x: ?0) -> ?0"],
        expect![""],
    );
    check(
        &format!("{DEPENDENT_LAMBDA} check <(echo '(fun x => x) : Int -> Int')"),
        expect!["(fun(x: Int) => x) : forall(_: Int) -> Int"],
        expect![""],
    );
}

#[test]
fn fun_app() {
    check(
        &format!("{DEPENDENT_LAMBDA} check <(echo '(fun x => x) 1')"),
        expect!["((fun(x: ?0) => x) 1) : Int"],
        expect![""],
    );
    check(
        &format!("{DEPENDENT_LAMBDA} check <(echo '(fun x => x) 1 2 3')"),
        expect!["#error : #error"],
        expect![[r#"
error: Expected function, found `Int`
  ┌─ /dev/fd/63:1:1
  │
1 │ (fun x => x) 1 2 3
  │ ^^^^^^^^^^^^^^
"#]],
    );
}
