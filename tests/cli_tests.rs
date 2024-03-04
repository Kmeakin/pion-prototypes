#![feature(iter_intersperse)]
#![allow(clippy::needless_raw_string_hashes)]

use expect_test::*;

const PION: &str = env!("CARGO_BIN_EXE_pion");

fn check(command: &str, mut expected_stdout: Expect, mut expected_stderr: Expect) {
    let mut shell = std::process::Command::new("/bin/sh");
    let shell = shell.arg("-c");
    let command = shell.arg(command);
    let output = command.output().unwrap();
    let stdout = String::from_utf8(output.stdout).unwrap();
    let stderr = String::from_utf8(output.stderr).unwrap();

    let stdout: String = stdout
        .lines()
        .map(str::trim_end)
        .intersperse("\n")
        .collect();
    let stderr: String = stderr
        .lines()
        .map(str::trim_end)
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
        PION,
        expect![[""]],
        expect![[r#"
Usage: pion <COMMAND>

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
        &format!("{PION} check"),
        expect![[""]],
        expect![[r#"
error: the following required arguments were not provided:
  <PATH>

Usage: pion check <PATH>

For more information, try '--help'."#]],
    );
    check(
        &format!("{PION} eval"),
        expect![[""]],
        expect![[r#"
error: the following required arguments were not provided:
  <PATH>

Usage: pion eval <PATH>

For more information, try '--help'."#]],
    );
}

#[test]
fn parse_errors() {
    check(
        &format!("{PION} check <(echo fun )"),
        expect!["#error : #error"],
        expect![[r#"
error: Syntax error: unexpected end of file
  ┌─ /dev/fd/63:1:4
  │
1 │ fun
  │    ^ expected one of "(", "@", "Ident" or "_"
"#]],
    );
}

#[test]
fn consts() {
    check(
        &format!("{PION} check <(echo true)"),
        expect!["true : Bool"],
        expect![""],
    );
    check(
        &format!("{PION} check <(echo false)"),
        expect!["false : Bool"],
        expect![""],
    );
    check(
        &format!("{PION} check <(echo 5)"),
        expect!["5 : Int"],
        expect![""],
    );
}

#[test]
fn prims() {
    check(
        &format!("{PION} check <(echo Type)"),
        expect!["Type : Type"],
        expect![""],
    );
    check(
        &format!("{PION} check <(echo Int)"),
        expect!["Int : Type"],
        expect![""],
    );
    check(
        &format!("{PION} check <(echo Bool)"),
        expect!["Bool : Type"],
        expect![""],
    );
    check(
        &format!("{PION} check <(echo add)"),
        expect!["add : Int -> Int -> Int"],
        expect![""],
    );
    check(
        &format!("{PION} check <(echo sub)"),
        expect!["sub : Int -> Int -> Int"],
        expect![""],
    );
    check(
        &format!("{PION} check <(echo mul)"),
        expect!["mul : Int -> Int -> Int"],
        expect![""],
    );
    check(
        &format!("{PION} check <(echo eq)"),
        expect!["eq : Int -> Int -> Bool"],
        expect![""],
    );
    check(
        &format!("{PION} check <(echo ne)"),
        expect!["ne : Int -> Int -> Bool"],
        expect![""],
    );
    check(
        &format!("{PION} check <(echo lt)"),
        expect!["lt : Int -> Int -> Bool"],
        expect![""],
    );
    check(
        &format!("{PION} check <(echo gt)"),
        expect!["gt : Int -> Int -> Bool"],
        expect![""],
    );
    check(
        &format!("{PION} check <(echo lte)"),
        expect!["lte : Int -> Int -> Bool"],
        expect![""],
    );
    check(
        &format!("{PION} check <(echo gte)"),
        expect!["gte : Int -> Int -> Bool"],
        expect![""],
    );
    check(
        &format!("{PION} check <(echo fix)"),
        expect!["fix : forall (@A : Type) (@B : Type) -> ((A -> B) -> A -> B) -> A -> B"],
        expect![""],
    );
}

#[test]
fn arith_prims() {
    check(
        &format!("{PION} eval <(echo add 3 2)"),
        expect!["5 : Int"],
        expect![""],
    );
    check(
        &format!("{PION} eval <(echo sub 3 2)"),
        expect!["1 : Int"],
        expect![""],
    );
    check(
        &format!("{PION} eval <(echo mul 3 2)"),
        expect!["6 : Int"],
        expect![""],
    );
    check(
        &format!("{PION} eval <(echo eq 1 0)"),
        expect!["false : Bool"],
        expect![""],
    );
    check(
        &format!("{PION} eval <(echo ne 1 0)"),
        expect!["true : Bool"],
        expect![""],
    );
    check(
        &format!("{PION} eval <(echo lt 1 0)"),
        expect!["false : Bool"],
        expect![""],
    );
    check(
        &format!("{PION} eval <(echo gt 1 0)"),
        expect!["true : Bool"],
        expect![""],
    );
    check(
        &format!("{PION} eval <(echo lte 1 0)"),
        expect!["false : Bool"],
        expect![""],
    );
    check(
        &format!("{PION} eval <(echo gte 1 0)"),
        expect!["true : Bool"],
        expect![""],
    );
}

#[test]
fn fun_arrow() {
    check(
        &format!("{PION} check <(echo 'Int -> Bool')"),
        expect!["(Int -> Bool) : Type"],
        expect![""],
    );
}

#[test]
fn fun_type() {
    check(
        &format!("{PION} check <(echo 'forall (x : Int) -> Bool')"),
        expect!["(Int -> Bool) : Type"],
        expect![""],
    );
    check(
        &format!("{PION} check <(echo 'forall (A : Type) -> A -> A')"),
        expect!["(forall (A : Type) -> A -> A) : Type"],
        expect![""],
    );
    check(
        &format!("{PION} check <(echo 'forall (A : Type) (B: Type) -> A -> B')"),
        expect!["(forall (A : Type) (B : Type) -> A -> B) : Type"],
        expect![""],
    );
    check(
        &format!("{PION} check <(echo 'forall (A : Type) (_ : A) -> A')"),
        expect!["(forall (A : Type) -> A -> A) : Type"],
        expect![""],
    );
}

#[test]
fn fun_lit() {
    check(
        &format!("{PION} check <(echo 'fun(x : Int) => x')"),
        expect!["(fun (x : Int) => x) : Int -> Int"],
        expect![""],
    );
    check(
        &format!("{PION} check <(echo 'fun (x : Int) (y : Bool) => x')"),
        expect!["(fun (x : Int) (y : Bool) => x) : Int -> Bool -> Int"],
        expect![""],
    );
    check(
        &format!("{PION} check <(echo 'fun x => x')"),
        expect!["(fun (x : ?0) => x) : ?0 -> ?0"],
        expect![[r#"
error: Unsolved metavariable: ?0
  ┌─ /dev/fd/63:1:5
  │
1 │ fun x => x
  │     ^ could not infer type of variable `x`
"#]],
    );
    check(
        &format!("{PION} check <(echo 'fun x y => x')"),
        expect!["(fun (x : ?0) (y : ?1 x) => x) : forall (x : ?0) -> ?1 x -> ?0"],
        expect![[r#"
error: Unsolved metavariable: ?0
  ┌─ /dev/fd/63:1:5
  │
1 │ fun x y => x
  │     ^ could not infer type of variable `x`

error: Unsolved metavariable: ?1
  ┌─ /dev/fd/63:1:7
  │
1 │ fun x y => x
  │       ^ could not infer type of variable `y`
"#]],
    );
    check(
        &format!("{PION} check <(echo '(fun x => x) : Int -> Int')"),
        expect!["(fun (x : Int) => x) : Int -> Int"],
        expect![""],
    );
    check(
        &format!("{PION} check <(echo '(fun x y => x) : Int -> Bool -> Int')"),
        expect!["(fun (x : Int) (y : Bool) => x) : Int -> Bool -> Int"],
        expect![""],
    );
}

#[test]
fn fun_app() {
    check(
        &format!("{PION} check <(echo '(fun x => x) 1')"),
        expect!["((fun (x : Int) => x) 1) : Int"],
        expect![""],
    );
    check(
        &format!("{PION} check <(echo '(fun x => x) 1 2 3')"),
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

#[test]
fn r#let() {
    check(
        &format!("{PION} check <(echo 'let f = fun x => x; f false')"),
        expect![[r#"
let f : Bool -> Bool = fun (x : Bool) => x;
(f false) : Bool"#]],
        expect![""],
    );
    check(
        &format!("{PION} check <(echo 'let f : Bool -> Bool = fun x => x; f false')"),
        expect![[r#"
let f : Bool -> Bool = fun (x : Bool) => x;
(f false) : Bool"#]],
        expect![""],
    );
}

#[test]
fn holes() {
    check(
        &format!("{PION} check <(echo 'let x: _ = 5; x')"),
        expect![[r#"
let x : Int = 5;
x : Int"#]],
        expect![[""]],
    );
}

#[test]
fn implicit_args() {
    check(
        &format!("{PION} check <(echo '@Int -> Bool')"),
        expect!["(@Int -> Bool) : Type"],
        expect![""],
    );
    check(
        &format!("{PION} check <(echo 'forall (@x: Int) -> Bool')"),
        expect!["(@Int -> Bool) : Type"],
        expect![""],
    );
    check(
        &format!("{PION} check <(echo 'fun (@x : Int) => x')"),
        expect!["(fun (@x : Int) => x) : @Int -> Int"],
        expect![""],
    );
    check(
        &format!("{PION} check <(echo '(fun (@x : Int) => x) @5')"),
        expect!["((fun (@x : Int) => x) @5) : Int"],
        expect![""],
    );
    check(
        &format!("{PION} eval <(echo '(fun (@x : Int) => x) @5')"),
        expect!["5 : Int"],
        expect![""],
    );
}

#[test]
fn generalize() {
    check(
        &format!("{PION} check <(echo 'let id: forall (@A: Type) -> A -> A = fun x => x; id')"),
        expect![[r#"
let id : forall (@A : Type) -> A -> A = fun (x : Type) (x : x) => x;
id : forall (@A : Type) -> A -> A"#]],
        expect![""],
    );
}

#[test]
fn specialize() {
    check(
        &format!("{PION} check <(echo 'let id: forall (@A: Type) -> A -> A = fun x => x; id 5')"),
        expect![[r#"
let id : forall (@A : Type) -> A -> A = fun (x : Type) (x : x) => x;
(id @Int 5) : Int"#]],
        expect![""],
    );
}

#[test]
fn plicity_mismatch() {
    check(
        &format!("{PION} check <(echo '(fun (x : Int) => x) @5')"),
        expect!["#error : #error"],
        expect![[r#"
error: Applied implicit argument when explicit argument was expected
  ┌─ /dev/fd/63:1:22
  │
1 │ (fun (x : Int) => x) @5
  │ -------------------- ^^ implicit argument
  │ │
  │ function has type Int -> Int
"#]],
    );
}

#[test]
fn if_then_else() {
    check(
        &format!("{PION} check <(echo 'if true then 1 else 0')"),
        expect!["(if true then 1 else 0) : Int"],
        expect![""],
    );
    check(
        &format!("{PION} eval <(echo 'if true then 1 else 0')"),
        expect!["1 : Int"],
        expect![""],
    );
}

#[test]
fn fixpoint_factorial() {
    let fact = "fix (fun fact n => if eq n 0 then 1 else mul n (sub n 1))";

    check(
        &format!("{PION} check <(echo '{fact}')"),
        expect![
            "(fix @Int @Int (fun (fact : Int -> Int) (n : Int) => if eq n 0 then 1 else mul n \
             (sub n 1))) : Int -> Int"
        ],
        expect![""],
    );
    check(
        &format!("{PION} eval <(echo '{fact}')"),
        expect![
            "(fix @Int @Int (fun (fact : Int -> Int) (n : Int) => if eq n 0 then 1 else mul n \
             (sub n 1))) : Int -> Int"
        ],
        expect![""],
    );
    check(
        &format!("{PION} eval <(echo '{fact} 5')"),
        expect!["20 : Int"],
        expect![""],
    );
}

#[test]
fn fixpoint_parity() {
    let evenodd = "fix (fun (evenodd : Bool -> Int -> Bool) b n => if b then (if eq n 0 then true \
                   else evenodd false (sub n 1)) else (if eq n 0 then false else evenodd true \
                   (sub n 1)))";

    check(
        &format!("{PION} check <(echo '{evenodd}')"),
        expect![
            "(fix @Bool @(Int -> Bool) (fun (evenodd : Bool -> Int -> Bool) (b : Bool) (n : Int) \
             => if b then if eq n 0 then true else evenodd false (sub n 1) else if eq n 0 then \
             false else evenodd true (sub n 1))) : Bool -> Int -> Bool"
        ],
        expect![""],
    );
    check(
        &format!("{PION} eval <(echo '{evenodd}')"),
        expect![
            "(fix @Bool @(Int -> Bool) (fun (evenodd : Bool -> Int -> Bool) (b : Bool) (n : Int) \
             => if b then if eq n 0 then true else evenodd false (sub n 1) else if eq n 0 then \
             false else evenodd true (sub n 1))) : Bool -> Int -> Bool"
        ],
        expect![""],
    );
    check(
        &format!("{PION} eval <(echo '{evenodd} true')"),
        expect![
            "(fun (n : Int) => if eq n 0 then true else fix @Bool @(Int -> Bool) (fun (evenodd : \
             Bool -> Int -> Bool) (b : Bool) (n : Int) => if b then if eq n 0 then true else \
             evenodd false (sub n 1) else if eq n 0 then false else evenodd true (sub n 1)) false \
             (sub n 1)) : Int -> Bool"
        ],
        expect![""],
    );
    check(
        &format!("{PION} eval <(echo '{evenodd} true 3')"),
        expect!["false : Bool"],
        expect![""],
    );
}
