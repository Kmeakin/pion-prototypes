#![feature(iter_intersperse)]
#![allow(clippy::needless_raw_string_hashes)]

use expect_test::*;

const PION: &str = env!("CARGO_BIN_EXE_pion");

fn check(expr: &str, mut expected: Expect) {
    let mut shell = std::process::Command::new("/bin/sh");
    let shell = shell.arg("-c");
    let command = shell.arg(format!("{PION} check - <<< '{expr}'"));
    let output = command.output().unwrap();

    let output = {
        let mut stdout = output.stdout;
        let mut stderr = output.stderr;
        stdout.append(&mut stderr);
        String::from_utf8(stdout).unwrap()
    };

    let output: String = output
        .lines()
        .map(str::trim_end)
        .intersperse("\n")
        .collect();

    expected.indent(false);
    expected.assert_eq(&output);
}

fn eval(expr: &str, mut expected: Expect) {
    let mut shell = std::process::Command::new("/bin/sh");
    let shell = shell.arg("-c");
    let command = shell.arg(format!("{PION} eval - <<< '{expr}'"));
    let output = command.output().unwrap();

    let output = {
        let mut stdout = output.stdout;
        let mut stderr = output.stderr;
        stdout.append(&mut stderr);
        String::from_utf8(stdout).unwrap()
    };

    let output: String = output
        .lines()
        .map(str::trim_end)
        .intersperse("\n")
        .collect();

    expected.indent(false);
    expected.assert_eq(&output);
}

#[test]
fn parse_errors() {
    check(
        "fun ",
        expect![[r##"
#error : #error
error: Syntax error: unexpected end of file
  ┌─ <stdin>:1:4
  │
1 │ fun
  │    ^ expected one of "(", "@", "Ident" or "_"
"##]],
    );
}

#[test]
fn consts() {
    check("true", expect!["true : Bool"]);
    check("false", expect!["false : Bool"]);
    check("5", expect!["5 : Int"]);
}

#[test]
fn prims() {
    check("Type", expect!["Type : Type"]);
    check("Int", expect!["Int : Type"]);
    check("Bool", expect!["Bool : Type"]);
    check("add", expect!["add : Int -> Int -> Int"]);
    check("sub", expect!["sub : Int -> Int -> Int"]);
    check("mul", expect!["mul : Int -> Int -> Int"]);
    check("eq", expect!["eq : Int -> Int -> Bool"]);
    check("ne", expect!["ne : Int -> Int -> Bool"]);
    check("lt", expect!["lt : Int -> Int -> Bool"]);
    check("gt", expect!["gt : Int -> Int -> Bool"]);
    check("lte", expect!["lte : Int -> Int -> Bool"]);
    check("gte", expect!["gte : Int -> Int -> Bool"]);
    check(
        "fix",
        expect!["fix : forall (@A : Type) (@B : Type) -> ((A -> B) -> A -> B) -> A -> B"],
    );
}

#[test]
fn arith_prims() {
    eval("add 3 2", expect!["5 : Int"]);
    eval("sub 3 2", expect!["1 : Int"]);
    eval("mul 3 2", expect!["6 : Int"]);
    eval("eq 1 0", expect!["false : Bool"]);
    eval("ne 1 0", expect!["true : Bool"]);
    eval("lt 1 0", expect!["false : Bool"]);
    eval("gt 1 0", expect!["true : Bool"]);
    eval("lte 1 0", expect!["false : Bool"]);
    eval("gte 1 0", expect!["true : Bool"]);
}

#[test]
fn fun_arrow() { check("Int -> Bool", expect!["(Int -> Bool) : Type"]); }

#[test]
fn fun_type() {
    check("forall (x : Int) -> Bool", expect!["(Int -> Bool) : Type"]);
    check(
        "forall (A : Type) -> A -> A",
        expect!["(forall (A : Type) -> A -> A) : Type"],
    );
    check(
        "forall (A : Type) (B: Type) -> A -> B",
        expect!["(forall (A : Type) (B : Type) -> A -> B) : Type"],
    );
    check(
        "forall (A : Type) (_ : A) -> A",
        expect!["(forall (A : Type) -> A -> A) : Type"],
    );
}

#[test]
fn fun_lit() {
    check(
        "fun(x : Int) => x",
        expect!["(fun (x : Int) => x) : Int -> Int"],
    );
    check(
        "fun (x : Int) (y : Bool) => x",
        expect!["(fun (x : Int) (y : Bool) => x) : Int -> Bool -> Int"],
    );
    check(
        "fun x => x",
        expect![[r#"
(fun (x : ?0) => x) : ?0 -> ?0
error: Unsolved metavariable: ?0
  ┌─ <stdin>:1:5
  │
1 │ fun x => x
  │     ^ could not infer type of variable `x`
"#]],
    );
    check(
        "fun x y => x",
        expect![[r#"
(fun (x : ?0) (y : ?1 x) => x) : forall (x : ?0) -> ?1 x -> ?0
error: Unsolved metavariable: ?0
  ┌─ <stdin>:1:5
  │
1 │ fun x y => x
  │     ^ could not infer type of variable `x`

error: Unsolved metavariable: ?1
  ┌─ <stdin>:1:7
  │
1 │ fun x y => x
  │       ^ could not infer type of variable `y`
"#]],
    );
    check(
        "(fun x => x) : Int -> Int",
        expect!["(fun (x : Int) => x) : Int -> Int"],
    );
    check(
        "(fun x y => x) : Int -> Bool -> Int",
        expect!["(fun (x : Int) (y : Bool) => x) : Int -> Bool -> Int"],
    );
}

#[test]
fn fun_app() {
    check("(fun x => x) 1", expect!["((fun (x : Int) => x) 1) : Int"]);
    check(
        "(fun x => x) 1 2 3",
        expect![[r#"
#error : #error
error: Expected function, found `Int`
  ┌─ <stdin>:1:1
  │
1 │ (fun x => x) 1 2 3
  │ ^^^^^^^^^^^^^^
"#]],
    );
}

#[test]
fn r#let() {
    check(
        "let f = fun x => x; f false",
        expect![[r#"
let f : Bool -> Bool = fun (x : Bool) => x;
(f false) : Bool"#]],
    );
    check(
        "let f : Bool -> Bool = fun x => x; f false",
        expect![[r#"
let f : Bool -> Bool = fun (x : Bool) => x;
(f false) : Bool"#]],
    );
}

#[test]
fn holes() {
    check(
        "let x: _ = 5; x",
        expect![[r#"
let x : Int = 5;
x : Int"#]],
    );
}

#[test]
fn implicit_args() {
    check("@Int -> Bool", expect!["(@Int -> Bool) : Type"]);
    check("forall (@x: Int) -> Bool", expect!["(@Int -> Bool) : Type"]);
    check(
        "fun (@x : Int) => x",
        expect!["(fun (@x : Int) => x) : @Int -> Int"],
    );
    check(
        "(fun (@x : Int) => x) @5",
        expect!["((fun (@x : Int) => x) @5) : Int"],
    );
    eval("(fun (@x : Int) => x) @5", expect!["5 : Int"]);
}

#[test]
fn generalize() {
    check(
        "let id: forall (@A: Type) -> A -> A = fun x => x; id",
        expect![[r#"
let id : forall (@A : Type) -> A -> A = fun (@A : Type) (x : A) => x;
id : forall (@A : Type) -> A -> A"#]],
    );
}

#[test]
fn specialize() {
    check(
        "let id: forall (@A: Type) -> A -> A = fun x => x; id 5",
        expect![[r#"
let id : forall (@A : Type) -> A -> A = fun (@A : Type) (x : A) => x;
(id @Int 5) : Int"#]],
    );
}

#[test]
fn plicity_mismatch() {
    check(
        "(fun (x : Int) => x) @5",
        expect![[r#"
#error : #error
error: Applied implicit argument when explicit argument was expected
  ┌─ <stdin>:1:22
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
        "if true then 1 else 0",
        expect!["(if true then 1 else 0) : Int"],
    );
    eval("if true then 1 else 0", expect!["1 : Int"]);
}

#[test]
fn fixpoint_factorial() {
    let fact = "fix (fun fact n => if eq n 0 then 1 else mul n (sub n 1))";

    check(
        fact,
        expect![
            "(fix @Int @Int (fun (fact : Int -> Int) (n : Int) => if eq n 0 then 1 else mul n \
             (sub n 1))) : Int -> Int"
        ],
    );
    eval(
        fact,
        expect![
            "(fix @Int @Int (fun (fact : Int -> Int) (n : Int) => if eq n 0 then 1 else mul n \
             (sub n 1))) : Int -> Int"
        ],
    );
    eval(&format!("{fact} 5"), expect!["20 : Int"]);
}

#[test]
fn fixpoint_parity() {
    let evenodd = "fix (fun (evenodd : Bool -> Int -> Bool) b n => if b then (if eq n 0 then true \
                   else evenodd false (sub n 1)) else (if eq n 0 then false else evenodd true \
                   (sub n 1)))";

    check(
        evenodd,
        expect![
            "(fix @Bool @(Int -> Bool) (fun (evenodd : Bool -> Int -> Bool) (b : Bool) (n : Int) \
             => if b then if eq n 0 then true else evenodd false (sub n 1) else if eq n 0 then \
             false else evenodd true (sub n 1))) : Bool -> Int -> Bool"
        ],
    );
    eval(
        evenodd,
        expect![
            "(fix @Bool @(Int -> Bool) (fun (evenodd : Bool -> Int -> Bool) (b : Bool) (n : Int) \
             => if b then if eq n 0 then true else evenodd false (sub n 1) else if eq n 0 then \
             false else evenodd true (sub n 1))) : Bool -> Int -> Bool"
        ],
    );
    eval(
        &format!("{evenodd} true"),
        expect![
            "(fun (n : Int) => if eq n 0 then true else fix @Bool @(Int -> Bool) (fun (evenodd : \
             Bool -> Int -> Bool) (b : Bool) (n : Int) => if b then if eq n 0 then true else \
             evenodd false (sub n 1) else if eq n 0 then false else evenodd true (sub n 1)) false \
             (sub n 1)) : Int -> Bool"
        ],
    );
    eval(&format!("{evenodd} true 3"), expect!["false : Bool"]);
}
