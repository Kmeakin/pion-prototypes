# mini-pion
Minimalist reimplementation of [pion](github.com/kmeakin/pion)

## Features
* [x] dependent lambda calculus
    * [x] local variables
    * [x] integer and boolean constants
    * [x] `let` expressions
    * [x] `fun` expressions
    * [x] `forall` expressions
    * [x] function application expressions
* [x] unification
    * [x] inferring types of unnanotated parameters
    * [x] hole expressions
    * [x] implicit arguments
      * [x] specialization
      * [x] generalization

* [ ] recursion
    * [ ] term level `fix`
    * [ ] type level `fix`?
    * [ ] `let rec`

* [ ] aggregate types
    * [ ] record types
    * [ ] sum types
    * [ ] row types

* [ ] pattern matching
    * [ ] single-layer pattern matching over integers and booleans
    * [ ] multi-layer pattern matching compilation w/ coverage checking

* [ ] user interface
    * [x] `pion check`
    * [x] `pion eval`
    * [ ] `pion repl`
    * [ ] "commands" a la Lean4/Rocq (eg `#check term`, `#print metavars`)

* [ ] documentation
    * [ ] code comments
    * [ ] tutorial
    * [ ] typing rules
    * [ ] bibliography
    * [ ] spec
