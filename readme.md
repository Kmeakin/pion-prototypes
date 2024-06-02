# mini-pion
Minimalist reimplementation of [pion](github.com/kmeakin/pion)

## Features
* [x] dependent lambda calculus
    * [x] local variables
    * [x] integer and boolean literals
    * [x] `let` expressions
    * [x] `fun` expressions
    * [x] `forall` expressions
    * [x] function application expressions
    * [x] propositional equality
    * [ ] type universes

* [x] unification
    * [x] inferring types of unnanotated parameters
    * [x] hole expressions
    * [x] implicit arguments
      * [x] specialization
      * [x] generalization
    * [ ] pruning

* [ ] recursion
    * [x] `fix`
    * [x] `let rec`
        * [x] single recursive value binding
        * [ ] mutually recursive value bindings
    * [ ] termination checking

* [ ] aggregate types
    * [x] dependent pairs
    * [x] record types
    * [ ] sum types
    * [ ] row types

* [ ] pattern matching
    * [x] `if` expressions
        * [ ] dependent `if` expressions
    * [x] single-layer pattern matching over integers and booleans
    * [x] multi-layer pattern matching compilation w/ coverage checking
    * [x] or-patterns
    * [ ] and-patterns
    * [ ] as-patterns
    * [x] pattern guards

* [ ] user interface
    * [x] `pion check`
    * [x] `pion eval`
    * [ ] `pion repl`
    * [ ] `pion fmt`
    * [ ] "commands" a la Lean4/Rocq (eg `#check term`, `#print metavars`)

* [ ] documentation
    * [ ] code comments
    * [ ] tutorial
    * [ ] typing rules
    * [ ] bibliography
    * [ ] spec
