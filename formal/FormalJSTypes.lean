/-
# FormalJSTypes — Lean 4 formalization of the minfern JavaScript subset

This is the entry point of the library.  The four sub-modules build the
formalisation in layers:

* `Syntax`    — surface syntax: `Ty`, `Expr`, `Value`, `Env`, `Store`.
* `Semantics` — small-step reduction relation `Step`.
* `Typing`    — bidirectional typing relation (`Infer` / `Check`),
                store typing and store-well-formedness.
* `Soundness` — Progress and Preservation theorem statements (proofs are
                left as `sorry`).
-/

import FormalJSTypes.Syntax
import FormalJSTypes.Semantics
import FormalJSTypes.Typing
import FormalJSTypes.Soundness
