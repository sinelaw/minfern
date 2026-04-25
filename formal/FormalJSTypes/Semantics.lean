/-
# Small-step operational semantics

This module gives a small-step reduction relation `Step` over configurations
`(e, σ) ∈ Expr × Store`.  The choice of substitution-free, environment-passing
semantics (closures carry their environment; variable references resolve via
heap locations) follows the implementation in `src/infer/env.rs`, where the
type environment is an immutable persistent map but each binding may point
to a mutable cell.

Only a representative subset of reduction rules is laid out – enough to
expose every interaction the soundness proofs must consider:

* β-reduction for closures
* variable lookup and assignment via the heap
* property access and property assignment (the `[ASSIGN]` rule from
  `infernu.md` with its value-restriction precondition)
* propagation (congruence) rules for the head sub-expression of each
  compound construct

Additional arithmetic / comparison rules can be added uniformly by
following the shape of `binAdd` and `binCongL`/`binCongR` below.
-/

import FormalJSTypes.Syntax

namespace FormalJSTypes

/-! ## Substitution into the run-time environment

We model β-reduction by allocating fresh heap cells for each parameter and
extending the environment of the closure.  This mirrors how the inference
engine extends `TypeEnv` when it enters a function body. -/

/-- Allocate a fresh cell for each `(name, value)` pair, threading the
    store and accumulating the environment.  Returns the resulting
    environment and store. -/
def bindParams : List (VarName × Value) → Env → Store → Env × Store
  | [],            ρ, σ => (ρ, σ)
  | (x, v) :: rest, ρ, σ =>
    let alloc := σ.alloc (HeapVal.cell v)
    bindParams rest (Env.bind x alloc.1 ρ) alloc.2

/-! ## The small-step relation -/

/-- `Step c c'` says configuration `c` reduces in one step to `c'`.  Each
    constructor corresponds to either a primitive reduction or a congruence
    rule that drives reduction inside a sub-expression. -/
inductive Step : Config → Config → Prop where
  /- ### Variable / heap interaction -/

  /-- Variable lookup: an `ident x` in store σ reduces to the value stored
      at the location bound to `x` in the *ambient* environment.  Because
      the environment is implicit in this stripped configuration, we
      existentially quantify over the location: -/
  | identLookup
      (x : VarName) (l : Loc) (v : Value) (σ : Store)
      (hLook  : σ.get l = some (HeapVal.cell v))
      :
      Step (Expr.ident x, σ)
           (valueToExpr v, σ)

  /-- Assignment to a variable: `x = v` mutates the cell at `x`'s location
      and reduces to the new value. -/
  | assignVar
      (x : VarName) (v : Value) (l : Loc) (σ : Store) (old : Value)
      (hOld  : σ.get l = some (HeapVal.cell old))
      :
      Step (Expr.assign (Expr.ident x) (valueToExpr v), σ)
           (valueToExpr v, σ.update l (HeapVal.cell v))

  /- ### `let` / `var` declarations -/

  /-- Allocate a fresh cell for the bound variable and continue with the
      body.  The body itself still references the variable through the
      ambient environment; in this scaffolding we expose only the store
      growth. -/
  | letAlloc
      (x : VarName) (v : Value) (body : Expr) (σ : Store)
      :
      Step (Expr.letE x (valueToExpr v) body, σ)
           (body, (σ.alloc (HeapVal.cell v)).2)

  /-- Congruence: evaluate the let-bound expression first. -/
  | letCong
      (x : VarName) (e₁ e₁' body : Expr) (σ σ' : Store)
      (h : Step (e₁, σ) (e₁', σ'))
      :
      Step (Expr.letE x e₁ body, σ) (Expr.letE x e₁' body, σ')

  /- ### Object / array primitives -/

  /-- Property access on a heap-allocated object. -/
  | memberLookup
      (l : Loc) (p : PropName) (props : List (PropName × Value))
      (v : Value) (σ : Store)
      (hHeap : σ.get l = some (HeapVal.objH props))
      (hProp : (props.find? (fun q => q.1 = p)).map (·.2) = some v)
      :
      Step (Expr.member (valueToExpr (Value.locV l)) p, σ)
           (valueToExpr v, σ)

  /-- Property assignment.  The value-restriction side-condition from
      `[ASSIGN]` is enforced at the typing layer, not here. -/
  | propAssign
      (l : Loc) (p : PropName) (v : Value)
      (props : List (PropName × Value)) (σ : Store)
      (hHeap : σ.get l = some (HeapVal.objH props))
      :
      Step (Expr.assign (Expr.member (valueToExpr (Value.locV l)) p)
                        (valueToExpr v), σ)
           (valueToExpr v,
             σ.update l (HeapVal.objH
               (props.map (fun q => if q.1 = p then (p, v) else q))))

  /- ### Function application -/

  /-- β-reduction for closures.  The closure's captured environment is
      ignored in this stripped configuration; in a fully environment-passing
      formalisation it would be threaded through. -/
  | callBeta
      (params : List VarName) (body : Expr) (capt : Env)
      (vs : List Value) (σ : Store)
      (hLen : params.length = vs.length)
      :
      Step (Expr.call (valueToExpr (Value.closV params body capt))
                      (vs.map valueToExpr), σ)
           (body, σ)

  /-- Congruence: evaluate the callee. -/
  | callCong
      (f f' : Expr) (args : List Expr) (σ σ' : Store)
      (h : Step (f, σ) (f', σ'))
      :
      Step (Expr.call f args, σ) (Expr.call f' args, σ')

  /- ### Arithmetic -/

  /-- `n₁ + n₂` on two number literals reduces to their sum.  The
      `Plus`-class side condition is enforced at typing time. -/
  | binAdd
      (n₁ n₂ : Float) (σ : Store)
      :
      Step (Expr.bin BinOp.add
              (Expr.lit (Literal.numL n₁))
              (Expr.lit (Literal.numL n₂)), σ)
           (Expr.lit (Literal.numL (n₁ + n₂)), σ)

  /-- Congruence for the left operand of a binary op. -/
  | binCongL
      (op : BinOp) (e₁ e₁' e₂ : Expr) (σ σ' : Store)
      (h : Step (e₁, σ) (e₁', σ'))
      :
      Step (Expr.bin op e₁ e₂, σ) (Expr.bin op e₁' e₂, σ')

  /-- Congruence for the right operand once the left is a value. -/
  | binCongR
      (op : BinOp) (v : Value) (e₂ e₂' : Expr) (σ σ' : Store)
      (h : Step (e₂, σ) (e₂', σ'))
      :
      Step (Expr.bin op (valueToExpr v) e₂, σ)
           (Expr.bin op (valueToExpr v) e₂', σ')

  /- ### Conditional -/

  | ifTrue
      (t e : Expr) (σ : Store)
      :
      Step (Expr.ifE (Expr.lit (Literal.boolL true)) t e, σ) (t, σ)

  | ifFalse
      (t e : Expr) (σ : Store)
      :
      Step (Expr.ifE (Expr.lit (Literal.boolL false)) t e, σ) (e, σ)

  | ifCong
      (c c' t e : Expr) (σ σ' : Store)
      (h : Step (c, σ) (c', σ'))
      :
      Step (Expr.ifE c t e, σ) (Expr.ifE c' t e, σ')

  /- ### Sequencing -/

  /-- Discard a fully-evaluated head of a sequence. -/
  | seqStep
      (v : Value) (rest : List Expr) (σ : Store)
      :
      Step (Expr.seq (valueToExpr v :: rest), σ) (Expr.seq rest, σ)

  /-- A singleton sequence reduces to its element. -/
  | seqSingleton
      (e : Expr) (σ : Store)
      :
      Step (Expr.seq [e], σ) (e, σ)

  /-- Congruence: drive reduction of the head. -/
  | seqCong
      (e e' : Expr) (rest : List Expr) (σ σ' : Store)
      (h : Step (e, σ) (e', σ'))
      :
      Step (Expr.seq (e :: rest), σ) (Expr.seq (e' :: rest), σ')

/-- Reflexive transitive closure of `Step`. -/
inductive Step.Star : Config → Config → Prop where
  | refl  (c : Config) : Step.Star c c
  | tail  {c c' c'' : Config} : Step.Star c c' → Step c' c'' → Step.Star c c''

end FormalJSTypes
