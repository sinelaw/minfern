/-
# Syntax of the minfern JavaScript subset

This module mirrors the abstract syntax exposed by the Rust implementation
(`src/parser/ast.rs`) and the type representation used by the inference engine
(`src/types/ty.rs`).  It defines, in Lean 4:

* `Ty`            – the surface types of the language (HMF + row polymorphism)
* `TypePred`      – type-class predicates (`Plus`, `Indexable`)
* `QualType`      – qualified types `(C => τ)`
* `TypeScheme`    – HMF schemes `∀ᾱ. Q ⇒ τ`
* `Literal`, `Expr`, `BinOp`, `UnaryOp` – the AST of expressions / statements
* `Value`         – the subset of expressions that are fully evaluated
* `HeapVal`,
  `Store`, `Env`  – the mutable heap and the lexical-scope environment

The implementation in `src/infer/env.rs` distinguishes mutable (`var`/`let`)
from immutable (`const`) bindings; both are represented here uniformly as
heap cells, with mutability enforced at the typing layer.

We use `Δ` (rather than the customary `Σ`) for the *store typing* throughout
the development because `Σ` is reserved in Lean 4 for the dependent-pair
type former `Σ x : α, β x`.
-/

namespace FormalJSTypes

/-! ## A pointwise relation on lists

Lean 4's core library does not export `List.Forall₂`.  We use a recursive
definition that walks the two lists structurally without unfolding via
`And`, sidestepping the kernel's restriction on nested inductives whose
parameters mention local variables. -/

def Forall2 {α β : Type} (R : α → β → Prop) : List α → List β → Prop
  | [],      []      => True
  | x :: xs, y :: ys => R x y ∧ Forall2 R xs ys
  | _,       _       => False

attribute [irreducible] Forall2

/-! ## Basic identifiers and tags -/

/-- The lexical name of a variable. -/
abbrev VarName  : Type := String

/-- The textual name of an object property. -/
abbrev PropName : Type := String

/-- Heap location, used for mutable variable cells, objects and arrays. -/
abbrev Loc : Type := Nat

/-- Type-variable identifier, mirroring `TVarId` in `src/types/ty.rs`. -/
abbrev TVarId : Type := Nat

/-- Distinguishes flexible (unification) variables from rigid skolems,
    matching `TVarName` in the Rust codebase. -/
inductive TVarKind where
  | flex
  | skolem
  deriving Repr, DecidableEq

/-- A typed variable name carries its kind and a fresh identifier. -/
structure TVar where
  kind : TVarKind
  id   : TVarId
  deriving Repr, DecidableEq

/-! ## Types

The `Ty` inductive type captures the unquantified syntax of types `ρ` from
`infernu.md`.  Polymorphism is layered on top via `TypeScheme`. -/

/-- Row tail: closed (no extra fields) or open (extensible via row
    variable).  Equi-recursive references are intentionally omitted from the
    scaffolding; they can be encoded via fixed points over `Ty`. -/
inductive RowTail where
  | closed   : RowTail
  | openVar  : TVarId → RowTail
  deriving Repr, Inhabited

/-- Surface types.  The constructors correspond 1-to-1 with the variants of
    `enum Type` in `src/types/ty.rs`. -/
inductive Ty where
  | number  : Ty
  | string  : Ty
  | bool    : Ty
  | undef   : Ty
  | null    : Ty
  | regex   : Ty
  | tvar    : TVar → Ty
  /-- Function type.  `thisTy = none` corresponds to `static_func` in the
      implementation; `thisTy = some t` corresponds to `func t params ret`. -/
  | func    : (thisTy : Option Ty) → (params : List Ty) → (ret : Ty) → Ty
  | array   : Ty → Ty
  | mapTy   : Ty → Ty
  | promise : Ty → Ty
  | row     : (props : List (PropName × Ty)) → (tail : RowTail) → Ty
  deriving Repr

/-! ## Type classes and qualified types -/

/-- Type-class names supported by the inference engine. -/
inductive ClassName where
  | plus       -- `Plus α`         : `+` operator on Number / String
  | indexable  -- `Indexable c i e`: container types with indexed access
  deriving Repr, DecidableEq

/-- A type-class predicate `C τ̄`. -/
structure TypePred where
  cls   : ClassName
  args  : List Ty
  deriving Repr

/-- A qualified type `Q ⇒ τ`. -/
structure QualType where
  preds : List TypePred
  ty    : Ty
  deriving Repr

/-- HMF type scheme `∀ᾱ. Q ⇒ τ`. -/
structure TypeScheme where
  vars : List TVar
  body : QualType
  deriving Repr

namespace TypeScheme
  /-- A monomorphic scheme has no quantified variables and no predicates. -/
  def mono (t : Ty) : TypeScheme :=
    { vars := [], body := { preds := [], ty := t } }

  /-- A polymorphic scheme without predicates. -/
  def poly (vs : List TVar) (t : Ty) : TypeScheme :=
    { vars := vs, body := { preds := [], ty := t } }
end TypeScheme

/-! ## Operators -/

/-- Binary operators (mirrors `enum BinOp` in `src/parser/ast.rs`). -/
inductive BinOp where
  -- Arithmetic
  | add | sub | mul | div | mod | pow
  -- Comparison
  | lt | gt | leq | geq | eqEq | neqEq | eqEqEq | neqEqEq
  -- Logical
  | and_ | or_
  -- Bitwise
  | bAnd | bOr | bXor | shl | shr | ushr
  -- Membership
  | inOp | instOf
  deriving Repr, DecidableEq

/-- Unary operators (mirrors `enum UnaryOp`). -/
inductive UnaryOp where
  | neg | pos | notOp | bitNot | typeOf | voidOp | deleteOp | awaitOp
  | preInc | preDec | postInc | postDec
  deriving Repr, DecidableEq

/-! ## Literals and expressions

Statements such as `if`, `while`, `var`, blocks and sequences are folded into
the `Expr` syntax via dedicated constructors so the formalization can use a
single small-step relation.  Block scoping is intentionally not modelled
(the implementation flattens `let` to `var` semantics). -/

inductive Literal where
  | numL   : Float → Literal
  | strL   : String → Literal
  | boolL  : Bool → Literal
  | undefL : Literal
  | nullL  : Literal
  | regexL : (pattern : String) → (flags : String) → Literal
  deriving Repr

inductive Expr where
  | lit     : Literal → Expr
  | ident   : VarName → Expr
  | thisE   : Expr
  | arr     : List Expr → Expr
  | obj     : List (PropName × Expr) → Expr
  /-- Function expression.  Captures the lexical environment when reduced
      to a closure value. -/
  | func    : (params : List VarName) → (body : Expr) → Expr
  /-- Member access `e.l`. -/
  | member  : Expr → PropName → Expr
  /-- Computed member access `e₁[e₂]`. -/
  | index   : Expr → Expr → Expr
  /-- Function call `f(ē)`. -/
  | call    : Expr → List Expr → Expr
  /-- `new C(ē)`. -/
  | newE    : Expr → List Expr → Expr
  | unary   : UnaryOp → Expr → Expr
  | bin     : BinOp → Expr → Expr → Expr
  /-- Assignment `lhs = rhs`; `lhs` must be a variable or a member. -/
  | assign  : Expr → Expr → Expr
  /-- Ternary `c ? t : e`. -/
  | cond    : Expr → Expr → Expr → Expr
  /-- Sequencing `(e₁, e₂, …, eₙ)`; also models statement sequencing. -/
  | seq     : List Expr → Expr
  /-- `var`/`let`/`const x = e₁ in e₂` (block-scoped surface syntax is
      desugared to this nested form). -/
  | letE    : VarName → Expr → Expr → Expr
  /-- `if (c) t else e` viewed as an expression returning `undefined` when
      branches return `undefined`. -/
  | ifE     : Expr → Expr → Expr → Expr
  /-- `while (c) body`; evaluates to `undefined`. -/
  | whileE  : Expr → Expr → Expr
  /-- `return e` reified inside an expression-level body. -/
  | retE    : Expr → Expr
  deriving Repr

/-! ## Values, environments and the heap

Variable mutation (`var`, parameter reassignment) and object-property
mutation are both expressed via a heap.  An `Env` maps lexical names to
heap locations of variable cells; objects and arrays themselves live as
records / vectors in the heap and are referenced through `Value.locV`.
-/

mutual
  /-- Run-time values.  Closures retain their definition environment so the
      semantics can be substitution-free. -/
  inductive Value where
    | numV   : Float → Value
    | strV   : String → Value
    | boolV  : Bool → Value
    | undefV : Value
    | nullV  : Value
    | regexV : (pattern : String) → (flags : String) → Value
    /-- Reference to an object/array allocated on the heap. -/
    | locV   : Loc → Value
    /-- Closure: parameter list, body, captured environment. -/
    | closV  : (params : List VarName) → (body : Expr) → (env : Env) → Value

  /-- Heap-resident values reached by `Value.locV`. -/
  inductive HeapVal where
    /-- A mutable variable cell holding a value. -/
    | cell : Value → HeapVal
    /-- An object record: an association list of property → value. -/
    | objH : List (PropName × Value) → HeapVal
    /-- A dense array of values. -/
    | arrH : List Value → HeapVal

  /-- A lexical environment: variable name ↦ heap location of its cell.
      Newer bindings shadow older ones. -/
  inductive Env where
    | empty : Env
    | bind  : VarName → Loc → Env → Env
end

/-- The heap, indexed by `Loc`. -/
abbrev Store : Type := List (Loc × HeapVal)

/-- A configuration of the abstract machine: the expression under reduction
    paired with the current store. -/
abbrev Config : Type := Expr × Store

namespace Env
  /-- Look up the location associated with `x`, returning `none` if unbound. -/
  def lookup : Env → VarName → Option Loc
    | Env.empty,        _ => none
    | Env.bind y l rest, x =>
      if x = y then some l else rest.lookup x
end Env

namespace Store
  /-- Look up a heap value by location.  Returns `none` if unallocated. -/
  def get (σ : Store) (l : Loc) : Option HeapVal :=
    (σ.find? (fun p => p.1 = l)).map (·.2)

  /-- A location strictly above every allocated address; used as the next
      free address by `alloc`. -/
  def freshLoc (σ : Store) : Loc :=
    σ.foldl (fun n p => Nat.max n (p.1 + 1)) 0

  /-- Allocate a new cell with value `h`; returns the fresh location and the
      extended store. -/
  def alloc (σ : Store) (h : HeapVal) : Loc × Store :=
    let l := σ.freshLoc
    (l, (l, h) :: σ)

  /-- Update the heap value at `l`.  If `l` is unallocated, returns `σ`
      unchanged (the typing relation will rule this out for well-typed
      programs). -/
  def update (σ : Store) (l : Loc) (h : HeapVal) : Store :=
    σ.map (fun p => if p.1 = l then (l, h) else p)
end Store

/-- A value is a syntactic fragment of `Expr`.  `valueToExpr` injects the
    fully-reduced subset back into `Expr` so the small-step relation can be
    stated on `Expr × Store` without an extra sort.  Heap-allocated values
    (`locV`) and closures (`closV`) round-trip via the dedicated
    constructors `Expr.lit`/`Expr.func`. -/
def valueToExpr : Value → Expr
  | Value.numV   n         => Expr.lit (Literal.numL n)
  | Value.strV   s         => Expr.lit (Literal.strL s)
  | Value.boolV  b         => Expr.lit (Literal.boolL b)
  | Value.undefV           => Expr.lit Literal.undefL
  | Value.nullV            => Expr.lit Literal.nullL
  | Value.regexV p f       => Expr.lit (Literal.regexL p f)
  | Value.locV   _         => Expr.lit Literal.undefL  -- locations are heap-only
  | Value.closV  ps b _    => Expr.func ps b

/-- Predicate: an expression is in normal (value) form.  Heap references
    appearing as expressions are treated as values via the syntactic lift
    `valueToExpr (Value.locV l)`. -/
inductive IsValue : Expr → Prop where
  | lit   (l : Literal)                          : IsValue (Expr.lit l)
  /-- A bare function literal whose parameters are syntactic and whose body
      is an arbitrary expression. -/
  | func  (ps : List VarName) (b : Expr)         : IsValue (Expr.func ps b)

end FormalJSTypes
