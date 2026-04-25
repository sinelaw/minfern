/-
# Typing relation

The minfern implementation drives type checking with **HMF unification**
(`src/infer/unify.rs`) and exposes two-mode behaviour familiar from
bidirectional type checking:

* **Inference** (`infer_expr` and friends in `src/infer/infer.rs`) – the
  default, used for every expression that has no annotation; produces a
  fresh principal type.
* **Checking** – when an annotation is present (`/*: T */`, `/** x: T */`),
  the inferred type is unified with the declared type, which acts as an
  upward constraint flowing into the term.

We model this two-mode regime explicitly with two mutually inductive
relations, `Infer Γ e τ` (`Γ ⊢ e ⇒ τ`) and `Check Γ e τ` (`Γ ⊢ e ⇐ τ`),
linked by a subsumption rule that wraps `Infer` into `Check`.

The typing context `TyEnv` matches `TypeEnv` in `src/infer/env.rs`: each
binding carries a `TypeScheme` together with a mutability flag.  The
mutability flag is consulted by the `assign` rules and by the
value-restriction side condition on polymorphic property mutation.

The mutable store is reflected by a *store typing* `Δ : Loc → Ty`; the
well-formedness predicate `StoreOK` ties the run-time store to the store
typing.  These are essential for the statement of preservation.

Note: we use `Δ` (not the customary `Σ`) for the store typing because
`Σ` is reserved by Lean 4 for the dependent-pair type former.
-/

import FormalJSTypes.Syntax

namespace FormalJSTypes

/-! ## Type environment -/

/-- Whether a binding can be reassigned. -/
inductive Mutability where
  | mutable
  | immutable
  deriving Repr, DecidableEq

/-- A single binding in the typing context. -/
structure Binding where
  scheme     : TypeScheme
  mutability : Mutability
  deriving Repr

/-- The typing context: an association list from variable names to
    bindings.  Newer bindings shadow older ones. -/
inductive TyEnv where
  | empty : TyEnv
  | cons  : VarName → Binding → TyEnv → TyEnv
  deriving Repr

namespace TyEnv
  def lookup : TyEnv → VarName → Option Binding
    | TyEnv.empty,        _ => none
    | TyEnv.cons y b rest, x =>
      if x = y then some b else rest.lookup x

  def extendMut (Γ : TyEnv) (x : VarName) (s : TypeScheme) : TyEnv :=
    TyEnv.cons x { scheme := s, mutability := Mutability.mutable } Γ

  def extendImm (Γ : TyEnv) (x : VarName) (s : TypeScheme) : TyEnv :=
    TyEnv.cons x { scheme := s, mutability := Mutability.immutable } Γ
end TyEnv

/-! ## Store typing

The store typing assigns a `Ty` to every allocated location.  The
`StoreOK` relation says the run-time heap respects that typing. -/

abbrev StoreTy : Type := List (Loc × Ty)

namespace StoreTy
  def lookup (Δ : StoreTy) (l : Loc) : Option Ty :=
    (Δ.find? (fun p => p.1 = l)).map (·.2)
end StoreTy

/-! ## Generic instantiation

`σ₁ ⊑ σ₂` says `σ₁` is an instance of the more general scheme `σ₂`.
Defining the substitution machinery in full generality is orthogonal to
the soundness scaffolding; the constructor below states it abstractly so
that the `inst` rule of `Infer` can refer to it.  A complete development
would replace `True` below with the existence of a substitution from the
quantified variables of `σ₂` realising `σ₁.body.ty`. -/

inductive InstanceOf : TypeScheme → TypeScheme → Prop where
  /-- Reflexivity. -/
  | refl  (s : TypeScheme) : InstanceOf s s
  /-- Existence of a witnessing substitution; the placeholder is to be
      replaced by genuine substitution algebra in subsequent work. -/
  | inst  (s₁ s₂ : TypeScheme)
          (_h : ∃ _θ : TVar → Option Ty, True)
          : InstanceOf s₁ s₂

/-! ## Type-class entailment

`Entails Q` says the predicate set `Q` is satisfiable by the built-in
instances (`Plus Number`, `Plus String`, `Indexable [α] Number α`, …). -/

inductive Entails : List TypePred → Prop where
  | nil     : Entails []
  | plusNum (q : List TypePred) (h : Entails q)
            : Entails ({ cls := ClassName.plus, args := [Ty.number] } :: q)
  | plusStr (q : List TypePred) (h : Entails q)
            : Entails ({ cls := ClassName.plus, args := [Ty.string] } :: q)
  | idxArr  (α : Ty) (q : List TypePred) (h : Entails q)
            : Entails ({ cls := ClassName.indexable
                       , args := [Ty.array α, Ty.number, α] } :: q)
  | idxMap  (α : Ty) (q : List TypePred) (h : Entails q)
            : Entails ({ cls := ClassName.indexable
                       , args := [Ty.mapTy α, Ty.string, α] } :: q)
  | idxStr  (q : List TypePred) (h : Entails q)
            : Entails ({ cls := ClassName.indexable
                       , args := [Ty.string, Ty.number, Ty.string] } :: q)

/-! ## Bidirectional typing relation

`Infer Γ Δ e τ` reads "in context `Γ` and store typing `Δ`, expression `e`
synthesises type `τ`".  `Check Γ Δ e τ` reads "… checks against the
expected type `τ`".  They are tied together by `subsume` (every
synthesised type is also a checked type) and by `ann` (the type-annotation
form `e /*: τ */` re-enters checking from inference). -/

/-- A scheme is monomorphic iff it has no quantifier and no predicate.
    This corresponds to the `σ ∈ τ` value-restriction premise on the
    `[ASSIGN]` rule in `infernu.md`. -/
def TypeScheme.IsMono (s : TypeScheme) : Prop :=
  s.vars = [] ∧ s.body.preds = []

/-- A type itself is "concrete" (used as a coarse over-approximation of
    monomorphism for property-assignment payloads). -/
def isMonoTy : Ty → Prop
  | _ => True

mutual
  /-- Synthesis: `Γ ⊢ e ⇒ τ`. -/
  inductive Infer : TyEnv → StoreTy → Expr → Ty → Prop where

    /- ### Literals (rule `[LIT]`) -/

    | litNum   (Γ : TyEnv) (Δ : StoreTy) (n : Float)
        : Infer Γ Δ (Expr.lit (Literal.numL n))   Ty.number
    | litStr   (Γ : TyEnv) (Δ : StoreTy) (s : String)
        : Infer Γ Δ (Expr.lit (Literal.strL s))   Ty.string
    | litBool  (Γ : TyEnv) (Δ : StoreTy) (b : Bool)
        : Infer Γ Δ (Expr.lit (Literal.boolL b))  Ty.bool
    | litUndef (Γ : TyEnv) (Δ : StoreTy)
        : Infer Γ Δ (Expr.lit Literal.undefL)     Ty.undef
    | litNull  (Γ : TyEnv) (Δ : StoreTy)
        : Infer Γ Δ (Expr.lit Literal.nullL)      Ty.null
    | litRegex (Γ : TyEnv) (Δ : StoreTy) (p f : String)
        : Infer Γ Δ (Expr.lit (Literal.regexL p f)) Ty.regex

    /- ### Variables (rule `[VAR]` + HMF `[INST]`) -/

    | varIdent
        (Γ : TyEnv) (Δ : StoreTy) (x : VarName)
        (b : Binding) (τ : Ty)
        (hLook : Γ.lookup x = some b)
        (hInst : InstanceOf (TypeScheme.mono τ) b.scheme)
        : Infer Γ Δ (Expr.ident x) τ

    /- ### Functions (rule `[FUN-N]`) -/

    -- Functions: extend Γ with a fresh monomorphic binding per parameter
    -- and type-check the body against the resulting return type.
    | funN
        (Γ : TyEnv) (Δ : StoreTy)
        (params : List VarName) (body : Expr)
        (paramTys : List Ty) (retTy : Ty)
        (hLen : params.length = paramTys.length)
        (hBody : Infer
                  ((params.zip paramTys).foldl
                      (fun Γ' p => Γ'.extendMut p.1 (TypeScheme.mono p.2)) Γ)
                  Δ body retTy)
        : Infer Γ Δ (Expr.func params body)
                    (Ty.func none paramTys retTy)

    /- ### Application (rule `[APP]`) -/

    | app
        (Γ : TyEnv) (Δ : StoreTy)
        (f : Expr) (args : List Expr)
        (paramTys : List Ty) (retTy : Ty)
        (hFun  : Infer Γ Δ f (Ty.func none paramTys retTy))
        (hArgs : CheckArgs Γ Δ args paramTys)
        : Infer Γ Δ (Expr.call f args) retTy

    /- ### Object literals (rule `[OBJ]`) -/

    | obj
        (Γ : TyEnv) (Δ : StoreTy)
        (fields : List (PropName × Expr)) (props : List (PropName × Ty))
        (hMatch : InferProps Γ Δ fields props)
        : Infer Γ Δ (Expr.obj fields) (Ty.row props RowTail.closed)

    /- ### Property access (rule `[PROJ]`) -/

    | proj
        (Γ : TyEnv) (Δ : StoreTy) (e : Expr) (l : PropName)
        (props : List (PropName × Ty)) (tail : RowTail) (τ : Ty)
        (hObj  : Infer Γ Δ e (Ty.row props tail))
        (hHas  : (props.find? (fun q => q.1 = l)).map (·.2) = some τ)
        : Infer Γ Δ (Expr.member e l) τ

    /- ### Assignment (rule `[ASSIGN]`)

        Two flavours: assignment to a (mutable) variable and to an object
        property.  In both cases the value's type must match the target
        and the value-restriction monomorphism premise applies. -/

    | assignVar
        (Γ : TyEnv) (Δ : StoreTy) (x : VarName) (rhs : Expr) (τ : Ty)
        (b : Binding)
        (hLook      : Γ.lookup x = some b)
        (hMut       : b.mutability = Mutability.mutable)
        (hMonoVars  : b.scheme.vars = [])
        (hMonoPreds : b.scheme.body.preds = [])
        (hScm       : b.scheme.body.ty = τ)
        (hRhs       : Check Γ Δ rhs τ)
        : Infer Γ Δ (Expr.assign (Expr.ident x) rhs) τ

    | assignProp
        (Γ : TyEnv) (Δ : StoreTy)
        (eobj : Expr) (l : PropName) (rhs : Expr)
        (props : List (PropName × Ty)) (tail : RowTail) (τ : Ty)
        (hObj  : Infer Γ Δ eobj (Ty.row props tail))
        (hProp : (props.find? (fun q => q.1 = l)).map (·.2) = some τ)
        (hMono : isMonoTy τ)            -- value restriction
        (hRhs  : Check Γ Δ rhs τ)
        : Infer Γ Δ (Expr.assign (Expr.member eobj l) rhs) τ

    /- ### Binary operators with type-class predicates -/

    | binPlus
        (Γ : TyEnv) (Δ : StoreTy) (e₁ e₂ : Expr) (τ : Ty)
        (hCls : Entails [{ cls := ClassName.plus, args := [τ] }])
        (h₁   : Check Γ Δ e₁ τ)
        (h₂   : Check Γ Δ e₂ τ)
        : Infer Γ Δ (Expr.bin BinOp.add e₁ e₂) τ

    /- ### Conditional / `if` -/

    | cond
        (Γ : TyEnv) (Δ : StoreTy) (c t e : Expr) (τ : Ty)
        (hC : Check Γ Δ c Ty.bool)
        (hT : Infer Γ Δ t τ)
        (hE : Check Γ Δ e τ)
        : Infer Γ Δ (Expr.cond c t e) τ

    | ifE
        (Γ : TyEnv) (Δ : StoreTy) (c t e : Expr)
        (hC : Check Γ Δ c Ty.bool)
        (hT : Infer Γ Δ t Ty.undef)
        (hE : Infer Γ Δ e Ty.undef)
        : Infer Γ Δ (Expr.ifE c t e) Ty.undef

    /- ### `let` / `var` (rule `[LET]`) -/

    -- `let` (rule [LET]).  `hWit` witnesses that the chosen scheme is
    -- derivable for `e₁`; the meta-property that `s` is the *most general*
    -- such scheme (`∀τ', Infer e₁ τ' → InstanceOf (mono τ') s`) is *not*
    -- expressible as a constructor premise — it would put `Infer` in a
    -- non-positive position.  It is captured separately as a theorem
    -- statement in the soundness layer.
    | letE
        (Γ : TyEnv) (Δ : StoreTy)
        (x : VarName) (e₁ e₂ : Expr) (s : TypeScheme) (τ₂ : Ty)
        (hWit  : InferAtScheme Γ Δ e₁ s)
        (hBody : Infer (Γ.extendMut x s) Δ e₂ τ₂)
        : Infer Γ Δ (Expr.letE x e₁ e₂) τ₂

    /- ### Sequencing -/

    | seqOne
        (Γ : TyEnv) (Δ : StoreTy) (e : Expr) (τ : Ty)
        (h : Infer Γ Δ e τ)
        : Infer Γ Δ (Expr.seq [e]) τ

    | seqMore
        (Γ : TyEnv) (Δ : StoreTy)
        (e : Expr) (rest : List Expr) (τ₁ τ : Ty)
        (h₁    : Infer Γ Δ e τ₁)
        (hRest : Infer Γ Δ (Expr.seq rest) τ)
        : Infer Γ Δ (Expr.seq (e :: rest)) τ

    -- ### Heap reference.  Once a value has been allocated, the reference
    -- inherits the type recorded in the store typing.  This rule has no
    -- surface syntax – it is consumed by preservation when an `Expr.lit`
    -- is replaced by a heap-reference reduct.
    | locRef
        (Γ : TyEnv) (Δ : StoreTy) (l : Loc) (τ : Ty)
        (hΔ : Δ.lookup l = some τ)
        : Infer Γ Δ (valueToExpr (Value.locV l)) τ

  /-- Checking: `Γ ⊢ e ⇐ τ`.  In this scaffolding, the only structural
      checking rule is *subsumption* — every expression that synthesises a
      type that matches the expected one is accepted.  The implementation
      adds annotation-driven checking on top of this same shape. -/
  inductive Check : TyEnv → StoreTy → Expr → Ty → Prop where
    | subsume
        (Γ : TyEnv) (Δ : StoreTy) (e : Expr) (τ : Ty)
        (h : Infer Γ Δ e τ)
        : Check Γ Δ e τ

    /-- Annotation-driven checking: the implementation uses unification to
        merge the expected type with the synthesised one.  Equality of
        types is the simplest scaffolding of that step. -/
    | ann
        (Γ : TyEnv) (Δ : StoreTy) (e : Expr) (τ τ' : Ty)
        (hEq : τ = τ')
        (h   : Infer Γ Δ e τ')
        : Check Γ Δ e τ

  /-- Pointwise checking of an argument list against a parameter-type
      list (used by the `app` rule).  Defined as an auxiliary inductive
      in the mutual block so its premises may mention `Check`. -/
  inductive CheckArgs : TyEnv → StoreTy → List Expr → List Ty → Prop where
    | nil  (Γ : TyEnv) (Δ : StoreTy) : CheckArgs Γ Δ [] []
    | cons (Γ : TyEnv) (Δ : StoreTy) (e : Expr) (τ : Ty)
           (es : List Expr) (τs : List Ty)
           (hHead : Check Γ Δ e τ)
           (hTail : CheckArgs Γ Δ es τs)
           : CheckArgs Γ Δ (e :: es) (τ :: τs)

  /-- Pointwise inference of an object-literal field list against a
      property-type list (used by the `obj` rule).  Each pair matches in
      both name and value type. -/
  inductive InferProps : TyEnv → StoreTy
                          → List (PropName × Expr) → List (PropName × Ty)
                          → Prop where
    | nil  (Γ : TyEnv) (Δ : StoreTy) : InferProps Γ Δ [] []
    | cons (Γ : TyEnv) (Δ : StoreTy)
           (k : PropName) (e : Expr) (τ : Ty)
           (rest : List (PropName × Expr)) (tprops : List (PropName × Ty))
           (hVal : Infer Γ Δ e τ)
           (hRest : InferProps Γ Δ rest tprops)
           : InferProps Γ Δ ((k, e) :: rest) ((k, τ) :: tprops)

  /-- Witness for the `letE` rule's "exists a derivable type that is an
      instance of the chosen scheme" premise.  Encoded as an inductive
      so we may avoid `∃` in the mutual block. -/
  inductive InferAtScheme : TyEnv → StoreTy → Expr → TypeScheme → Prop where
    | intro (Γ : TyEnv) (Δ : StoreTy)
            (e : Expr) (s : TypeScheme) (τ : Ty)
            (hInfer : Infer Γ Δ e τ)
            (hInst  : InstanceOf (TypeScheme.mono τ) s)
            : InferAtScheme Γ Δ e s
end

/-- `HasType` is the customary alias for the synthesised typing judgement
    used in the statement of soundness. -/
abbrev HasType (Γ : TyEnv) (Δ : StoreTy) (e : Expr) (τ : Ty) : Prop :=
  Infer Γ Δ e τ

/-! ## Bidirectional notation

We expose the conventional `⇒` / `⇐` arrows for inference and checking
respectively.  The notation is purely syntactic sugar; it makes proof
goals significantly easier to read. -/

notation:50 Γ:51 " ;; " Δ:51 " ⊢ " e:51 " ⇒ " τ:50 => Infer Γ Δ e τ
notation:50 Γ:51 " ;; " Δ:51 " ⊢ " e:51 " ⇐ " τ:50 => Check Γ Δ e τ

/-! ## Store well-formedness

For preservation we need to know that the run-time store agrees with the
store typing: every allocated location holds a heap value whose contents
have the type recorded in `Δ`. -/

mutual
  inductive HeapValHasType : StoreTy → HeapVal → Ty → Prop where
    | cell
        (Δ : StoreTy) (v : Value) (τ : Ty)
        (h : HasType TyEnv.empty Δ (valueToExpr v) τ)
        : HeapValHasType Δ (HeapVal.cell v) τ
    | obj
        (Δ : StoreTy) (props : List (PropName × Value))
        (tprops : List (PropName × Ty)) (tail : RowTail)
        (hMatch : ObjStored Δ props tprops)
        : HeapValHasType Δ (HeapVal.objH props) (Ty.row tprops tail)
    | arr
        (Δ : StoreTy) (vs : List Value) (α : Ty)
        (h : ArrStored Δ vs α)
        : HeapValHasType Δ (HeapVal.arrH vs) (Ty.array α)

  /-- Pointwise typing of a stored object record against a row's
      property-type list. -/
  inductive ObjStored : StoreTy → List (PropName × Value)
                              → List (PropName × Ty) → Prop where
    | nil  (Δ : StoreTy) : ObjStored Δ [] []
    | cons (Δ : StoreTy) (k : PropName) (v : Value) (τ : Ty)
           (rest : List (PropName × Value)) (tprops : List (PropName × Ty))
           (hVal  : HasType TyEnv.empty Δ (valueToExpr v) τ)
           (hRest : ObjStored Δ rest tprops)
           : ObjStored Δ ((k, v) :: rest) ((k, τ) :: tprops)

  /-- All elements of a stored array share the array's element type. -/
  inductive ArrStored : StoreTy → List Value → Ty → Prop where
    | nil  (Δ : StoreTy) (α : Ty) : ArrStored Δ [] α
    | cons (Δ : StoreTy) (v : Value) (vs : List Value) (α : Ty)
           (hHead : HasType TyEnv.empty Δ (valueToExpr v) α)
           (hTail : ArrStored Δ vs α)
           : ArrStored Δ (v :: vs) α
end

/-- Store-typing well-formedness: every entry of the store has the type
    recorded in `Δ`, and the two domains coincide. -/
def StoreOK (Δ : StoreTy) (σ : Store) : Prop :=
  (∀ l h, σ.get l = some h →
     ∃ τ, Δ.lookup l = some τ ∧ HeapValHasType Δ h τ)
  ∧ (∀ l τ, Δ.lookup l = some τ →
     ∃ h, σ.get l = some h ∧ HeapValHasType Δ h τ)

end FormalJSTypes
