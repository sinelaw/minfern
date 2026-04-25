/-
# Type soundness scaffolding (Progress and Preservation)

The two pillars of type soundness for a small-step language are stated
here in the standard form:

* **Progress**: a closed, well-typed expression is either a value or
  there exists a configuration it can step to.
* **Preservation**: stepping from a well-typed configuration produces
  another well-typed configuration of the same type, possibly under an
  extended store typing (allocations may grow `Δ`).

The proofs are left as `sorry` – they are the proof obligations that this
formalisation enables.  Standard supporting lemmas (substitution lemma,
canonical forms, store extension monotonicity, weakening) are stated as
auxiliary `theorem`s with `sorry` bodies so the soundness theorems can be
reduced to them in subsequent work.

We use `Δ` for store typings throughout (rather than the customary `Σ`,
which is a reserved Lean 4 token for the dependent-pair type former).
-/

import FormalJSTypes.Syntax
import FormalJSTypes.Semantics
import FormalJSTypes.Typing

namespace FormalJSTypes

/-! ## Store-typing extension

A run step may allocate fresh heap cells.  We say `Δ' ⊒ Δ` (read "Δ'
extends Δ") if every binding in Δ also appears in Δ'. -/

def StoreTy.Extends (Δ' Δ : StoreTy) : Prop :=
  ∀ l τ, Δ.lookup l = some τ → Δ'.lookup l = some τ

infix:50 " ⊒ " => StoreTy.Extends

/-! ## Structural properties of store-typing extension -/

theorem StoreTy.Extends.refl (Δ : StoreTy) : Δ ⊒ Δ := by
  intro _ _ h; exact h

theorem StoreTy.Extends.trans
    {Δ₁ Δ₂ Δ₃ : StoreTy} (h₁₂ : Δ₂ ⊒ Δ₁) (h₂₃ : Δ₃ ⊒ Δ₂) : Δ₃ ⊒ Δ₁ := by
  intro l τ h; exact h₂₃ l τ (h₁₂ l τ h)

/-- Allocating a fresh location into the store typing yields an extension.
    Stated as a `sorry`-stub so the precise freshness condition (the inverse
    of `StoreTy.lookup` returning `none`) can be discharged in subsequent
    work alongside the real preservation proof. -/
theorem StoreTy.Extends.cons_fresh
    {Δ : StoreTy} {l : Loc} {τ : Ty}
    (hFresh : Δ.lookup l = none)
    : ((l, τ) :: Δ) ⊒ Δ := by
  sorry

/-! ## Auxiliary lemmas (proof targets)

These standard lemmas underpin the Progress / Preservation proofs.  Each
is stated as a `theorem` with a `sorry`. -/

/-- Weakening of the synthesis (`Infer`) judgement under store-typing
    extension.  Proved by mutual induction with `check_weaken_store`. -/
theorem infer_weaken_store
    {Γ : TyEnv} {Δ Δ' : StoreTy} {e : Expr} {τ : Ty}
    (hExt : Δ' ⊒ Δ)
    (h    : Infer Γ Δ e τ)
    : Infer Γ Δ' e τ := by
  sorry

/-- Weakening of the checking (`Check`) judgement under store-typing
    extension.  Proved by mutual induction with `infer_weaken_store`. -/
theorem check_weaken_store
    {Γ : TyEnv} {Δ Δ' : StoreTy} {e : Expr} {τ : Ty}
    (hExt : Δ' ⊒ Δ)
    (h    : Check Γ Δ e τ)
    : Check Γ Δ' e τ := by
  sorry

/-- Convenience corollary used in the soundness statement. -/
theorem hasType_weaken_store
    {Γ : TyEnv} {Δ Δ' : StoreTy} {e : Expr} {τ : Ty}
    (hExt : Δ' ⊒ Δ)
    (h    : HasType Γ Δ e τ)
    : HasType Γ Δ' e τ :=
  infer_weaken_store hExt h

/-- Canonical forms: a value of function type is a function literal. -/
theorem canonical_form_func
    {Δ : StoreTy} {v : Expr} {paramTys : List Ty} {retTy : Ty}
    (hVal : IsValue v)
    (hTy  : HasType TyEnv.empty Δ v (Ty.func none paramTys retTy))
    : ∃ ps b, v = Expr.func ps b ∧ ps.length = paramTys.length := by
  sorry

/-- Canonical forms: a value of `Number` type is a number literal. -/
theorem canonical_form_number
    {Δ : StoreTy} {v : Expr}
    (hVal : IsValue v)
    (hTy  : HasType TyEnv.empty Δ v Ty.number)
    : ∃ n : Float, v = Expr.lit (Literal.numL n) := by
  sorry

/-- Canonical forms: a value of `Bool` type is a boolean literal. -/
theorem canonical_form_bool
    {Δ : StoreTy} {v : Expr}
    (hVal : IsValue v)
    (hTy  : HasType TyEnv.empty Δ v Ty.bool)
    : ∃ b : Bool, v = Expr.lit (Literal.boolL b) := by
  sorry

/-! ## Progress

For any closed, well-typed expression either evaluation has terminated
(the expression is a `Value`) or there is a configuration it can step to.
"Closed" is captured by the empty typing context `TyEnv.empty`; "well-
typed" by the existence of a derivation `HasType TyEnv.empty Δ e τ`
together with a well-formed store. -/

theorem progress
    {Δ : StoreTy} {σ : Store} {e : Expr} {τ : Ty}
    (hStore : StoreOK Δ σ)
    (hType  : HasType TyEnv.empty Δ e τ)
    : IsValue e ∨ ∃ e' σ', Step (e, σ) (e', σ') := by
  sorry

/-! ## Preservation

If the configuration `(e, σ)` is well-typed at type `τ` (under store
typing `Δ`) and steps to `(e', σ')`, then there is an extended store
typing `Δ' ⊒ Δ` such that `(e', σ')` is well-typed at the same type. -/

theorem preservation
    {Δ : StoreTy} {σ σ' : Store} {e e' : Expr} {τ : Ty}
    (hStore : StoreOK Δ σ)
    (hType  : HasType TyEnv.empty Δ e τ)
    (hStep  : Step (e, σ) (e', σ'))
    : ∃ Δ', Δ' ⊒ Δ ∧ StoreOK Δ' σ' ∧ HasType TyEnv.empty Δ' e' τ := by
  sorry

/-! ## Soundness

Multistep version: well-typed programs can only reduce to values or to
configurations that can themselves take a further step.  Stated here as a
direct corollary skeleton. -/

theorem soundness
    {Δ : StoreTy} {σ σ' : Store} {e e' : Expr} {τ : Ty}
    (hStore : StoreOK Δ σ)
    (hType  : HasType TyEnv.empty Δ e τ)
    (hStar  : Step.Star (e, σ) (e', σ'))
    : IsValue e' ∨ ∃ e'' σ'', Step (e', σ') (e'', σ'') := by
  sorry

end FormalJSTypes
