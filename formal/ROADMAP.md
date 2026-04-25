# Roadmap: completing the FormalJSTypes development

The current Lean 4 code under `formal/` is **scaffolding**: every
`theorem` body is `sorry`, several reduction rules are missing, and
two key pieces of the typing relation (substitution-based generic
instantiation, principal-type witness on `let`) are stubbed.

This document is the punch list for turning the scaffolding into a
real type-soundness proof, and then into a faithful model of
[minfern](https://github.com/sinelaw/minfern)'s implementation.
The phases are ordered by dependency, not difficulty.

---

## Phase 1 — Make the model honest (no proofs yet)

These are gaps in the *definitions* that no proof can paper over.

1. **Thread the environment through `Step`.** `Config` is currently
   `Expr × Store` and `Step.identLookup` existentially quantifies the
   location.  To prove anything, change it to
   `Config := Env × Expr × Store` (or commit to substitution semantics
   and rewrite `Step.callBeta` to actually substitute the args into the
   body).  `bindParams` is already defined but unused.
2. **Fill in the missing reduction rules.**  Currently only ~12 of
   ~30 needed.  Missing:
   * object/array literal evaluation (allocate on the heap),
   * index access,
   * `new`,
   * every unary op,
   * every binary op other than `+`,
   * ternary `cond`,
   * member assignment with computed keys,
   * `while`,
   * `return`.
   Each needs a primitive rule plus a congruence rule.
3. **Substitution on `Ty`.**  Add `Ty.subst : (TVar → Option Ty) → Ty → Ty`
   and replace the placeholder `InstanceOf.inst` (currently
   `_h : ∃ _θ, True`) with a real witness — a substitution that maps
   `s₂.vars` to types yielding `s₁.body.ty` (and discharges
   `s₁.body.preds` from `s₂.body.preds` under that substitution).
4. **Re-introduce `letE.hBound`.**  It was dropped because it put
   `Infer` in negative position.  Recover it as a separate predicate
   `Principal Γ Δ e s` defined positively (e.g. as a function, not an
   inductive), and re-attach as a non-recursive premise.
5. **Row-polymorphic unification + row equality.**  The `proj` rule
   only accepts an exact row match.  Real row poly needs to unify open
   rows; encode either as an inductive `RowEquiv` or as a substitution
   that closes/opens the tail variable.
6. **Equi-recursive type folding/unfolding.**  `RowTail.Recursive`
   exists in the Rust code but is omitted here.  Adding
   `Ty.named (id : TypeId) (args : List Ty)` and an unfold relation is
   a self-contained chunk.
7. **Bidirectional structural checking.**  `Check` currently has only
   `subsume` and `ann`.  Add the standard structural rules:
   * `Check Γ Δ (Expr.func params body) (Ty.func _ paramTys retTy)`
     should descend into the body in checking mode,
   * ditto `cond`, `obj` against an expected row.

---

## Phase 2 — Supporting lemmas (still all `sorry`)

These are the lemmas the soundness theorems will reduce to.

8. **Store-typing arithmetic.**
   * `StoreTy.Extends.cons_fresh` (already stubbed) needs the freshness
     condition.
   * `Store.alloc_extends_storeTy`: allocating extends both store and
     store typing consistently.
   * `Store.update_preserves_storeOK`: writing the same type at a typed
     location preserves `StoreOK`.
9. **Weakening and exchange.**
   * `infer_weaken_store` / `check_weaken_store` (already stubbed) —
     proved by mutual induction over the typing.
   * `infer_weaken_env` (binding more variables in `Γ`) — needed for
     `letE` / `funN`.
10. **Substitution lemma (the big one).**  Either
    * **Term substitution.**
      `Γ, x:τ ⊢ e : σ → Γ ⊢ v : τ → Γ ⊢ e[v/x] : σ` (pure approach),
      **or**
    * **Environment-extension lemma.**
      `Γ ⊢ closV ps b ρ : Func paramTys ret → Γ ⊢ vs : paramTys → after binding params in ρ, body type-checks at ret`
      (closure approach — matches our current `Value.closV`).
11. **Canonical forms.**  Already stubbed for `func`, `number`, `bool`.
    Add `string`, `null`, `undef`, `regex`, plus `canonical_form_row`
    and `canonical_form_array` (these conclude "the value is a `locV`
    whose stored heap value matches the type").
12. **Determinism (optional but nice).**
    `Step (e, σ) c₁ → Step (e, σ) c₂ → c₁ = c₂` — useful for the
    multistep version.

---

## Phase 3 — The two real proofs

13. **`progress`** — induction on the typing derivation.  For each
    `Infer` rule, either the head expression matches `IsValue` or one
    of the `Step` rules fires.  This is where missing reduction rules
    from Phase 1 will bite: every typing rule needs a corresponding
    way to make progress.
14. **`preservation`** — induction on the `Step` derivation, inverting
    the typing at each rule.  Uses every lemma in Phase 2.  The
    β-reduction case is the longest; the heap-mutation cases need
    `Store.update_preserves_storeOK`.
15. **`soundness`** — direct corollary by induction on `Step.Star`
    from `progress` + `preservation`.

---

## Phase 4 — Fidelity to the actual implementation

The current model captures the type system from `infernu.md` plus a
heap.  To tie it back to the Rust code in `src/`:

16. **Match the bidirectional split** to what `src/infer/infer.rs`
    actually does — currently `Infer` corresponds to `infer_*` and
    `Check` to the unification call sites; the boundary should be made
    precise.
17. **`this` resolution** (`Ty.Func.thisTy`) — fully wire into
    `Step.callBeta` and the `member.call` shape: `o.f(args)` binds
    `this := o`.
18. **Rank-1 polymorphism on annotations** — encode the restriction
    noted in `README.md` line 7.
19. **Value restriction for property mutation** — strengthen
    `assignProp.hMono` from the current trivial `isMonoTy` to the real
    "no quantified vars in the assigned value's scheme" condition once
    `TypeScheme.IsMono` is usable outside the mutual block.
20. **An `equiv_to_implementation` theorem.**  The inference algorithm
    in Rust produces type `τ` for `e` ⇔ `Γ ⊢ e ⇒ τ` is derivable.
    This is the "soundness and completeness of the algorithm" theorem
    and is the most ambitious piece — typically a multi-month effort
    even for plain HM.

---

## Phase 5 — Plumbing

21. **Toolchain portability.**  `formal/lean-toolchain` currently says
    `lean4-local` (works only on the sandbox where this scaffolding
    was authored).  Change to `leanprover/lean4:v4.29.1` once you are
    on a machine that can reach `release.lean-lang.org`.
22. **CI.**  Move `formal/.github/workflows/lean_action_ci.yml` to the
    repo-root `.github/workflows/` so GitHub Actions actually picks it
    up; alternatively wire `check_proofs.sh` into the existing Rust
    CI.
23. **Concrete `example` derivations.**  Build a handful of small,
    fully-elaborated `example`s (e.g. `let x = 1 in x : Ty.number`,
    `(λ x. x) : ∀α. α → α`) — they catch broken rules immediately and
    double as documentation.

---

## Realistic effort estimate

| Milestone | Scope | Effort (Lean+PLT-fluent author) |
|---|---|---|
| **Soundness for the core** (Phases 1–3, 5) | literals, vars, functions, let, if, seq, member, simple assignment, `+` | 2–4 weeks |
| **Soundness with row poly + equi-recursive + `this`** (Phase 4 items 17–19) | adds the distinctive minfern features | +2–4 weeks |
| **Algorithm-vs-relation equivalence** (Phase 4 item 20) | implementation tracks the formal model | months |

Phases 1, 2, 3 and 5 are the *"formalization is meaningful"* milestone;
Phase 4 items are the *"formalization tracks the implementation"*
milestone.
