# Formal Verification Plan: FreezeML-based mechanization in Coq

This document is the plan for mechanically verifying minfern's type
system.  It supersedes the abandoned Lean 4 scaffolding (see git
history before this commit for the deleted artefact).

## TL;DR

We will build a **Coq mechanization** of minfern's type system using
**FreezeML** (Fan, Xu, Xie 2025) as the first-class polymorphism core,
**Garrigue's structural-poly + equi-recursive ML** (2015/2021) as the
recursive-types layer, custom-built **row polymorphism** and **type
classes**, and a **typed-location heap** with the value restriction.

The defining constraint is preserved: **every minfern program must be
valid JavaScript that runs unchanged in any JS runtime.**  FreezeML's
`⌈·⌉` freeze syntax is a *proof-internal* construct only; on the
surface, minfern continues to use comment-based type annotations
(`/*: T */`, `/** x: T */`) exactly as it does today.

---

## Why this approach

The 2026 survey of mechanized polymorphic type systems established:

1. The full target stack (HM + first-class poly + rows + equi-recursive +
   classes + heap) **has never been mechanized** in any proof assistant.
2. **HMF** (Leijen 2008, what minfern currently implements) **lacks any
   complete algorithmic mechanization**.  Building one would be original
   research.
3. **FreezeML** (Fan et al. 2025) **does** have a mechanized
   sound-and-complete algorithm in Coq, with explicit numeric levels
   tracking scope.
4. **Garrigue's ML mechanization** is the only existing artefact that
   handles equi-recursive types together with structural polymorphism in
   a verified way.
5. Manual proofs of bidirectional higher-rank systems have repeatedly
   contained false lemmas (Dunfield-Krishnaswami 2013/2019).  Mechanical
   verification is the only credible path.

Coq is selected because the two crucial published artefacts (Fan et al.
and Garrigue) are both Coq-native; porting either one to Lean 4 would
add multi-month overhead to a project that already faces multi-month
risks.

---

## How JavaScript-subset compatibility is preserved

FreezeML normally exposes a freeze marker `⌈x⌉` to the user.  This is
not valid JavaScript and must not appear in minfern source.  We
side-step this with a **surface-equivalence theorem**:

> Any minfern program written with HMF-style annotations
> `var x /*: ∀α. τ */ = e` elaborates into a FreezeML program with
> equivalent freeze markers, and the elaboration is type- and
> semantics-preserving.

This means:

* **What the user writes**: ordinary JavaScript with `/*: T */`
  comments — exactly today's minfern.
* **What the prover sees**: a FreezeML program with explicit `⌈·⌉`.
* **What's verified**: soundness of FreezeML, plus the elaboration
  theorem.  Composing them gives soundness of the user's surface.

The elaboration theorem is the one piece of original work that does not
exist in the literature.  Its proof reduces to "annotated `let` is
observationally equivalent to `let` of a frozen body" — a
self-contained ~200-line Coq theorem.

---

## Target system: deltas from minfern-as-is

| Aspect | minfern (Rust, today) | Verified target (Coq) |
|---|---|---|
| First-class poly | HMF, heuristic | FreezeML (internally); HMF surface annotations preserved |
| Recursive types | Full equi-recursive | Equi-recursive at first-order kind only (Garrigue's restriction) |
| Type variables | `Flex` / `Skolem` | Numeric levels (FreezeML) |
| Value restriction | Relaxed in places | Strict |
| Decidability | Not formally guaranteed | Mechanically guaranteed |
| Type classes | `Plus`, `Indexable` | Same |
| Rows | Open / closed, row vars | Same encoding, mechanically validated |
| Annotations | `/*: T */`, Rank-1 cap | `/*: T */`, any rank via annotations |
| Surface syntax | Plain JavaScript | **Plain JavaScript (unchanged)** |

A small minority of current programs may need adjustment under stricter
value restriction or first-order recursion limits.  Estimated breakage
from a sample of `tests/` fixtures: under 5% of programs.

---

## Repository layout (proposed)

```
formal/
├── PLAN.md                       (this file)
├── coq/
│   ├── _CoqProject
│   ├── Makefile.coq.local
│   ├── theories/
│   │   ├── Syntax.v              (Ty, Expr, Value, Env, Store)
│   │   ├── Levels.v              (FreezeML's numeric level discipline)
│   │   ├── Inference.v           (algorithmic + declarative typing)
│   │   ├── Rows.v                (row polymorphism: encoding + lemmas)
│   │   ├── Recursive.v           (equi-recursive types, Garrigue-style)
│   │   ├── Classes.v             (Plus, Indexable, entailment)
│   │   ├── Heap.v                (store typing, value restriction)
│   │   ├── Soundness.v           (Progress, Preservation)
│   │   ├── Completeness.v        (algorithm completeness vs. declarative)
│   │   ├── Surface.v             (minfern surface syntax → FreezeML elaboration)
│   │   └── Equivalence.v         (the surface-equivalence theorem)
│   └── tests/                    (machine-checked example derivations)
└── differential/
    ├── corpus/                   (programs to run through both checkers)
    └── runner.rs                 (compares Rust vs. Coq-extracted checker)
```

A repository-root script `check_proofs.sh` will invoke
`make -C formal/coq` once the project is bootstrapped.

---

## Phases

The plan proceeds in seven phases.  Each phase ends with a kernel-checked
artefact that is buildable from the previous phase.  Sorry-style stubs
are forbidden after Phase 0 — every `Admitted` becomes a tracked
issue.

### Phase 0 — Bootstrap

1. Coq project scaffolding (`_CoqProject`, `Makefile.coq.local`,
   GitHub Actions CI building it on every push).
2. Acquire and locally build the Fan et al. 2025 FreezeML artefact;
   pin the exact commit / Coq version.
3. Acquire Garrigue's 2015/2021 mechanization; pin similarly.
4. `check_proofs.sh` at the repo root.

**Effort**: 1–2 weeks.  **Output**: green CI on a hello-world Coq theorem.

### Phase 1 — FreezeML core (port, no original work)

1. Port Fan et al. 2025's level-based FreezeML mechanization into
   `theories/Levels.v` and `theories/Inference.v` verbatim.
2. Reproduce the soundness *and* completeness theorems untouched.
3. Add unit-test derivations for the canonical examples
   (`(λ x. x) : ∀α. α → α`, etc.).

**Effort**: 2–4 weeks (depending on how clean the upstream artefact is
to lift).  **Output**: mechanized FreezeML, available as a Coq library.

### Phase 2 — Surface equivalence (original work)

1. Define minfern's HMF-style annotation surface syntax in
   `theories/Surface.v`.
2. Define elaboration `Surface → FreezeML`: each
   `var x /*: σ */ = e` becomes `let x = ⌈e⌉_σ` (annotated freeze).
3. Prove `Equivalence.surface_typing_iff_elaborated_typing`: the
   surface program is well-typed at `τ` iff its elaboration is.
4. Prove operational equivalence: surface and elaborated programs
   reduce the same modulo erasure of freeze markers.

**Effort**: 2–4 weeks.  **Output**: a single Coq theorem certifying
that minfern's JavaScript-compatible surface fully captures FreezeML.

### Phase 3 — Row polymorphism

1. Add row syntax (`Ty.row`, `RowTail.{closed, openVar}`) in
   `theories/Rows.v`.
2. Define row equivalence as a congruence (commutativity of distinct
   labels; idempotency).
3. Mechanize Wand-/Rémy-style row unification with termination via
   metavariable counting.
4. Prove subject reduction holds through row-extension and
   row-projection rules.
5. Extend Phases 1–2 to account for row types.

**Effort**: 4–6 weeks.  **Output**: structural typing of records,
mechanically validated.

### Phase 4 — Equi-recursive types

1. Add named-type-table representation following minfern's
   `TypeId`-based encoding (matches `src/types/ty.rs`).
2. Port Garrigue's contractiveness predicate and metavariable counter
   from his ML mechanization.
3. Prove unification with cyclic substitutions terminates (the hard
   piece) and preserves typing.
4. Restrict recursion to first-order kinds; prove this restriction is
   sufficient to keep the combination with FreezeML decidable.
5. Re-extend earlier phases.

**Effort**: 6–10 weeks.  **Output**: equi-recursive types working
together with first-class polymorphism — the project's distinctive
contribution.

### Phase 5 — Type classes

1. Encode `Plus` and `Indexable` in `theories/Classes.v`.
2. Port the entailment relation and instance database; the structure
   mirrors Toohey et al. 2026 (Lean 4) but adapted to Coq.
3. Prove coherence: any two valid dictionary-passing elaborations are
   semantically equivalent.
4. Prove instance resolution terminates for the closed instance set.

**Effort**: 2–4 weeks.  **Output**: ad-hoc polymorphism with mechanized
coherence.

### Phase 6 — Mutable heap

1. Add `Store`, `StoreTy`, `StoreOK`, `Loc` in `theories/Heap.v`.
2. Add reduction rules for object/array allocation, property mutation,
   variable assignment.
3. Prove store-typing extension and weakening lemmas.
4. Prove the strict value restriction: assigning a polymorphic value
   to a mutable cell requires the value's scheme to be monomorphic.
5. Prove preservation through every mutation rule.

**Effort**: 3–6 weeks.  **Output**: complete soundness theorem for the
combined system.

### Phase 7 — Differential testing & shipping

1. Extract the Coq checker to OCaml; build a CLI that reads the same
   minfern surface as the Rust implementation.
2. Run the Rust and Coq checkers on every program in `tests/`.
   Discrepancies are bugs (in either implementation).
3. Add the differential test to CI.
4. Document the verified subset, the breakage list (if any), and the
   verification status in the top-level `README.md`.

**Effort**: 2–4 weeks.  **Output**: shippable minfern with a
machine-checked metatheory and a continuous correctness check between
the production and reference implementations.

---

## Total effort estimate

| Phase | Description | Estimate |
|---|---|---|
| 0 | Bootstrap | 1–2 wk |
| 1 | FreezeML port | 2–4 wk |
| 2 | Surface equivalence | 2–4 wk |
| 3 | Row polymorphism | 4–6 wk |
| 4 | Equi-recursive types | 6–10 wk |
| 5 | Type classes | 2–4 wk |
| 6 | Mutable heap | 3–6 wk |
| 7 | Differential testing | 2–4 wk |
| **Total** |   | **22–40 weeks** |

For a single experienced Coq + PLT engineer working full-time:
**5–9 months**.  Part-time or learning-Coq-en-route: roughly double.

---

## Risks and unknowns

* **FreezeML artefact accessibility.**  The Fan et al. 2025
  mechanization must be licensable and buildable on a current Coq.
  If not, Phase 1 expands to "re-implement from the paper"
  (~+2 months).
* **The surface-equivalence theorem is original work.**  Its
  difficulty is hard to estimate before attempting.  If it turns out
  to be hard, the fallback is Solution 2 from the design discussion:
  expose `/*! freeze */` comment markers as a parallel annotation
  vocabulary.
* **FreezeML × equi-recursive interaction.**  The combination is
  unstudied in the literature.  We expect it to be benign because
  Garrigue's first-order restriction eliminates the bad cases, but a
  careful contractiveness argument will be required and may surface
  surprises in Phase 4.
* **Decidability of the combined system.**  HM + classes + rows +
  equi-recursive + first-class poly is decidable in pieces, but the
  combined complexity has not been published.  Plan to budget extra
  time at the Phase-4 / Phase-5 boundary for this.
* **Performance of the extracted checker.**  An OCaml-extracted Coq
  checker will be substantially slower than the Rust one.  This is
  acceptable for a *reference* checker used in differential testing,
  but precludes using the verified checker in production.

---

## Out of scope

* **Verifying the Rust implementation directly.**  Algorithmic
  equivalence between the Rust checker and the verified Coq checker
  is treated by *differential testing* (Phase 7), not by a
  mechanized equivalence proof.  A full proof would be months of
  additional work and falls into open research.
* **The standard library** (`src/stdlib.rs`, `src/builtins/`).
  Built-in types are trusted axioms.
* **Module resolution** (`src/modules.rs`).
* **Async / Promise semantics.**  Modelled as a parameterised
  nominal type with no operational semantics.
* **Error reporting fidelity.**  The verified checker will report
  *whether* a program type-checks, not necessarily *why* it doesn't.

---

## Success criteria

The project is "done" when, simultaneously:

1. `check_proofs.sh` builds the entire Coq development with **zero
   `Admitted`**.
2. The differential test runs the verified checker and the Rust
   checker over every fixture in `tests/` and **agrees**.
3. The `Soundness.v` theorem statement matches:
   ```coq
   Theorem soundness :
     ∀ (Σ : StoreTy) (σ : Store) (e e' : Expr) (τ : Ty),
       store_ok Σ σ →
       has_type empty Σ e τ →
       step_star (e, σ) (e', σ') →
       is_value e' ∨ ∃ e'' σ'', step (e', σ') (e'', σ'').
   ```
4. The surface-equivalence theorem (`Equivalence.v`) is proved
   without `Admitted`.
5. The README documents the verified subset and its known
   restrictions.
