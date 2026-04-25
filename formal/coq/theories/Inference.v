(** * Inference — declarative + algorithmic typing

    PHASE 1 / PHASE 2 / PHASE 7:
      - port Fan et al. 2025's level-based bidirectional algorithm,
      - extend with rows, recursive types, and classes,
      - re-prove soundness and completeness of the algorithm against
        the declarative system. *)

From FormalMinfern Require Import Prelude Syntax Levels Heap.

(** Typing context: variable -> scheme + mutability. *)
Inductive mutability : Type := Mut | Imm.

Record binding : Type :=
  { bnd_scheme  : type_scheme
  ; bnd_mutable : mutability }.

Definition ty_env : Type := list (var_name * binding).

Fixpoint ty_env_get (Gamma : ty_env) (x : var_name) : option binding :=
  match Gamma with
  | [] => None
  | (y, b) :: rest =>
      if string_eq_dec x y then Some b else ty_env_get rest x
  end.
