(** * Rows — structural typing of objects

    This module houses row equivalence, row unification, and the
    associated subject-reduction lemmas.

    PHASE 3: implement Wand-/Rémy-style row unification with
    metavariable counting for termination, prove congruence of row
    equivalence, prove preservation through row-extension and
    row-projection rules. *)

From FormalMinfern Require Import Prelude Syntax.

(** Look up a property in a list of (label, value) pairs.  Used both at
    the type level and for stored object records. *)
Fixpoint assoc {A : Type} (k : prop_name) (xs : list (prop_name * A))
  : option A :=
  match xs with
  | [] => None
  | (k', v) :: rest =>
      if string_eq_dec k k' then Some v else assoc k rest
  end.
