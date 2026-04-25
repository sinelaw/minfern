(** * Classes — type-class entailment

    Encodes the two built-in type classes [Plus] and [Indexable] and
    the entailment relation `Q |- P`.

    PHASE 5: prove coherence (any two valid dictionary-passing
    elaborations are observationally equivalent) and termination of
    instance resolution. *)

From FormalMinfern Require Import Prelude Syntax.

Inductive class_name : Type :=
  | C_plus
  | C_indexable.

(** Resolve the abstract [pred_class : nat] tag of [Syntax.type_pred]
    to a concrete class name.  We leave this as a one-way function for
    now; a refinement would rebuild [Syntax.type_pred] over
    [class_name] directly. *)
Definition class_of_id (n : nat) : option class_name :=
  match n with
  | 0 => Some C_plus
  | 1 => Some C_indexable
  | _ => None
  end.
