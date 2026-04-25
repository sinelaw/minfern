(** * Heap — store, store typing, and well-formedness

    PHASE 6: define [store], [store_ty], the well-formedness predicate
    [store_ok], and the value restriction.  Prove store-typing
    extension lemmas and preservation through every mutation rule. *)

From FormalMinfern Require Import Prelude Syntax.

(** Heap-resident values: variable cells, object records, arrays. *)
Inductive heap_val : Type :=
  | hv_cell : value -> heap_val
  | hv_obj  : list (prop_name * value) -> heap_val
  | hv_arr  : list value -> heap_val.

Definition store    : Type := list (loc * heap_val).
Definition store_ty : Type := list (loc * ty).

(** Look up a heap value by location. *)
Fixpoint store_get (s : store) (l : loc) : option heap_val :=
  match s with
  | [] => None
  | (l', h) :: rest => if Nat.eq_dec l l' then Some h else store_get rest l
  end.

Fixpoint store_ty_get (Sigma : store_ty) (l : loc) : option ty :=
  match Sigma with
  | [] => None
  | (l', t) :: rest => if Nat.eq_dec l l' then Some t else store_ty_get rest l
  end.

(** A configuration is an expression paired with the current store. *)
Definition config : Type := expr * store.
