(** * Recursive — equi-recursive types via a named-type table

    Following minfern's Rust implementation (`TypeId` + the table in
    `src/infer/state.rs`), an equi-recursive type is a [ty_named id args]
    whose meaning is [body[args/params]] where [body] comes from a
    table.  Two types are equi-recursively equivalent when they have
    the same infinite unfolding under that table.

    PHASE 4: port Garrigue 2015/2021's contractiveness predicate and
    metavariable-counting termination argument; prove decidability of
    equivalence; restrict recursion to first-order kinds. *)

From FormalMinfern Require Import Prelude Syntax.

(** Type-definition table entry: parameters and body. *)
Record type_def : Type :=
  { td_id     : nat
  ; td_params : list tvar
  ; td_body   : ty }.

Definition type_table : Type := list type_def.

Definition empty_table : type_table := [].
