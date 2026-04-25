(** * Prelude

    Common utilities and type aliases shared by the FormalMinfern
    development.  Kept deliberately small; substantive lemmas belong in
    feature-specific modules. *)

From Coq Require Export List String Arith Bool.
From Coq Require Export Lia.

Export ListNotations.

(** A heap location is a natural-number index into the store. *)
Definition loc : Type := nat.

(** A property name (for object types and record values). *)
Definition prop_name : Type := string.

(** A variable name. *)
Definition var_name : Type := string.

(** A type-variable identifier — used both for inference variables and
    for the level-stamped variables of FreezeML. *)
Definition tvar_id : Type := nat.

(** Decidable equality on strings (lifted from the standard library so
    every subsequent module can rely on `string_dec`). *)
Definition string_eq_dec : forall (a b : string), {a = b} + {a <> b} := string_dec.

(** Sanity check: the prelude builds. *)
Lemma prelude_loaded : True.
Proof. exact I. Qed.
