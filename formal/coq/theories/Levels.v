(** * Levels — FreezeML-style numeric level discipline

    This module will host the FreezeML level mechanism (Fan, Xu, Xie
    2025).  Every type variable is stamped with a numeric level; the
    level of a context grows on entering a [let] / function body, and
    generalisation only abstracts over variables strictly above the
    enclosing level.

    In the published artefact this is the lever that gives
    sound-and-complete first-class polymorphism.

    PHASE 1: port the level discipline + soundness lemmas from
    Fan et al. 2025.  This file currently exposes only the type
    of a level so other modules can refer to it. *)

From FormalMinfern Require Import Prelude.

Definition level : Type := nat.

(** The "outermost" level — used as the initial level when typing a
    closed program. *)
Definition top_level : level := 0.

(** Increment a level on entering a binder. *)
Definition succ_level (l : level) : level := S l.
