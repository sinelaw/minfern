(** * Soundness — Progress and Preservation

    PHASE 1–6 culmination.  This module ties the operational semantics
    in [Heap.v] to the typing relation in [Inference.v] and states the
    two pillars of soundness.  The actual proofs are populated as
    earlier phases land their lemmas. *)

From FormalMinfern Require Import Prelude Syntax Heap Inference.

(** Sentinel theorem: confirms the build pipeline runs end-to-end. *)
Theorem build_pipeline_ok : True.
Proof. exact I. Qed.
