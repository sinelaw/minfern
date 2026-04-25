(** * Equivalence — surface ≃ FreezeML

    The key novel theorem of the project: minfern's HMF-style
    annotated surface elaborates into FreezeML's freeze-marked core,
    typing- and semantics-preservingly.  Composing this theorem with
    Fan et al. 2025's mechanized FreezeML soundness gives soundness of
    the surface language minfern users actually write.

    PHASE 2 deliverable.  Statement (target):

    {|
      Theorem surface_typing_iff_elaborated_typing :
        forall (s : surface) (t : ty),
          surface_well_typed empty empty s t
        <-> core_well_typed   empty empty (elaborate s) t.
    |}

    Currently empty; the elaboration relation will be added once
    [Inference.v] exposes a [core_well_typed] predicate. *)

From FormalMinfern Require Import Prelude Syntax Surface Inference.
