(** * Surface — minfern's JavaScript surface syntax

    The surface is *strictly* what one can write in valid JavaScript:
    no FreezeML freeze marker, no inline kind annotations.  First-class
    polymorphism is requested by way of comment-based type
    annotations of the form [/*: forall a. tau */] which are parsed
    upstream and arrive here as an attached scheme.

    PHASE 2: define this surface AST and the elaboration into the core
    FreezeML representation in [Inference.v]. *)

From FormalMinfern Require Import Prelude Syntax.

(** A surface expression is just an [expr] paired with optional type
    annotations on let-bindings and function arguments.  We model the
    annotation positions explicitly so the elaboration in
    [Equivalence.v] can target each one. *)
Inductive surface : Type :=
  | s_raw      : expr -> surface
  | s_let_ann  : var_name -> type_scheme -> surface -> surface -> surface
  | s_fun_ann  : list (var_name * option ty) -> option ty -> surface -> surface.
