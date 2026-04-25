(** * Syntax — surface and core

    This module fixes the abstract syntax of:

      - [ty]      — the surface types of the language (HMF + rows + μ).
      - [expr]    — the AST of expressions.
      - [literal] — primitive values appearing in expressions.

    The constructors here mirror the variants in `src/types/ty.rs` and
    `src/parser/ast.rs` of the Rust implementation.  Some Rust variants
    are intentionally collapsed into a single Coq constructor when the
    operational difference is irrelevant to typing soundness (e.g.
    `let` and `var` are both modelled by [tm_let]). *)

From FormalMinfern Require Import Prelude.

(** ** Type variables and row tails *)

(** Distinguishes flexible (unification) variables from rigid skolems.
    Mirrors `TVarKind` in the Rust code. *)
Inductive tvar_kind : Type :=
  | Flex
  | Skolem.

Record tvar : Type :=
  { tv_kind : tvar_kind
  ; tv_id   : tvar_id }.

Inductive row_tail : Type :=
  | RTclosed  : row_tail
  | RTopen    : tvar_id -> row_tail.

(** ** Types *)

(** The single-constructor inductive [ty] mirrors `enum Type` in the
    Rust code.  Equi-recursive types are introduced via the `ty_named`
    constructor + a separate type-definition table (modelled in
    [Recursive.v]).  This file fixes only the syntax. *)
Inductive ty : Type :=
  | ty_number    : ty
  | ty_string    : ty
  | ty_bool      : ty
  | ty_undef     : ty
  | ty_null      : ty
  | ty_regex     : ty
  | ty_tvar      : tvar -> ty
  (* (this_ty, params, ret).  this_ty = None is a static function. *)
  | ty_func      : option ty -> list ty -> ty -> ty
  | ty_array     : ty -> ty
  | ty_map       : ty -> ty
  | ty_promise   : ty -> ty
  (* row encoding: list of (label, type) plus a tail. *)
  | ty_row       : list (prop_name * ty) -> row_tail -> ty
  (* equi-recursive named reference: id + type arguments. *)
  | ty_named     : nat -> list ty -> ty.

(** ** Type schemes (HMF + classes)

    A scheme [ts] is intended to be read as `forall vars. preds => body`.
    The predicate set is realised in [Classes.v]. *)
Record type_pred : Type :=
  { pred_class : nat (* class identifier; resolved in Classes.v *)
  ; pred_args  : list ty }.

Record qual_ty : Type :=
  { qt_preds : list type_pred
  ; qt_ty    : ty }.

Record type_scheme : Type :=
  { ts_vars : list tvar
  ; ts_body : qual_ty }.

(** Construct a monomorphic scheme. *)
Definition mono_scheme (t : ty) : type_scheme :=
  {| ts_vars := []
   ; ts_body := {| qt_preds := []; qt_ty := t |} |}.

(** ** Operators *)

Inductive bin_op : Type :=
  | Op_add | Op_sub | Op_mul | Op_div | Op_mod | Op_pow
  | Op_lt  | Op_gt  | Op_leq | Op_geq | Op_eqeq | Op_neqeq
  | Op_eqeqeq | Op_neqeqeq
  | Op_and | Op_or
  | Op_band | Op_bor | Op_bxor | Op_shl | Op_shr | Op_ushr
  | Op_in | Op_instof.

Inductive un_op : Type :=
  | Op_neg | Op_pos | Op_not | Op_bitnot
  | Op_typeof | Op_void | Op_delete | Op_await
  | Op_preinc | Op_predec | Op_postinc | Op_postdec.

(** ** Literals *)

(** Numbers are modelled abstractly to dodge the IEEE-754 minefield.
    A complete development would refine this to a verified [Float]
    library; for soundness scaffolding, an opaque token suffices. *)
Inductive literal : Type :=
  | Lnum   : nat -> literal           (* placeholder for f64 *)
  | Lstr   : string -> literal
  | Lbool  : bool -> literal
  | Lundef : literal
  | Lnull  : literal
  | Lregex : string -> string -> literal.

(** ** Expressions

    Statements are folded into [expr] via dedicated constructors so the
    small-step relation can be a single inductive over [expr * store]
    configurations. *)
Inductive expr : Type :=
  | e_lit     : literal -> expr
  | e_var     : var_name -> expr
  | e_this    : expr
  | e_arr     : list expr -> expr
  | e_obj     : list (prop_name * expr) -> expr
  | e_func    : list var_name -> expr -> expr
  | e_member  : expr -> prop_name -> expr
  | e_index   : expr -> expr -> expr
  | e_call    : expr -> list expr -> expr
  | e_new     : expr -> list expr -> expr
  | e_unary   : un_op -> expr -> expr
  | e_bin     : bin_op -> expr -> expr -> expr
  | e_assign  : expr -> expr -> expr
  | e_cond    : expr -> expr -> expr -> expr
  | e_seq     : list expr -> expr
  | e_let     : var_name -> expr -> expr -> expr
  | e_if      : expr -> expr -> expr -> expr
  | e_while   : expr -> expr -> expr
  | e_ret     : expr -> expr.

(** ** Values

    Run-time values include heap locations and closures.  Closures
    capture the environment in which they were created. *)
Inductive value : Type :=
  | v_num   : nat -> value
  | v_str   : string -> value
  | v_bool  : bool -> value
  | v_undef : value
  | v_null  : value
  | v_regex : string -> string -> value
  | v_loc   : loc -> value
  | v_clos  : list var_name -> expr -> list (var_name * loc) -> value.

(** Identify the syntactic subset of [expr] that is in normal form. *)
Inductive is_value : expr -> Prop :=
  | iv_lit  : forall l, is_value (e_lit l)
  | iv_func : forall ps b, is_value (e_func ps b).

(** Inject a [value] into [expr].  Heap locations and closures use the
    [e_lit] / [e_func] tags as their syntactic carriers. *)
Definition value_to_expr (v : value) : expr :=
  match v with
  | v_num n     => e_lit (Lnum n)
  | v_str s     => e_lit (Lstr s)
  | v_bool b    => e_lit (Lbool b)
  | v_undef     => e_lit Lundef
  | v_null      => e_lit Lnull
  | v_regex p f => e_lit (Lregex p f)
  | v_loc _     => e_lit Lundef        (* heap-only; surfaces as undef *)
  | v_clos ps b _ => e_func ps b
  end.
