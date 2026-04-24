//! Metamorphic property tests for the type checker.
//!
//! Idea: when there's no absolute oracle for "is this program well-typed?",
//! we can still test relational properties — pick a transformation `T` that
//! is supposed to preserve well-typedness, and assert
//! `check(p) ≈ check(T(p))` for randomly generated `p`.
//!
//! Each test below picks one such transformation, generates random programs,
//! and checks the relation. If `check` ever disagrees on `p` and `T(p)`,
//! that's either a bug in the type checker or in `T`.
//!
//! Transformations covered:
//! - `prepend_empty`     — prepend `;`
//! - `prepend_dead_var`  — prepend `var <fresh> = 0;`
//! - `intersperse_empty` — insert `;` between every pair of statements
//! - `alpha_rename`      — pick a fresh top-level var, rename consistently
//! - `swap_independent`  — swap two adjacent statements when independent
//!
//! The oracle compares both ok-ness and (when both succeed) the printed
//! program type, normalised by [`PrettyContext`].

use std::collections::HashSet;

use proptest::prelude::*;

use minfern::infer::{InferState, TypeEnv};
use minfern::lexer::Span;
use minfern::parser::ast::*;
use minfern::parser::parse;
use minfern::parser::pretty::print_program;
use minfern::stdlib::initial_env_with_stdlib;
use minfern::types::PrettyContext;

// =========================================================================
// Oracle
// =========================================================================

#[derive(Debug, Clone, PartialEq)]
enum CheckResult {
    /// Type-checker accepted the program; payload is the normalised
    /// printed type of the whole program.
    Ok(String),
    /// Type-checker rejected the program. We don't compare the message —
    /// that's brittle — only that *both* runs failed.
    Err,
}

fn check(program: &Program) -> CheckResult {
    // Use the same env both runs see so any difference is attributable
    // to the program itself.
    let (env, mut state) = match initial_env_with_stdlib() {
        Ok(r) => r,
        Err(_) => return CheckResult::Err,
    };
    run_check(env, &mut state, program)
}

fn run_check(env: TypeEnv, state: &mut InferState, program: &Program) -> CheckResult {
    let result_type = match state.infer_program_with_env(&env, program) {
        Ok((ty, _)) => ty,
        Err(_) => return CheckResult::Err,
    };
    if state.resolve_constraints().is_err() {
        return CheckResult::Err;
    }
    let mut ctx = PrettyContext::new();
    CheckResult::Ok(ctx.format_type(&state.apply_subst(&result_type)))
}

/// A transformed program is "consistent" with the original if both have the
/// same ok/err status and the same normalised type when both succeed.
fn assert_consistent(label: &str, original: &Program, transformed: &Program) {
    let r_orig = check(original);
    let r_trans = check(transformed);
    assert_eq!(
        r_orig,
        r_trans,
        "metamorphic property `{}` violated\n\
         original program:\n{}\n\
         original result: {:?}\n\
         transformed program:\n{}\n\
         transformed result: {:?}",
        label,
        print_program(original),
        r_orig,
        print_program(transformed),
        r_trans,
    );
}

// =========================================================================
// AST helpers
// =========================================================================

fn span() -> Span {
    Span::new(0, 0)
}

fn empty_stmt() -> Stmt {
    Stmt::Empty { span: span() }
}

fn var_stmt(kind: VarKind, name: &str, init: Option<Expr>) -> Stmt {
    Stmt::Var {
        kind,
        declarations: vec![VarDeclarator {
            name: name.to_string(),
            init,
            type_annotation: None,
            kind,
            span: span(),
        }],
        span: span(),
    }
}

fn ident_expr(name: &str) -> Expr {
    Expr::Ident {
        name: name.to_string(),
        span: span(),
    }
}

fn num_lit(n: f64) -> Expr {
    Expr::Lit {
        value: Literal::Number(n),
        span: span(),
    }
}

// =========================================================================
// Free-name collection — used both to choose fresh names and to check that
// the alpha-rename target name doesn't already appear anywhere in the
// program. We collect *all* identifier-shaped strings, not just free
// variables, so we never accidentally collide with a binder either.
// =========================================================================

fn names_in(program: &Program) -> HashSet<String> {
    let mut names = HashSet::new();
    for stmt in &program.statements {
        names_in_stmt(stmt, &mut names);
    }
    names
}

fn names_in_stmt(stmt: &Stmt, out: &mut HashSet<String>) {
    match stmt {
        Stmt::Block { body, .. } => {
            for s in body {
                names_in_stmt(s, out);
            }
        }
        Stmt::Empty { .. } | Stmt::Break { .. } | Stmt::Continue { .. } => {}
        Stmt::Expr { expression, .. } => names_in_expr(expression, out),
        Stmt::Var { declarations, .. } => {
            for d in declarations {
                out.insert(d.name.clone());
                if let Some(e) = &d.init {
                    names_in_expr(e, out);
                }
            }
        }
        Stmt::FunctionDecl {
            name, params, body, ..
        } => {
            out.insert(name.clone());
            for p in params {
                out.insert(p.clone());
            }
            names_in_stmt(body, out);
        }
        Stmt::Return { argument, .. } => {
            if let Some(e) = argument {
                names_in_expr(e, out);
            }
        }
        Stmt::If {
            test,
            consequent,
            alternate,
            ..
        } => {
            names_in_expr(test, out);
            names_in_stmt(consequent, out);
            if let Some(a) = alternate {
                names_in_stmt(a, out);
            }
        }
        Stmt::While { test, body, .. } | Stmt::DoWhile { body, test, .. } => {
            names_in_expr(test, out);
            names_in_stmt(body, out);
        }
        Stmt::Throw { argument, .. } => names_in_expr(argument, out),
        // Other statement kinds can't appear in the strategies below; if
        // they ever do, this function will simply under-collect, which is
        // safe (we'd just risk a name collision and the property test
        // would catch the spurious failure).
        _ => {}
    }
}

fn names_in_expr(expr: &Expr, out: &mut HashSet<String>) {
    match expr {
        Expr::Lit { .. } | Expr::This { .. } | Expr::NewTarget { .. } => {}
        Expr::Ident { name, .. } => {
            out.insert(name.clone());
        }
        Expr::Array { elements, .. } => {
            for e in elements.iter().flatten() {
                names_in_expr(e, out);
            }
        }
        Expr::Object { properties, .. } => {
            for p in properties {
                match p {
                    PropDef::Property { value, .. } => names_in_expr(value, out),
                    PropDef::Method {
                        params, body, ..
                    } => {
                        for param in params {
                            out.insert(param.clone());
                        }
                        names_in_stmt(body, out);
                    }
                    PropDef::Getter { body, .. } => names_in_stmt(body, out),
                    PropDef::Setter { param, body, .. } => {
                        out.insert(param.clone());
                        names_in_stmt(body, out);
                    }
                }
            }
        }
        Expr::Function {
            name, params, body, ..
        } => {
            if let Some(n) = name {
                out.insert(n.clone());
            }
            for p in params {
                out.insert(p.clone());
            }
            names_in_stmt(body, out);
        }
        Expr::Member { object, .. } => names_in_expr(object, out),
        Expr::ComputedMember {
            object, property, ..
        } => {
            names_in_expr(object, out);
            names_in_expr(property, out);
        }
        Expr::Call {
            callee, arguments, ..
        }
        | Expr::New {
            callee, arguments, ..
        } => {
            names_in_expr(callee, out);
            for a in arguments {
                names_in_expr(a, out);
            }
        }
        Expr::Unary { argument, .. } => names_in_expr(argument, out),
        Expr::Binary { left, right, .. } | Expr::Assign { left, right, .. } => {
            names_in_expr(left, out);
            names_in_expr(right, out);
        }
        Expr::Conditional {
            test,
            consequent,
            alternate,
            ..
        } => {
            names_in_expr(test, out);
            names_in_expr(consequent, out);
            names_in_expr(alternate, out);
        }
        Expr::Sequence { expressions, .. } => {
            for e in expressions {
                names_in_expr(e, out);
            }
        }
        Expr::TemplateLiteral { expressions, .. } => {
            for e in expressions {
                names_in_expr(e, out);
            }
        }
    }
}

/// Produce a name that doesn't appear anywhere in `taken`.
fn fresh_name(prefix: &str, taken: &HashSet<String>) -> String {
    let mut i = 0;
    loop {
        let candidate = format!("{}{}", prefix, i);
        if !taken.contains(&candidate) {
            return candidate;
        }
        i += 1;
    }
}

// =========================================================================
// Transformations
// =========================================================================

fn prepend_stmt(program: &Program, extra: Stmt) -> Program {
    let mut statements = Vec::with_capacity(program.statements.len() + 1);
    statements.push(extra);
    statements.extend(program.statements.iter().cloned());
    Program {
        statements,
        span: span(),
    }
}

/// T1: prepend `;` (an empty statement). This shouldn't change anything
/// the type checker observes — empty statements are no-ops.
fn t_prepend_empty(p: &Program) -> Program {
    prepend_stmt(p, empty_stmt())
}

/// T2: prepend `var <fresh> = 0;` where `<fresh>` doesn't shadow anything
/// in `p`. The new binding is unused in the rest of the program, so it
/// should have no effect on inference.
fn t_prepend_dead_var(p: &Program) -> Program {
    let taken = names_in(p);
    let name = fresh_name("__metamorphic_dead_", &taken);
    prepend_stmt(p, var_stmt(VarKind::Var, &name, Some(num_lit(0.0))))
}

/// T3: insert an empty statement between every pair of adjacent statements.
/// Same rationale as T1, but more invasive.
fn t_intersperse_empty(p: &Program) -> Program {
    if p.statements.is_empty() {
        return p.clone();
    }
    let mut out = Vec::with_capacity(p.statements.len() * 2 - 1);
    let mut iter = p.statements.iter().cloned();
    out.push(iter.next().unwrap());
    for s in iter {
        out.push(empty_stmt());
        out.push(s);
    }
    Program {
        statements: out,
        span: span(),
    }
}

/// T4: alpha-renaming. Inject a fresh top-level binding `<x> = 42` plus a
/// usage of it (`var __log_x = <x>;`); then build the *same* program but
/// with the binding and its usage renamed to a different fresh name `<y>`.
/// Any difference in the inferred result is a renaming-sensitivity bug.
///
/// We don't pick from existing bindings in `p` because doing so safely
/// requires scope-aware substitution (function params and nested `var`
/// declarations can shadow). Injecting fresh names sidesteps that without
/// weakening what's actually being tested: the type checker's invariance
/// under choice of identifier name.
fn t_alpha_rename_pair(p: &Program) -> (Program, Program) {
    let taken = names_in(p);
    let x = fresh_name("__metamorphic_x_", &taken);
    let mut taken_with_x = taken.clone();
    taken_with_x.insert(x.clone());
    let log_x = fresh_name("__metamorphic_log_", &taken_with_x);

    let y = fresh_name("__metamorphic_y_", &taken);
    let mut taken_with_y = taken.clone();
    taken_with_y.insert(y.clone());
    let log_y = fresh_name("__metamorphic_log_", &taken_with_y);

    let prog_x = prepend_stmts(
        p,
        vec![
            var_stmt(VarKind::Var, &x, Some(num_lit(42.0))),
            var_stmt(VarKind::Var, &log_x, Some(ident_expr(&x))),
        ],
    );
    let prog_y = prepend_stmts(
        p,
        vec![
            var_stmt(VarKind::Var, &y, Some(num_lit(42.0))),
            var_stmt(VarKind::Var, &log_y, Some(ident_expr(&y))),
        ],
    );

    (prog_x, prog_y)
}

fn prepend_stmts(p: &Program, mut extras: Vec<Stmt>) -> Program {
    extras.extend(p.statements.iter().cloned());
    Program {
        statements: extras,
        span: span(),
    }
}

/// Names a statement *binds* (introduces into the surrounding scope).
fn bound_names_in_stmt(stmt: &Stmt) -> HashSet<String> {
    let mut out = HashSet::new();
    match stmt {
        Stmt::Var { declarations, .. } => {
            for d in declarations {
                out.insert(d.name.clone());
            }
        }
        Stmt::FunctionDecl { name, .. } => {
            out.insert(name.clone());
        }
        _ => {}
    }
    out
}

/// Names a statement *reads* (free identifier references). Approximate:
/// we collect every identifier that appears, including names re-bound in
/// nested scopes. That over-approximates "uses", which only matters for
/// the swap test, and over-approximation is conservative — it can only
/// cause us to refuse to swap when we could have, never to swap when we
/// shouldn't have.
fn referenced_names_in_stmt(stmt: &Stmt) -> HashSet<String> {
    let mut all = HashSet::new();
    names_in_stmt(stmt, &mut all);
    let bound = bound_names_in_stmt(stmt);
    all.difference(&bound).cloned().collect()
}

/// T5: pick the first adjacent pair `(s_i, s_{i+1})` where neither
/// statement references a name the other binds, and swap them. If no
/// such pair exists, return `None` so the caller can `prop_assume!` it
/// away.
///
/// "Independent" here means: `s_i` doesn't read anything `s_{i+1}` binds
/// AND `s_{i+1}` doesn't read anything `s_i` binds. We don't care about
/// shared reads — those don't change behaviour.
///
/// We additionally refuse to swap two statements that bind the same name,
/// because reordering changes which one wins (and `const` would loudly
/// reject duplicates anyway).
///
/// We also refuse to swap any pair that includes the *last* statement,
/// because minfern reports the program type as the type of the last
/// statement (`infer_stmt_list` returns its last element's type). Swapping
/// in the program *type*, even though the program meaning is preserved.
/// Restricting to non-final pairs lets us keep the stronger oracle that
/// also compares the printed type.
fn t_swap_first_independent_pair(p: &Program) -> Option<Program> {
    let n = p.statements.len();
    // Need at least one swappable pair that doesn't include the last stmt.
    if n < 3 {
        return None;
    }
    for i in 0..n - 2 {
        let a = &p.statements[i];
        let b = &p.statements[i + 1];

        let bound_a = bound_names_in_stmt(a);
        let bound_b = bound_names_in_stmt(b);

        // Same-name binders: skip.
        if !bound_a.is_disjoint(&bound_b) {
            continue;
        }

        let refs_a = referenced_names_in_stmt(a);
        let refs_b = referenced_names_in_stmt(b);

        // Independence: neither side reads what the other binds.
        if refs_a.is_disjoint(&bound_b) && refs_b.is_disjoint(&bound_a) {
            let mut statements = p.statements.clone();
            statements.swap(i, i + 1);
            return Some(Program {
                statements,
                span: span(),
            });
        }
    }
    None
}

// =========================================================================
// Strategies — small, biased toward type-checkable programs but the
// metamorphic property holds either way.
// =========================================================================

/// Identifiers chosen from a small fixed pool. Keeping the pool small
/// raises the chance that a randomly-referenced name was actually declared
/// earlier in the program, which means more programs reach the inference
/// stage and exercise something.
fn ident_strategy() -> impl Strategy<Value = String> {
    prop_oneof![
        Just("a".to_string()),
        Just("b".to_string()),
        Just("c".to_string()),
        Just("foo".to_string()),
        Just("bar".to_string()),
    ]
}

fn literal_strategy() -> impl Strategy<Value = Literal> {
    prop_oneof![
        Just(Literal::Null),
        Just(Literal::Undefined),
        Just(Literal::Boolean(true)),
        Just(Literal::Boolean(false)),
        (-100i32..100).prop_map(|n| Literal::Number(n as f64)),
        prop::string::string_regex("[a-zA-Z0-9 ]{0,8}")
            .unwrap()
            .prop_map(Literal::String),
    ]
}

fn binop_strategy() -> impl Strategy<Value = BinOp> {
    prop_oneof![
        Just(BinOp::Add),
        Just(BinOp::Sub),
        Just(BinOp::Mul),
        Just(BinOp::Lt),
        Just(BinOp::EqEqEq),
    ]
}

fn expr_strategy() -> impl Strategy<Value = Expr> {
    let leaf = prop_oneof![
        literal_strategy().prop_map(|v| Expr::Lit {
            value: v,
            span: span(),
        }),
        ident_strategy().prop_map(|n| Expr::Ident {
            name: n,
            span: span(),
        }),
    ];
    leaf.prop_recursive(2, 8, 3, |inner| {
        prop_oneof![
            (binop_strategy(), inner.clone(), inner.clone()).prop_map(|(op, l, r)| {
                Expr::Binary {
                    op,
                    left: Box::new(l),
                    right: Box::new(r),
                    span: span(),
                }
            }),
            prop::collection::vec(inner.clone(), 0..3).prop_map(|elements| Expr::Array {
                elements: elements.into_iter().map(Some).collect(),
                span: span(),
            }),
            inner.clone().prop_map(|arg| Expr::Unary {
                op: UnaryOp::Not,
                argument: Box::new(arg),
                span: span(),
            }),
        ]
    })
}

fn stmt_strategy() -> impl Strategy<Value = Stmt> {
    prop_oneof![
        // Var declaration with init
        (ident_strategy(), expr_strategy()).prop_map(|(name, init)| var_stmt(
            VarKind::Var,
            &name,
            Some(init)
        )),
        // Bare expression statement
        expr_strategy().prop_map(|e| Stmt::Expr {
            expression: e,
            span: span(),
        }),
        // Empty statement
        Just(empty_stmt()),
    ]
}

fn program_strategy() -> impl Strategy<Value = Program> {
    // Range chosen to give the swap test reasonable hit-rate: it needs at
    // least 3 statements (the last is excluded from swapping), so going
    // up to 5 keeps the assume rate manageable.
    prop::collection::vec(stmt_strategy(), 1..6).prop_map(|statements| Program {
        statements,
        span: span(),
    })
}

// =========================================================================
// Sanity check: every program our strategy produces should at minimum
// pretty-print and re-parse without errors. If this ever fails, the
// downstream metamorphic tests would be testing nothing.
// =========================================================================

fn round_trips(p: &Program) -> bool {
    parse(&print_program(p)).is_ok()
}

// =========================================================================
// Property tests
// =========================================================================

proptest! {
    #![proptest_config(ProptestConfig {
        cases: 64,
        // Generated programs are small, so don't burn time on shrinking
        // for ages when something does fail.
        max_shrink_iters: 1024,
        ..ProptestConfig::default()
    })]

    /// Any generated program must pretty-print + reparse, otherwise the
    /// other tests below aren't actually exercising inference.
    #[test]
    fn prop_strategy_round_trips(p in program_strategy()) {
        prop_assert!(round_trips(&p), "generated program failed to round-trip:\n{}", print_program(&p));
    }

    /// Prepending `;` shouldn't change anything.
    #[test]
    fn prop_prepend_empty(p in program_strategy()) {
        prop_assume!(round_trips(&p));
        let q = t_prepend_empty(&p);
        prop_assume!(round_trips(&q));
        assert_consistent("prepend_empty", &p, &q);
    }

    /// Prepending a fresh unused `var <x> = 0;` shouldn't change anything.
    #[test]
    fn prop_prepend_dead_var(p in program_strategy()) {
        prop_assume!(round_trips(&p));
        let q = t_prepend_dead_var(&p);
        prop_assume!(round_trips(&q));
        assert_consistent("prepend_dead_var", &p, &q);
    }

    /// Inserting `;` between every pair of statements shouldn't change
    /// anything.
    #[test]
    fn prop_intersperse_empty(p in program_strategy()) {
        prop_assume!(round_trips(&p));
        let q = t_intersperse_empty(&p);
        prop_assume!(round_trips(&q));
        assert_consistent("intersperse_empty", &p, &q);
    }

    /// Choice of identifier name for an injected fresh top-level binding
    /// must not affect the result of type-checking.
    #[test]
    fn prop_alpha_rename(p in program_strategy()) {
        prop_assume!(round_trips(&p));
        let (prog_x, prog_y) = t_alpha_rename_pair(&p);
        prop_assume!(round_trips(&prog_x));
        prop_assume!(round_trips(&prog_y));
        assert_consistent("alpha_rename", &prog_x, &prog_y);
    }

    /// Swapping two adjacent statements that don't depend on each other
    /// must not change the result.
    #[test]
    fn prop_swap_independent(p in program_strategy()) {
        prop_assume!(round_trips(&p));
        // Skip programs with no swappable pair — there's nothing to test.
        let q = match t_swap_first_independent_pair(&p) {
            Some(q) => q,
            None => return Ok(()),
        };
        prop_assume!(round_trips(&q));
        assert_consistent("swap_independent", &p, &q);
    }
}

// =========================================================================
// Hand-written sanity tests for the transformations themselves. These
// catch silly bugs in the transformations before the random tests get
// blamed for them.
// =========================================================================

#[cfg(test)]
mod transformation_unit_tests {
    use super::*;

    fn p_from(src: &str) -> Program {
        parse(src).expect("source must parse")
    }

    #[test]
    fn fresh_name_avoids_collisions() {
        let p = p_from("var __metamorphic_dead_0 = 1;");
        let q = t_prepend_dead_var(&p);
        let names = names_in(&q);
        // Both __metamorphic_dead_0 (existing) and __metamorphic_dead_1
        // (newly introduced) should now appear.
        assert!(names.contains("__metamorphic_dead_0"));
        assert!(names.contains("__metamorphic_dead_1"));
    }

    #[test]
    fn intersperse_empty_doubles_minus_one_statements() {
        let p = p_from("var a = 1; var b = 2; var c = 3;");
        let q = t_intersperse_empty(&p);
        assert_eq!(q.statements.len(), 5); // 3 + 2 separators
    }

    #[test]
    fn alpha_rename_pair_uses_distinct_fresh_names() {
        let p = p_from("var x = 1;");
        let (a, b) = t_alpha_rename_pair(&p);
        let names_a = names_in(&a);
        let names_b = names_in(&b);
        // Each side must inject a distinct identifier that the other
        // doesn't use.
        let a_only: Vec<_> = names_a.difference(&names_b).collect();
        let b_only: Vec<_> = names_b.difference(&names_a).collect();
        assert!(!a_only.is_empty(), "alpha-rename must inject a name unique to side A");
        assert!(!b_only.is_empty(), "alpha-rename must inject a name unique to side B");
    }

    #[test]
    fn prepend_empty_concrete() {
        let p = p_from("var a = 1;");
        let q = t_prepend_empty(&p);
        assert_consistent("prepend_empty (concrete)", &p, &q);
    }

    #[test]
    fn prepend_dead_var_concrete() {
        let p = p_from("var a = 1; var b = a + 2;");
        let q = t_prepend_dead_var(&p);
        assert_consistent("prepend_dead_var (concrete)", &p, &q);
    }

    #[test]
    fn alpha_rename_concrete() {
        let p = p_from("var a = 1; var b = a + 2;");
        let (x, y) = t_alpha_rename_pair(&p);
        assert_consistent("alpha_rename (concrete)", &x, &y);
    }

    #[test]
    fn swap_finds_independent_pair() {
        // Need at least 3 statements: the last one is excluded from
        // swapping (it determines the program type), so we need a pair
        // among the first n-1.
        let p = p_from("var a = 1; var b = 2; var c = 0;");
        let q = t_swap_first_independent_pair(&p).expect("a and b are independent");
        // After swapping, b's declaration should now come first.
        if let Stmt::Var { declarations, .. } = &q.statements[0] {
            assert_eq!(declarations[0].name, "b");
        } else {
            panic!("expected first stmt to be a var decl");
        }
    }

    #[test]
    fn swap_refuses_two_statement_programs() {
        // Even when the two statements are independent, we don't swap
        // when one of them is the last statement (it carries the program
        // type).
        let p = p_from("var a = 1; var b = 2;");
        assert!(t_swap_first_independent_pair(&p).is_none());
    }

    #[test]
    fn swap_refuses_dependent_pair() {
        let p = p_from("var a = 1; var b = a;");
        // b reads a, so the pair shouldn't be swapped. There's only one
        // pair, and it's not independent, so the function returns None.
        assert!(t_swap_first_independent_pair(&p).is_none());
    }

    #[test]
    fn swap_refuses_same_binder() {
        // Two `var a` declarations bind the same name; reordering them
        // changes which initialiser is the "live" one. Never swap.
        let p = p_from("var a = 1; var a = 2;");
        assert!(t_swap_first_independent_pair(&p).is_none());
    }

    #[test]
    fn swap_concrete() {
        let p = p_from("var a = 1; var b = 2; var c = a + b;");
        // Only the first pair (a, b) is independent; (b, c) reads b.
        let q = t_swap_first_independent_pair(&p).expect("first pair is independent");
        assert_consistent("swap_independent (concrete)", &p, &q);
    }
}
