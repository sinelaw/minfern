//! Built-in types and type class constraint resolution.
//!
//! This module provides:
//! - Initial type environment with built-in functions
//! - Type class instances for Plus and Indexable
//! - Constraint solving for deferred type class predicates

use crate::error::{MinfernError, TypeError};
use crate::infer::{InferState, TypeEnv};
use crate::lexer::Span;
use crate::types::{ClassName, RowType, TVarName, Type, TypePred, TypeScheme};

/// Create the initial type environment with built-in bindings.
pub fn initial_env() -> TypeEnv {
    let mut env = TypeEnv::empty();

    // console object with log method
    let console_type = Type::object([
        (
            "log",
            Type::simple_func(vec![Type::flex(100)], Type::Undefined),
        ),
        (
            "error",
            Type::simple_func(vec![Type::flex(101)], Type::Undefined),
        ),
        (
            "warn",
            Type::simple_func(vec![Type::flex(102)], Type::Undefined),
        ),
    ]);
    env = env.extend("console".to_string(), TypeScheme::mono(console_type));

    // Math object
    let math_type = Type::object([
        ("PI", Type::Number),
        ("E", Type::Number),
        ("abs", Type::simple_func(vec![Type::Number], Type::Number)),
        ("floor", Type::simple_func(vec![Type::Number], Type::Number)),
        ("ceil", Type::simple_func(vec![Type::Number], Type::Number)),
        ("round", Type::simple_func(vec![Type::Number], Type::Number)),
        ("sqrt", Type::simple_func(vec![Type::Number], Type::Number)),
        (
            "pow",
            Type::simple_func(vec![Type::Number, Type::Number], Type::Number),
        ),
        (
            "min",
            Type::simple_func(vec![Type::Number, Type::Number], Type::Number),
        ),
        (
            "max",
            Type::simple_func(vec![Type::Number, Type::Number], Type::Number),
        ),
        ("random", Type::simple_func(vec![], Type::Number)),
    ]);
    env = env.extend("Math".to_string(), TypeScheme::mono(math_type));

    // JSON object
    let json_type = Type::object([
        (
            "parse",
            Type::simple_func(vec![Type::String], Type::flex(103)),
        ),
        (
            "stringify",
            Type::simple_func(vec![Type::flex(104)], Type::String),
        ),
    ]);
    env = env.extend("JSON".to_string(), TypeScheme::mono(json_type));

    // parseInt, parseFloat
    env = env.extend(
        "parseInt".to_string(),
        TypeScheme::mono(Type::simple_func(vec![Type::String], Type::Number)),
    );
    env = env.extend(
        "parseFloat".to_string(),
        TypeScheme::mono(Type::simple_func(vec![Type::String], Type::Number)),
    );

    // isNaN, isFinite
    env = env.extend(
        "isNaN".to_string(),
        TypeScheme::mono(Type::simple_func(vec![Type::Number], Type::Boolean)),
    );
    env = env.extend(
        "isFinite".to_string(),
        TypeScheme::mono(Type::simple_func(vec![Type::Number], Type::Boolean)),
    );

    // undefined and null are keywords, but we can add them as values too
    env = env.extend("undefined".to_string(), TypeScheme::mono(Type::Undefined));

    // Array constructor (simplified)
    env = env.extend(
        "Array".to_string(),
        TypeScheme::poly(
            vec![TVarName::Flex(200)],
            Type::simple_func(vec![], Type::array(Type::flex(200))),
        ),
    );

    // Object constructor
    env = env.extend(
        "Object".to_string(),
        TypeScheme::mono(Type::simple_func(
            vec![],
            Type::Row(RowType::empty_closed()),
        )),
    );

    // String constructor: converts any value to string
    env = env.extend(
        "String".to_string(),
        TypeScheme::poly(
            vec![TVarName::Flex(201)],
            Type::simple_func(vec![Type::flex(201)], Type::String),
        ),
    );

    // Number constructor: converts any value to number
    env = env.extend(
        "Number".to_string(),
        TypeScheme::poly(
            vec![TVarName::Flex(202)],
            Type::simple_func(vec![Type::flex(202)], Type::Number),
        ),
    );

    // Boolean constructor: converts any value to boolean
    env = env.extend(
        "Boolean".to_string(),
        TypeScheme::poly(
            vec![TVarName::Flex(203)],
            Type::simple_func(vec![Type::flex(203)], Type::Boolean),
        ),
    );

    env
}

impl InferState {
    /// Resolve pending type class constraints.
    /// This should be called after inference to check that all constraints are satisfiable.
    pub fn resolve_constraints(&mut self) -> Result<(), MinfernError> {
        let constraints = std::mem::take(&mut self.pending_constraints);

        for constraint in constraints {
            self.resolve_constraint(&constraint.pred, constraint.span)?;
        }

        Ok(())
    }

    /// Resolve a single type class constraint.
    fn resolve_constraint(&mut self, pred: &TypePred, span: Span) -> Result<(), MinfernError> {
        match pred.class {
            ClassName::Plus => self.resolve_plus(&pred.types[0], span),
            ClassName::Indexable => {
                self.resolve_indexable(&pred.types[0], &pred.types[1], &pred.types[2], span)
            }
        }
    }

    /// Resolve Plus constraint: type must be Number or String.
    fn resolve_plus(&mut self, ty: &Type, span: Span) -> Result<(), MinfernError> {
        let ty = self.apply_subst(ty);

        match &ty {
            Type::Number | Type::String => Ok(()),

            Type::Var(TVarName::Flex(_)) => {
                // Keep the constraint - don't default to Number
                Ok(())
            }

            Type::Var(TVarName::Skolem(_)) => {
                // Skolem variables can't be resolved
                Err(TypeError::ConstraintNotSatisfied {
                    class: "Plus".to_string(),
                    ty: ty.to_string(),
                    span,
                }
                .into())
            }

            _ => Err(TypeError::ConstraintNotSatisfied {
                class: "Plus".to_string(),
                ty: ty.to_string(),
                span,
            }
            .into()),
        }
    }

    /// Resolve Indexable constraint: container[index] = element.
    fn resolve_indexable(
        &mut self,
        container: &Type,
        index: &Type,
        element: &Type,
        span: Span,
    ) -> Result<(), MinfernError> {
        let container = self.apply_subst(container);
        let index = self.apply_subst(index);
        let element = self.apply_subst(element);

        match &container {
            // Array indexing: [T][Number] = T
            Type::Array(elem_ty) => {
                self.unify(span, &index, &Type::Number)?;
                self.unify(span, &element, elem_ty)?;
                Ok(())
            }

            // String indexing: String[Number] = String
            Type::String => {
                self.unify(span, &index, &Type::Number)?;
                self.unify(span, &element, &Type::String)?;
                Ok(())
            }

            // Map indexing: Map<T>[String] = T
            Type::Map(value_ty) => {
                self.unify(span, &index, &Type::String)?;
                self.unify(span, &element, value_ty)?;
                Ok(())
            }

            // Object indexing with string key
            Type::Row(row) => {
                // Check if this row could be array-like (only has array properties like `length`)
                let is_array_like = row.props.keys().all(|k| k.0 == "length");

                if is_array_like && matches!(row.tail, crate::types::RowTail::Open(_)) {
                    // This looks like an array constraint - try array-style indexing
                    // Create a fresh element type and unify the row with Array<elem>
                    let elem_var = self.fresh_type_var();
                    let array_type = Type::array(elem_var.clone());

                    // Try to unify the row with the array's structural representation
                    // This will succeed if the row is compatible with arrays
                    if self.unify(span, &container, &array_type).is_ok() {
                        self.unify(span, &index, &Type::Number)?;
                        self.unify(span, &element, &elem_var)?;
                        return Ok(());
                    }
                }

                // Fall back to object indexing with string key
                self.unify(span, &index, &Type::String)?;

                // The element type is the union of all property types
                // For simplicity, we use a fresh variable
                // In a full implementation, we'd need union types
                Ok(())
            }

            Type::Var(TVarName::Flex(_)) => {
                // Keep the constraint - don't default to Array
                Ok(())
            }

            _ => Err(TypeError::ConstraintNotSatisfied {
                class: "Indexable".to_string(),
                ty: container.to_string(),
                span,
            }
            .into()),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_initial_env() {
        let env = initial_env();
        assert!(env.lookup("console").is_some());
        assert!(env.lookup("Math").is_some());
        assert!(env.lookup("JSON").is_some());
    }

    #[test]
    fn test_resolve_plus_number() {
        let mut state = InferState::new();
        assert!(state.resolve_plus(&Type::Number, Span::new(0, 0)).is_ok());
    }

    #[test]
    fn test_resolve_plus_string() {
        let mut state = InferState::new();
        assert!(state.resolve_plus(&Type::String, Span::new(0, 0)).is_ok());
    }

    #[test]
    fn test_resolve_plus_variable() {
        let mut state = InferState::new();
        let var = Type::flex(0);
        assert!(state.resolve_plus(&var, Span::new(0, 0)).is_ok());
        // Type variable should be kept (not defaulted) to preserve polymorphism
        assert_eq!(state.apply_subst(&var), var);
    }

    #[test]
    fn test_resolve_indexable_array() {
        let mut state = InferState::new();
        let arr = Type::array(Type::Number);
        let elem = Type::flex(0);
        assert!(state
            .resolve_indexable(&arr, &Type::Number, &elem, Span::new(0, 0))
            .is_ok());
        assert_eq!(state.apply_subst(&elem), Type::Number);
    }

    #[test]
    fn test_resolve_indexable_array_like_row() {
        use crate::types::TVarName;

        let mut state = InferState::new();
        // {length: Number | a} should be indexable like an array with Number index
        let row = Type::object_open([("length", Type::Number)], TVarName::Flex(0));
        let index = Type::flex(1);
        let elem = Type::flex(2);

        assert!(state
            .resolve_indexable(&row, &index, &elem, Span::new(0, 0))
            .is_ok());

        // The index should be Number (array-style) not String (object-style)
        assert_eq!(state.apply_subst(&index), Type::Number);
    }

    #[test]
    fn test_resolve_indexable_object_row() {
        let mut state = InferState::new();
        // {foo: String, bar: Number} is NOT array-like, should use string indexing
        let row = Type::object([("foo", Type::String), ("bar", Type::Number)]);
        let index = Type::flex(0);
        let elem = Type::flex(1);

        assert!(state
            .resolve_indexable(&row, &index, &elem, Span::new(0, 0))
            .is_ok());

        // Index should be string for object access
        assert_eq!(state.apply_subst(&index), Type::String);
    }
}
