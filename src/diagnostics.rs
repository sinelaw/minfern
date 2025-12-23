//! Diagnostic printing for minfern errors.

use ariadne::{Color, ColorGenerator, Fmt, Label, Report, ReportKind, Source};

use crate::error::{LexError, MinfernError, ParseError, TypeError};

/// Print a minfern error with colored diagnostics.
pub fn print_error(filename: &str, source: &str, error: &MinfernError) {
    // Handle UnificationError specially since it has multiple labels
    if let MinfernError::Type(TypeError::UnificationError {
        expected,
        found,
        span,
        context,
        expected_origin,
        found_origin,
    }) = error
    {
        let mut colors = ColorGenerator::new();
        let expected_color = colors.next();
        let found_color = colors.next();
        let error_color = Color::Red;

        let msg = format!(
            "Type mismatch: {} '{}', {} '{}'",
            "expected".fg(expected_color),
            expected,
            "found".fg(found_color),
            found
        );

        let mut report = Report::build(ReportKind::Error, (filename, span.start..span.end))
            .with_message(&msg)
            .with_label(
                Label::new((filename, span.start..span.end))
                    .with_message(&msg)
                    .with_color(error_color),
            );

        if let Some(ctx) = context {
            report.add_help(ctx.clone());
        }

        if let Some(origin) = expected_origin {
            let origin_span = origin.span();
            report = report.with_label(
                Label::new((filename, origin_span.start..origin_span.end))
                    .with_message(format!(
                        "{} type: from {}",
                        "Expected".fg(expected_color),
                        origin.description()
                    ))
                    .with_color(expected_color),
            );
        }

        if let Some(origin) = found_origin {
            let origin_span = origin.span();
            report = report.with_label(
                Label::new((filename, origin_span.start..origin_span.end))
                    .with_message(format!(
                        "{} type: from {}",
                        "Found".fg(found_color),
                        origin.description()
                    ))
                    .with_color(found_color),
            );
        }

        // Add footer note explaining why this is an error
        report.add_note(format!(
            "Cannot unify {} '{}' with {} '{}'",
            "expected".fg(expected_color),
            expected,
            "found".fg(found_color),
            found
        ));

        eprintln!(); // Add blank line before error
        report
            .finish()
            .eprint((filename, Source::from(source)))
            .unwrap();
        eprintln!(); // Add blank line after error
        return;
    }

    // Handle all other errors with single note
    let (message, span, note) = match error {
        MinfernError::Lex(e) => match e {
            LexError::UnexpectedCharacter { ch, span } => {
                (format!("Unexpected character: '{}'", ch), *span, None)
            }
            LexError::UnterminatedString { span } => {
                ("Unterminated string literal".to_string(), *span, None)
            }
            LexError::UnterminatedRegex { span } => {
                ("Unterminated regex literal".to_string(), *span, None)
            }
            LexError::InvalidEscapeSequence { span } => {
                ("Invalid escape sequence".to_string(), *span, None)
            }
            LexError::UnterminatedComment { span } => {
                ("Unterminated comment".to_string(), *span, None)
            }
            LexError::InvalidNumber { span } => ("Invalid number literal".to_string(), *span, None),
        },

        MinfernError::Parse(e) => match e {
            ParseError::UnexpectedToken {
                found,
                expected,
                span,
            } => (
                format!("Unexpected token: found '{}', expected {}", found, expected),
                *span,
                None,
            ),
            ParseError::UnexpectedEof { span } => {
                ("Unexpected end of file".to_string(), *span, None)
            }
            ParseError::InvalidAssignmentTarget { span } => {
                ("Invalid assignment target".to_string(), *span, None)
            }
            ParseError::DuplicateProperty { name, span } => {
                (format!("Duplicate property: '{}'", name), *span, None)
            }
            ParseError::InvalidForInTarget { span } => (
                "Invalid left-hand side in for-in/of loop".to_string(),
                *span,
                None,
            ),
            ParseError::BreakOutsideLoop { span } => {
                ("'break' outside of loop or switch".to_string(), *span, None)
            }
            ParseError::ContinueOutsideLoop { span } => {
                ("'continue' outside of loop".to_string(), *span, None)
            }
            ParseError::ReturnOutsideFunction { span } => {
                ("'return' outside of function".to_string(), *span, None)
            }
        },

        MinfernError::Type(e) => match e {
            TypeError::UnificationError { .. } => {
                unreachable!("UnificationError is handled above")
            }
            TypeError::OccursCheck { var, ty, span } => (
                format!("Infinite type: {} occurs in {}", var, ty),
                *span,
                Some("This would create an infinite type".to_string()),
            ),
            TypeError::UndefinedVariable { name, span } => {
                (format!("Undefined variable: '{}'", name), *span, None)
            }
            TypeError::PropertyNotFound {
                prop,
                obj_type,
                span,
            } => (
                format!("Property '{}' not found in type {}", prop, obj_type),
                *span,
                None,
            ),
            TypeError::NotAFunction { ty, span } => {
                (format!("Type '{}' is not a function", ty), *span, None)
            }
            TypeError::ArityMismatch {
                expected,
                found,
                span,
            } => (
                format!(
                    "Arity mismatch: expected {} arguments, found {}",
                    expected, found
                ),
                *span,
                None,
            ),
            TypeError::ConstraintNotSatisfied { class, ty, span } => (
                format!("Type '{}' is not an instance of {}", ty, class),
                *span,
                None,
            ),
            TypeError::TypeAnnotationParse { message, span } => (
                format!("Failed to parse type annotation: {}", message),
                *span,
                None,
            ),
            TypeError::Rank1Restriction { span } => (
                "Rank-1 restriction: type parameters not allowed in nested position".to_string(),
                *span,
                Some(
                    "Type parameters (<T>) can only appear at the outermost level of a type. \
                    Nested type parameters would require Rank-2 or higher polymorphism."
                        .to_string(),
                ),
            ),
            TypeError::EscapedSkolem { span } => (
                "Escaped skolem variable: type is not polymorphic enough".to_string(),
                *span,
                None,
            ),
            TypeError::AmbiguousType { span } => (
                "Ambiguous type: multiple valid instantiations".to_string(),
                *span,
                None,
            ),
            TypeError::AssignmentToConstant { name, span } => (
                format!("Cannot assign to constant '{}'", name),
                *span,
                Some("This binding is immutable and cannot be reassigned".to_string()),
            ),
            TypeError::AssignmentToPolymorphicProperty {
                object,
                property,
                span,
            } => (
                format!(
                    "Cannot assign to polymorphic property '{}' of immutable binding '{}'",
                    property, object
                ),
                *span,
                Some(
                    "Assigning to a polymorphic property would violate type safety. \
                    The property type uses type parameters that could be instantiated differently."
                        .to_string(),
                ),
            ),
        },
    };

    let mut report = Report::build(ReportKind::Error, (filename, span.start..span.end))
        .with_message(&message)
        .with_label(
            Label::new((filename, span.start..span.end))
                .with_message(&message)
                .with_color(Color::Red),
        );

    if let Some(note_text) = note {
        report.add_help(note_text);
    }

    report
        .finish()
        .eprint((filename, Source::from(source)))
        .unwrap();
}
