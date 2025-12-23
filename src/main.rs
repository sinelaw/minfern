//! Minfern CLI: Type inference for mquickjs JavaScript subset.

use std::env;
use std::fs;
use std::io::{self, Read};
use std::process::ExitCode;

use minfern::builtins::initial_env;
use minfern::diagnostics::print_error;
use minfern::error::MinfernError;
use minfern::infer::{decorate_with_types, InferState};
use minfern::lexer::{Scanner, Token};
use minfern::parser::{pretty::print_program, Parser};
use minfern::types::PrettyContext;

fn main() -> ExitCode {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        eprintln!("Usage: minfern <file.js> | minfern -");
        eprintln!("       minfern --help");
        return ExitCode::from(1);
    }

    if args[1] == "--help" || args[1] == "-h" {
        print_help();
        return ExitCode::SUCCESS;
    }

    if args[1] == "--version" || args[1] == "-V" {
        println!("minfern {}", env!("CARGO_PKG_VERSION"));
        return ExitCode::SUCCESS;
    }

    let (source, filename) = if args[1] == "-" {
        // Read from stdin
        let mut source = String::new();
        if let Err(e) = io::stdin().read_to_string(&mut source) {
            eprintln!("Error reading stdin: {}", e);
            return ExitCode::from(1);
        }
        (source, "<stdin>".to_string())
    } else {
        // Read from file
        let filename = &args[1];
        match fs::read_to_string(filename) {
            Ok(source) => (source, filename.clone()),
            Err(e) => {
                eprintln!("Error reading file '{}': {}", filename, e);
                return ExitCode::from(1);
            }
        }
    };

    match run_inference(&source, &filename) {
        Ok(()) => ExitCode::SUCCESS,
        Err(errors) => {
            for error in errors {
                print_error(&filename, &source, &error);
            }
            ExitCode::from(1)
        }
    }
}

fn print_help() {
    println!(
        r#"minfern - HMF-based type inference for mquickjs

USAGE:
    minfern <file.js>    Check types in a JavaScript file
    minfern -            Read from stdin

OPTIONS:
    -h, --help           Print help information
    -V, --version        Print version information

DESCRIPTION:
    Minfern performs static type inference on mquickjs JavaScript code.
    It features:

    - Row polymorphism for structural typing of objects
    - Equi-recursive types for self-referential structures
    - Type classes (Plus, Indexable) for overloaded operators
    - HMF-based inference with first-class polymorphism
    - Type annotations in comments using /*: Type */ syntax

EXAMPLES:
    minfern example.js          Check types in example.js
    echo "var x = 1" | minfern - Check types from stdin

TYPE ANNOTATIONS:
    You can add type annotations in specially formatted comments:

    /*: Number */ var x = 42;
    function add(a, b) /*: (Number, Number) -> Number */ {{
        return a + b;
    }}

AUTHOR:
    (c) Noam Lewis
"#
    );
}

fn run_inference(source: &str, _filename: &str) -> Result<(), Vec<MinfernError>> {
    let mut errors = Vec::new();

    // Lexing
    let mut scanner = Scanner::new(source);
    let mut tokens = Vec::new();

    loop {
        match scanner.next_token() {
            Ok(tok) => {
                let is_eof = matches!(tok.value, Token::Eof);
                tokens.push(tok);
                if is_eof {
                    break;
                }
            }
            Err(e) => {
                errors.push(e);
                break;
            }
        }
    }

    if !errors.is_empty() {
        return Err(errors);
    }

    // Parsing
    let type_annotations = scanner.type_annotations().to_vec();
    let mut parser = Parser::new(tokens, type_annotations);

    let program = match parser.parse_program() {
        Ok(program) => program,
        Err(e) => {
            errors.push(e);
            return Err(errors);
        }
    };

    // Type inference
    let mut state = InferState::new();
    let env = initial_env();

    match state.infer_program_with_env(&env, &program) {
        Ok((result_type, final_env)) => {
            // Resolve type class constraints
            if let Err(e) = state.resolve_constraints() {
                errors.push(e);
                return Err(errors);
            }

            // Print the program type
            let mut ctx = PrettyContext::new();
            let final_type = state.apply_subst(&result_type);
            println!("// Program type: {}", ctx.format_type(&final_type));
            println!();

            // Decorate the AST with inferred types and print it
            let decorated = decorate_with_types(&program, &final_env, &state);
            print!("{}", print_program(&decorated));

            Ok(())
        }
        Err(e) => {
            errors.push(e);
            Err(errors)
        }
    }
}
