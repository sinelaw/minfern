//! Minfern CLI: Type inference for mquickjs JavaScript subset.

use std::env;
use std::fs;
use std::io::{self, Read};
use std::process::ExitCode;

use minfern::diagnostics::print_error;
use minfern::error::MinfernError;
use minfern::infer::{decorate_with_types, InferState, TypeEnv};
use minfern::lexer::{Scanner, Token};
use minfern::parser::{pretty::print_program, Parser};
use minfern::stdlib::{initial_env_with_stdlib, load_lib};
use minfern::types::PrettyContext;

struct Args {
    input: Option<String>,
    /// Extra user-supplied declaration files (paths).
    extra_libs: Vec<String>,
    /// Skip the built-in stdlib (core.d.js, dom.d.js).
    no_stdlib: bool,
}

fn parse_args(raw: Vec<String>) -> Result<Args, String> {
    let mut input = None;
    let mut extra_libs = Vec::new();
    let mut no_stdlib = false;

    let mut iter = raw.into_iter().skip(1);
    while let Some(arg) = iter.next() {
        match arg.as_str() {
            "--help" | "-h" => {
                print_help();
                std::process::exit(0);
            }
            "--version" | "-V" => {
                println!("minfern {}", env!("CARGO_PKG_VERSION"));
                std::process::exit(0);
            }
            "--lib" => {
                let path = iter
                    .next()
                    .ok_or_else(|| "--lib requires a path argument".to_string())?;
                extra_libs.push(path);
            }
            "--no-stdlib" => {
                no_stdlib = true;
            }
            _ if arg.starts_with("--lib=") => {
                extra_libs.push(arg["--lib=".len()..].to_string());
            }
            _ if arg.starts_with("--") => {
                return Err(format!("unknown option: {}", arg));
            }
            _ => {
                if input.is_some() {
                    return Err(format!("unexpected extra argument: {}", arg));
                }
                input = Some(arg);
            }
        }
    }

    Ok(Args {
        input,
        extra_libs,
        no_stdlib,
    })
}

fn main() -> ExitCode {
    let raw: Vec<String> = env::args().collect();

    let args = match parse_args(raw) {
        Ok(a) => a,
        Err(e) => {
            eprintln!("error: {}", e);
            eprintln!();
            eprintln!("run 'minfern --help' for usage.");
            return ExitCode::from(2);
        }
    };

    let input = match args.input {
        Some(s) => s,
        None => {
            eprintln!("Usage: minfern <file.js> | minfern -");
            eprintln!("       minfern --help");
            return ExitCode::from(1);
        }
    };

    let (source, filename) = if input == "-" {
        let mut source = String::new();
        if let Err(e) = io::stdin().read_to_string(&mut source) {
            eprintln!("Error reading stdin: {}", e);
            return ExitCode::from(1);
        }
        (source, "<stdin>".to_string())
    } else {
        match fs::read_to_string(&input) {
            Ok(source) => (source, input.clone()),
            Err(e) => {
                eprintln!("Error reading file '{}': {}", input, e);
                return ExitCode::from(1);
            }
        }
    };

    // Build the initial env. Default: load embedded stdlib (core + dom). The
    // same InferState is threaded through so fresh type var IDs never clash
    // between the libs and the user program.
    let (env, state) = if args.no_stdlib {
        (minfern::builtins::initial_env(), InferState::new())
    } else {
        match initial_env_with_stdlib() {
            Ok(r) => r,
            Err(e) => {
                eprintln!("error loading built-in stdlib: {}", e);
                return ExitCode::from(1);
            }
        }
    };

    // Load any extra user-supplied lib files.
    let (env, mut state) = match load_extra_libs(env, state, &args.extra_libs) {
        Ok(r) => r,
        Err(code) => return code,
    };

    match run_inference(&mut state, env, &source, &filename) {
        Ok(()) => ExitCode::SUCCESS,
        Err(errors) => {
            for error in errors {
                print_error(&filename, &source, &error);
            }
            ExitCode::from(1)
        }
    }
}

fn load_extra_libs(
    mut env: TypeEnv,
    mut state: InferState,
    paths: &[String],
) -> Result<(TypeEnv, InferState), ExitCode> {
    for path in paths {
        let source = match fs::read_to_string(path) {
            Ok(s) => s,
            Err(e) => {
                eprintln!("error reading --lib file '{}': {}", path, e);
                return Err(ExitCode::from(1));
            }
        };
        match load_lib(&mut state, env.clone(), &source) {
            Ok(new_env) => env = new_env,
            Err(e) => {
                print_error(path, &source, &e);
                return Err(ExitCode::from(1));
            }
        }
    }
    Ok((env, state))
}

fn print_help() {
    println!(
        r#"minfern - HMF-based type inference for mquickjs

USAGE:
    minfern [OPTIONS] <file.js>
    minfern [OPTIONS] -

OPTIONS:
    --lib <path>         Load an additional declaration file (can be repeated)
    --no-stdlib          Skip the embedded core and DOM declarations
    -h, --help           Print help information
    -V, --version        Print version information

DESCRIPTION:
    Minfern performs static type inference on mquickjs JavaScript code.
    It features:

    - Row polymorphism for structural typing of objects
    - Equi-recursive types for self-referential structures
    - Type classes (Plus, Indexable) for overloaded operators
    - HMF-based inference with first-class polymorphism
    - Type annotations in doc comments using /** var x: T */ syntax

    By default, minfern auto-loads the embedded core and DOM declarations
    (stdlib/core.d.js and stdlib/dom.d.js). Pass --no-stdlib to disable
    them, or --lib <path> to load additional user-supplied declarations
    (e.g. for a third-party library).

EXAMPLES:
    minfern example.js                         Check example.js
    minfern --lib types/lodash.d.js app.js     Add a lib before checking
    minfern --no-stdlib small.js               Check without any libs
    echo "var x = 1" | minfern -               Check from stdin

AUTHOR:
    (c) Noam Lewis
"#
    );
}

fn run_inference(
    state: &mut InferState,
    env: TypeEnv,
    source: &str,
    filename: &str,
) -> Result<(), Vec<MinfernError>> {
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

    // Resolve any `import "./foo.js"` statements relative to the file's
    // parent directory before inferring the program itself. For stdin
    // (filename == "<stdin>") we skip resolution since there's no path.
    let env = if filename != "<stdin>" {
        let base_dir = std::path::Path::new(filename)
            .parent()
            .map(|p| p.to_path_buf())
            .unwrap_or_else(|| std::path::PathBuf::from("."));
        let mut visiting = std::collections::HashSet::new();
        match minfern::modules::resolve_imports(
            state,
            env,
            &program,
            &base_dir,
            &mut visiting,
        ) {
            Ok(e) => e,
            Err(e) => {
                errors.push(e);
                return Err(errors);
            }
        }
    } else {
        env
    };

    // Type inference
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
            let decorated = decorate_with_types(&program, &final_env, state);
            print!("{}", print_program(&decorated));

            Ok(())
        }
        Err(e) => {
            errors.push(e);
            Err(errors)
        }
    }
}
