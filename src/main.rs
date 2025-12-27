use anyhow::Result;
use rs_commentary::output::{render_source, render_source_semantic, RenderConfig, RenderStyle};
use std::env;
use std::fs;
use std::path::Path;
use tracing_subscriber::{fmt, prelude::*, EnvFilter};

fn main() -> Result<()> {
    let args: Vec<String> = env::args().collect();

    if args.len() > 1 {
        match args[1].as_str() {
            "annotate" | "a" => {
                let no_filter = args.iter().any(|s| s == "--all");
                let style = parse_style_arg(&args).unwrap_or(RenderStyle::Diagnostic);

                let is_flag = |s: &str| s.starts_with('-');
                let (source, file_path) = if args.len() >= 3 && !is_flag(&args[2]) {
                    (fs::read_to_string(&args[2])?, Some(args[2].clone()))
                } else if args.len() == 2 || args[2..].iter().all(|s| is_flag(s)) {
                    use std::io::Read;
                    let mut source = String::new();
                    std::io::stdin().read_to_string(&mut source)?;
                    (source, None)
                } else {
                    eprintln!("Usage: rs-commentary annotate [file.rs] [--style <STYLE>]");
                    std::process::exit(1);
                };

                let config = RenderConfig::new().with_filter_copy(!no_filter);

                // Use semantic analysis for Validated style when file path is available
                let output = if style.requires_semantic() {
                    if let Some(ref path) = file_path {
                        render_source_semantic(Path::new(path), &source, style, config.clone())
                            .unwrap_or_else(|| {
                                eprintln!("Warning: semantic analysis failed, falling back to AST-only");
                                render_source(&source, style, config)
                            })
                    } else {
                        eprintln!("Warning: semantic analysis requires file path, falling back to AST-only");
                        render_source(&source, style, config)
                    }
                } else {
                    render_source(&source, style, config)
                };

                print!("{}", output);
                return Ok(());
            }
            "help" | "-h" | "--help" => {
                print_help();
                return Ok(());
            }
            _ => {
                eprintln!("Unknown command: {}", args[1]);
                print_help();
                std::process::exit(1);
            }
        }
    }

    // Default: run LSP server
    let filter = EnvFilter::try_from_default_env().unwrap_or_else(|_| EnvFilter::new("info"));
    tracing_subscriber::registry()
        .with(fmt::layer().with_writer(std::io::stderr))
        .with(filter)
        .init();

    tracing::info!("Starting rs-commentary LSP server");
    rs_commentary::lsp::run_server()
}

fn parse_style_arg(args: &[String]) -> Option<RenderStyle> {
    for (i, arg) in args.iter().enumerate() {
        if let Some(s) = arg.strip_prefix("--style=") {
            return RenderStyle::from_str(s);
        }
        if (arg == "--style" || arg == "-s") && args.get(i + 1).is_some() {
            return RenderStyle::from_str(&args[i + 1]);
        }
    }
    None
}

fn print_help() {
    eprintln!(
        r#"rs-commentary - Rust ownership state visualizer

USAGE:
    rs-commentary                     Start LSP server
    rs-commentary annotate <file.rs>  Annotate source with ownership state

OPTIONS:
    -h, --help             Show this help
    -s, --style <STYLE>    Output style (default: diagnostic)
    --all                  Show all variables (include Copy types)

STYLES (valid Rust output):
    inline      Comments at end of each line
    columnar    Fixed columns per variable
    grouped     Horizontal rules between state changes

STYLES (rich text):
    diagnostic  Rustc-style with line numbers and underlines (default)
    set-notation  Tutorial-style: main{{mut x, r(&x)}}
    vertical-spans  Box-drawing brackets showing borrow lifetimes
    html        Interactive HTML visualization
    validated   Ownership state + rust-analyzer errors (requires cargo project)

EXAMPLES:
    rs-commentary annotate src/main.rs
    rs-commentary annotate src/main.rs --style=inline
    rs-commentary annotate src/main.rs --style=validated
    cat file.rs | rs-commentary annotate --style grouped

NOTATION:
    ●●●  Owned mutable (O+R+W)     ○●○  Shared borrow (R only)
    ●●○  Owned immutable (O+R)     ○●●  Mutable borrow (R+W)
    ●○○  Frozen by &mut (O only)   var† Variable dropped
"#
    );
}
