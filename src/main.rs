use anyhow::{bail, Result};
use clap::{Parser, Subcommand, ValueEnum};
use rs_commentary::output::{render_source, render_source_semantic, RenderConfig, RenderStyle};
use std::fs;
use std::path::PathBuf;
use tracing_subscriber::{fmt, prelude::*, EnvFilter};

/// Rust ownership state visualizer - annotates code with move/borrow/ownership state
#[derive(Parser)]
#[command(name = "rs-commentary")]
#[command(version, about, long_about = None)]
#[command(after_help = "NOTATION:
    ●●●  Owned mutable (O+R+W)     ○●○  Shared borrow (R only)
    ●●○  Owned immutable (O+R)     ○●●  Mutable borrow (R+W)
    ●○○  Frozen by &mut (O only)   var† Variable dropped")]
struct Cli {
    #[command(subcommand)]
    command: Option<Commands>,
}

#[derive(Subcommand)]
enum Commands {
    /// Annotate source with ownership state
    #[command(visible_alias = "a")]
    Annotate {
        /// Input file (reads from stdin if not provided)
        file: Option<PathBuf>,

        /// Output style
        #[arg(short, long, value_enum, default_value = "diagnostic")]
        style: Style,

        /// Show all variables including Copy types
        #[arg(long)]
        all: bool,

        /// Serve HTML output via local HTTP server (only with --style=html)
        #[arg(long)]
        serve: bool,

        /// Port for HTTP server (default: 8080)
        #[arg(long, default_value = "8080")]
        port: u16,
    },

    /// Start LSP server (default when no command given)
    Lsp,
}

#[derive(Copy, Clone, PartialEq, Eq, ValueEnum)]
enum Style {
    // Valid Rust output
    /// Comments at end of each line
    Inline,
    /// Fixed columns per variable
    Columnar,
    /// Horizontal rules between state changes
    Grouped,

    // Rich text output
    /// Rustc-style with line numbers and underlines
    Diagnostic,
    /// Tutorial-style: main{mut x, r(&x)}
    #[value(name = "set-notation")]
    SetNotation,
    /// Box-drawing brackets showing borrow lifetimes
    #[value(name = "vertical-spans")]
    VerticalSpans,
    /// Interactive HTML visualization
    Html,
    /// Ownership state + rust-analyzer errors
    Validated,
}

impl From<Style> for RenderStyle {
    fn from(s: Style) -> Self {
        match s {
            Style::Inline => RenderStyle::Inline,
            Style::Columnar => RenderStyle::Columnar,
            Style::Grouped => RenderStyle::Grouped,
            Style::Diagnostic => RenderStyle::Diagnostic,
            Style::SetNotation => RenderStyle::SetNotation,
            Style::VerticalSpans => RenderStyle::VerticalSpans,
            Style::Html => RenderStyle::Html,
            Style::Validated => RenderStyle::Validated,
        }
    }
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    match cli.command {
        Some(Commands::Annotate { file, style, all, serve, port }) => {
            // Validate --serve is only used with HTML
            if serve && style != Style::Html {
                bail!("--serve can only be used with --style=html");
            }

            let render_style: RenderStyle = style.into();

            let (source, file_path) = match file {
                Some(path) => (fs::read_to_string(&path)?, Some(path)),
                None => {
                    use std::io::Read;
                    let mut source = String::new();
                    std::io::stdin().read_to_string(&mut source)?;
                    (source, None)
                }
            };

            let config = RenderConfig::new()
                .with_filter_copy(!all);

            // Try semantic analysis first for accurate NLL drop detection
            let output = if let Some(ref path) = file_path {
                render_source_semantic(path, &source, render_style, config.clone())
                    .unwrap_or_else(|| {
                        // Fall back to AST-only analysis
                        render_source(&source, render_style, config)
                    })
            } else {
                // No file path - use AST-only analysis
                render_source(&source, render_style, config)
            };

            if serve {
                serve_html(&output, port)?;
            } else {
                print!("{}", output);
            }
        }

        Some(Commands::Lsp) | None => {
            let filter = EnvFilter::try_from_default_env()
                .unwrap_or_else(|_| EnvFilter::new("info"));
            tracing_subscriber::registry()
                .with(fmt::layer().with_writer(std::io::stderr))
                .with(filter)
                .init();

            tracing::info!("Starting rs-commentary LSP server");
            rs_commentary::lsp::run_server()?;
        }
    }

    Ok(())
}

fn serve_html(html: &str, port: u16) -> Result<()> {
    let addr = format!("127.0.0.1:{}", port);
    let server = tiny_http::Server::http(&addr)
        .map_err(|e| anyhow::anyhow!("Failed to start server: {}", e))?;

    let url = format!("http://{}", addr);
    eprintln!("Serving at {} (Ctrl+C to stop)", url);

    // Open browser
    if let Err(e) = open::that(&url) {
        eprintln!("Could not open browser: {}", e);
    }

    let html = html.to_string();
    for request in server.incoming_requests() {
        let response = tiny_http::Response::from_string(&html)
            .with_header(
                tiny_http::Header::from_bytes(&b"Content-Type"[..], &b"text/html; charset=utf-8"[..])
                    .unwrap(),
            );
        let _ = request.respond(response);
    }

    Ok(())
}
