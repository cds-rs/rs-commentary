use anyhow::{bail, Result};
use bpaf::Bpaf;
use rs_commentary::output::{render_source_semantic, RenderConfig, RenderStyle};
use std::fs;
use std::path::PathBuf;
use std::str::FromStr;
use tracing_subscriber::{fmt, prelude::*, EnvFilter};

/// Output style for annotations
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[derive(Default)]
enum Style {
    Inline,
    Columnar,
    Grouped,
    #[default]
    Diagnostic,
    SetNotation,
    VerticalSpans,
    Html,
    Validated,
}

impl FromStr for Style {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        match s.to_lowercase().as_str() {
            "inline" => Ok(Style::Inline),
            "columnar" => Ok(Style::Columnar),
            "grouped" => Ok(Style::Grouped),
            "diagnostic" => Ok(Style::Diagnostic),
            "set-notation" => Ok(Style::SetNotation),
            "vertical-spans" => Ok(Style::VerticalSpans),
            "html" => Ok(Style::Html),
            "validated" => Ok(Style::Validated),
            _ => Err(format!(
                "unknown style '{}'; expected: inline, columnar, grouped, diagnostic, \
                 set-notation, vertical-spans, html, validated",
                s
            )),
        }
    }
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

#[derive(Debug, Clone, Bpaf)]
#[bpaf(options, version, fallback_to_usage)]
/// Rust ownership state visualizer: annotates code with move/borrow/ownership state
///
/// NOTATION:
///     ●●●  Owned mutable (O+R+W)     ○●○  Shared borrow (R only)
///     ●●○  Owned immutable (O+R)     ○●●  Mutable borrow (R+W)
///     ●○○  Frozen by &mut (O only)   var† Variable dropped
enum Cmd {
    /// Annotate source with ownership state
    #[bpaf(command)]
    Annotate {
        /// Output style [inline, columnar, grouped, diagnostic (default), set-notation, vertical-spans, html, validated]
        #[bpaf(short, long, argument("STYLE"), fallback(Style::default()))]
        style: Style,

        /// Show all variables including Copy types
        #[bpaf(short, long)]
        all: bool,

        /// Strip comments from source before rendering
        #[bpaf(long)]
        strip_comments: bool,

        /// Serve HTML output via local HTTP server (only with --style=html)
        #[bpaf(long)]
        serve: bool,

        /// Port for HTTP server
        #[bpaf(long, argument("PORT"), fallback(8080u16))]
        port: u16,

        /// Input file (reads from stdin if not provided)
        #[bpaf(positional("FILE"))]
        file: Option<PathBuf>,
    },

    /// Start LSP server
    #[bpaf(command)]
    Lsp,
}

fn main() -> Result<()> {
    use bpaf::Args;

    let cmd = match cmd().run_inner(Args::current_args()) {
        Ok(cmd) => cmd,
        Err(bpaf::ParseFailure::Stdout(msg, _)) => {
            print!("{}", msg);
            std::process::exit(0);
        }
        Err(bpaf::ParseFailure::Completion(c)) => {
            print!("{}", c);
            std::process::exit(0);
        }
        Err(bpaf::ParseFailure::Stderr(_)) => {
            // Show help on any parse error
            if let Err(bpaf::ParseFailure::Stdout(help, _)) =
                cmd().run_inner(Args::from(&["--help"]))
            {
                print!("{}", help);
            }
            std::process::exit(1);
        }
    };

    match cmd {
        Cmd::Annotate { file, style, all, strip_comments, serve, port } => {
            if serve && style != Style::Html {
                bail!("--serve can only be used with --style=html");
            }

            let render_style: RenderStyle = style.into();

            let file_path = file.ok_or_else(|| {
                anyhow::anyhow!("file path required (stdin not supported - must be in a cargo project)")
            })?;
            let source = fs::read_to_string(&file_path)?;

            let config = RenderConfig::new()
                .with_filter_copy(!all)
                .with_strip_comments(strip_comments);

            let output = render_source_semantic(&file_path, &source, render_style, config)
                .ok_or_else(|| {
                    anyhow::anyhow!("failed to load cargo project (file must be in a cargo workspace)")
                })?;

            if serve {
                serve_html(&output, port)?;
            } else {
                print!("{}", output);
            }
        }

        Cmd::Lsp => {
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

    if let Err(e) = open::that(&url) {
        eprintln!("Could not open browser: {}", e);
    }

    let html = html.to_string();
    for request in server.incoming_requests() {
        let response = tiny_http::Response::from_string(&html).with_header(
            tiny_http::Header::from_bytes(&b"Content-Type"[..], &b"text/html; charset=utf-8"[..])
                .unwrap(),
        );
        let _ = request.respond(response);
    }

    Ok(())
}
