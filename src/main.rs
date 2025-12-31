use anyhow::{bail, Result};
use bpaf::Bpaf;
use rs_commentary::output::{render_source, render_source_semantic, RenderConfig, RenderStyle};
use std::fs;
use std::path::PathBuf;
use std::str::FromStr;
use tracing_subscriber::{fmt, prelude::*, EnvFilter};

/// Output style for annotations
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Style {
    Inline,
    Columnar,
    Grouped,
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

impl Default for Style {
    fn default() -> Self {
        Style::Diagnostic
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
#[bpaf(options, version)]
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
        /// Output style
        #[bpaf(short, long, argument("STYLE"), fallback(Style::default()))]
        style: Style,

        /// Show all variables including Copy types
        #[bpaf(short, long)]
        all: bool,

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
    let cmd = cmd().run();

    match cmd {
        Cmd::Annotate { file, style, all, serve, port } => {
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

            let config = RenderConfig::new().with_filter_copy(!all);

            let output = if let Some(ref path) = file_path {
                render_source_semantic(path, &source, render_style, config.clone())
                    .unwrap_or_else(|| render_source(&source, render_style, config))
            } else {
                render_source(&source, render_style, config)
            };

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
