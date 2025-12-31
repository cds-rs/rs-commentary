//! Core types for the renderer framework.

/// Renderer output category - determines syntactic validity.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RenderCategory {
    /// Output is syntactically valid Rust (uses `//` comments only).
    /// Can be compiled, run through rustfmt, etc.
    ValidRust,
    /// Output uses rich typography (box drawing, diagrams).
    /// More visually powerful but not valid Rust syntax.
    RichText,
}

/// Available rendering styles.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum RenderStyle {
    // ─────────────────────────────────────────────────────────────────────
    // Valid Rust renderers (output can be compiled)
    // ─────────────────────────────────────────────────────────────────────
    /// Inline comments at end of each line: `// x ●●○`
    #[default]
    Inline,
    /// Fixed columns for each variable with NLL transitions
    Columnar,
    /// Grouped transitions with horizontal rules and blank lines
    Grouped,

    // ─────────────────────────────────────────────────────────────────────
    // Rich text renderers (more typographical freedom)
    // ─────────────────────────────────────────────────────────────────────
    /// Rustc-style diagnostic format with line numbers and underlines
    Diagnostic,
    /// Set notation: `main{mut x, r(&x)}` - matches tutorial style
    SetNotation,
    /// Vertical borrow spans with box-drawing brackets
    VerticalSpans,
    /// Interactive HTML with Aquascope-inspired features
    Html,
    /// Validated output with rust-analyzer errors integrated
    Validated,
}

impl RenderStyle {
    pub fn parse(s: &str) -> Option<Self> {
        match s.to_lowercase().as_str() {
            "inline" => Some(Self::Inline),
            "columnar" | "columns" => Some(Self::Columnar),
            "grouped" | "transitions" => Some(Self::Grouped),
            "diagnostic" | "diag" | "rustc" => Some(Self::Diagnostic),
            "set" | "set-notation" | "sets" => Some(Self::SetNotation),
            "vertical" | "spans" | "vertical-spans" => Some(Self::VerticalSpans),
            "html" => Some(Self::Html),
            "validated" | "semantic" | "ra" => Some(Self::Validated),
            _ => None,
        }
    }

    /// Returns the output category for this render style.
    pub fn category(&self) -> RenderCategory {
        match self {
            // Valid Rust - can be compiled
            Self::Inline | Self::Columnar | Self::Grouped => RenderCategory::ValidRust,
            // Rich text - more typographical freedom
            Self::Diagnostic
            | Self::SetNotation
            | Self::VerticalSpans
            | Self::Html
            | Self::Validated => RenderCategory::RichText,
        }
    }

    /// Returns true if output is syntactically valid Rust.
    pub fn is_valid_rust(&self) -> bool {
        self.category() == RenderCategory::ValidRust
    }

    /// Returns true if this style requires semantic analysis (rust-analyzer).
    pub fn requires_semantic(&self) -> bool {
        matches!(self, Self::Validated)
    }

    pub fn all() -> &'static [(&'static str, &'static str, RenderCategory)] {
        &[
            ("inline", "Comments at end of line", RenderCategory::ValidRust),
            ("columnar", "Fixed columns with NLL transitions", RenderCategory::ValidRust),
            ("grouped", "Horizontal rules with blank lines", RenderCategory::ValidRust),
            ("diagnostic", "Rustc-style with line numbers and underlines", RenderCategory::RichText),
            ("set-notation", "Set notation: main{mut x, r(&x)}", RenderCategory::RichText),
            ("vertical-spans", "Vertical borrow spans with brackets", RenderCategory::RichText),
            ("html", "Interactive HTML visualization", RenderCategory::RichText),
            ("validated", "Ownership state + rust-analyzer errors", RenderCategory::RichText),
        ]
    }
}

/// Configuration options for rendering.
#[derive(Debug, Clone, Default)]
pub struct RenderConfig {
    /// Filter out Copy types (bool, i32, etc.)
    pub filter_copy_types: bool,
    /// Comment column position
    pub comment_column: usize,
    /// Show verbose explanations
    pub verbose: bool,
}

impl RenderConfig {
    pub fn new() -> Self {
        Self {
            filter_copy_types: true,
            comment_column: 58,
            verbose: false,
        }
    }

    pub fn with_filter_copy(mut self, filter: bool) -> Self {
        self.filter_copy_types = filter;
        self
    }

    pub fn with_comment_column(mut self, col: usize) -> Self {
        self.comment_column = col;
        self
    }
}
