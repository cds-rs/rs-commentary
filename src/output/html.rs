//! HTML renderer - interactive step-through visualization using reveal.js.
//!
//! The borrow checker analyzes each function independently, so this renderer
//! provides per-function stepping with a function selector to switch between them.
//!
//! Features:
//! - Function selector tabs at top
//! - Per-function line-by-line stepping
//! - Call site annotations showing what's moved/borrowed
//! - Linked navigation: click a call to jump to that function's analysis

use crate::analysis::SetEntryState;
use crate::execution::FunctionView;
use crate::util::{Boundary, BoundaryState, TransitionReason};
use super::context::RenderContext;
use super::traits::RichTextRenderer;

/// HTML renderer - produces reveal.js step-through presentation.
pub struct HtmlRenderer;

impl RichTextRenderer for HtmlRenderer {
    fn render(&self, ctx: &mut RenderContext) -> String {
        // Get all functions in the source
        let functions = FunctionView::from_source(ctx.source);

        let slides = build_slides(ctx, &functions);

        format!(
            r#"<!DOCTYPE html>
<html>
<head>
  <meta charset="utf-8">
  <title>Ownership Visualization</title>
  <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/reveal.js/4.6.1/reveal.min.css">
  <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/reveal.js/4.6.1/theme/black.min.css">
  <style>
{CSS_STYLES}
  </style>
</head>
<body>
  <div class="reveal">
    <div class="slides">
{slides}
    </div>
  </div>
  <script src="https://cdnjs.cloudflare.com/ajax/libs/reveal.js/4.6.1/reveal.min.js"></script>
  <script>
    Reveal.initialize({{
      hash: true,
      controls: true,
      progress: true,
      center: false,
      transition: 'none',
      width: 1200,
      height: 800,
      margin: 0.04
    }});

    // Function navigation: jump to function's first slide
    document.querySelectorAll('.fn-link').forEach(link => {{
      link.addEventListener('click', (e) => {{
        e.preventDefault();
        const target = e.target.dataset.fn;
        const slide = document.querySelector(`[data-fn-start="${{target}}"]`);
        if (slide) {{
          const indices = Reveal.getIndices(slide);
          Reveal.slide(indices.h, indices.v);
        }}
      }});
    }});
  </script>
</body>
</html>"#,
            slides = slides,
            CSS_STYLES = CSS_STYLES
        )
    }
}

/// Build all slides for the presentation.
fn build_slides(ctx: &RenderContext, functions: &[FunctionView]) -> String {
    let mut slides = String::new();

    // Title slide with function list
    slides.push_str(&build_title_slide(functions));

    // Build slides for each function
    for func in functions {
        build_function_slides(ctx, func, &mut slides);
    }

    slides
}

/// Build the title slide with function selector.
fn build_title_slide(functions: &[FunctionView]) -> String {
    let fn_list: String = functions
        .iter()
        .map(|f| format!(
            r#"<span class="fn-link" data-fn="{}">{}</span>"#,
            f.name, f.name
        ))
        .collect::<Vec<_>>()
        .join(" · ");

    format!(r#"      <section>
        <h1>Ownership State</h1>
        <p>Step through with arrow keys</p>
        <p style="font-size: 0.8em; margin-top: 20px;">Functions: {}</p>
        <div class="legend-box">
          <code>●●●</code> Owned mut &nbsp;
          <code>●●○</code> Owned/Shared &nbsp;
          <code>●○○</code> Frozen &nbsp;
          <code>○●○</code> &amp;T &nbsp;
          <code>○●●</code> &amp;mut T &nbsp;
          <code>†</code> Dropped
        </div>
        <p style="font-size: 0.7em; color: var(--line-num); margin-top: 30px;">
          Each function is analyzed independently by the borrow checker.
        </p>
      </section>
"#, fn_list)
}

/// Build slides for a single function.
fn build_function_slides(ctx: &RenderContext, func: &FunctionView, slides: &mut String) {
    let lines: Vec<u32> = func.lines().collect();
    let total = lines.len();

    for (step_idx, &line_num) in lines.iter().enumerate() {
        let is_first = step_idx == 0;
        let slide = build_function_slide(ctx, func, line_num, step_idx, total, is_first);
        slides.push_str(&slide);
    }
}

/// Build a single slide for a function line.
fn build_function_slide(
    ctx: &RenderContext,
    func: &FunctionView,
    current_line: u32,
    step_idx: usize,
    total_steps: usize,
    is_first: bool,
) -> String {
    // Build code panel - show only this function's lines, with current highlighted
    let code_html = build_code_panel_for_function(ctx, func, current_line);

    // Get boundary state for state panel and description
    let boundary_state = ctx.ownership_timeline.at(current_line, Boundary::Post);

    // Build state panel
    let state_html = build_state_panel(ctx, boundary_state);

    // Build description
    let desc = build_description(boundary_state, ctx, current_line);

    // Add data attribute for first slide to enable navigation
    let first_attr = if is_first {
        format!(r#" data-fn-start="{}""#, func.name)
    } else {
        String::new()
    };

    format!(
        r#"      <section{}>
        <div class="step-header">
          <span class="step-num">Step {}/{}</span>
          <span class="scope-badge">{}</span>
        </div>
        <div class="two-col">
          <div class="code-panel">
            <div class="panel-title">Source: {}</div>
            <pre class="code-block">{}</pre>
          </div>
          <div class="state-panel">
            <div class="panel-title">Ownership State</div>
            <pre class="state-block">{}</pre>
          </div>
        </div>
        <div class="description">{}</div>
      </section>
"#,
        first_attr,
        step_idx + 1,
        total_steps,
        func.name,
        func.name,
        code_html,
        state_html,
        desc
    )
}

/// Build the code panel showing only the current function's lines.
fn build_code_panel_for_function(ctx: &RenderContext, func: &FunctionView, current_line: u32) -> String {
    let mut code_html = String::new();

    for line_num in func.lines() {
        let line = ctx.lines.get(line_num as usize).map(|s| *s).unwrap_or("");
        let escaped = html_escape(line);
        let class = if line_num == current_line { "line current" } else { "line" };

        code_html.push_str(&format!(
            r#"<div class="{}"><span class="ln">{:3}</span> {}</div>"#,
            class,
            line_num + 1,
            if escaped.is_empty() { "&nbsp;" } else { &escaped }
        ));
        code_html.push('\n');
    }

    code_html
}

/// Build ASCII state panel showing all variables and their states.
fn build_state_panel(ctx: &RenderContext, boundary: Option<&BoundaryState>) -> String {
    let mut lines = Vec::new();

    // Box top
    lines.push("┌────────────────────────────────┐".to_string());
    lines.push("│  <span class=\"state-header\">Variable         State</span>       │".to_string());
    lines.push("├────────────────────────────────┤".to_string());

    let Some(state) = boundary else {
        lines.push("│  <span class=\"empty\">(no tracked variables)</span>       │".to_string());
        lines.push("└────────────────────────────────┘".to_string());
        return lines.join("\n");
    };

    // Display live variables
    let mut has_vars = false;
    for var in &state.variables {
        // Skip Copy types if configured (unless they're borrows)
        let is_borrow = matches!(var.state, SetEntryState::SharedBorrow | SetEntryState::MutBorrow);
        if ctx.config.filter_copy_types && ctx.is_copy_type(&var.name) && !is_borrow {
            continue;
        }

        has_vars = true;

        let dots = match var.state {
            SetEntryState::Owned => if var.mutable { "●●●" } else { "●●○" },
            SetEntryState::Shared => "●●○",
            SetEntryState::Frozen => "●○○",
            SetEntryState::SharedBorrow => "○●○",
            SetEntryState::MutBorrow => "○●●",
            SetEntryState::Dropped => "───",
        };
        let state_desc = state_short_desc(&var.state, var.mutable);

        // Highlight if newly introduced or state changed
        let is_new = state.is_new(&var.name);
        let has_transition = state.has_transition(&var.name);
        let (open_tag, close_tag) = if is_new || has_transition {
            ("<span class=\"changed\">", "</span>")
        } else {
            ("", "")
        };

        let var_display = format!("{}{:<12}{}", open_tag, var.name, close_tag);
        let state_display = format!("{} {}", dots, state_desc);

        lines.push(format!("│  {}  {:<16} │", var_display, state_display));
    }

    // Display dropped variables
    for var in &state.dropped_vars {
        // Skip Copy types if configured
        if ctx.config.filter_copy_types && ctx.is_copy_type(&var.name) {
            continue;
        }

        has_vars = true;
        lines.push(format!(
            "│  <span class=\"dropped\">{:<12}  ───  dropped</span>       │",
            format!("{}†", var.name)
        ));
    }

    if !has_vars {
        lines.push("│  <span class=\"empty\">(no tracked variables)</span>       │".to_string());
    }

    // Box bottom
    lines.push("└────────────────────────────────┘".to_string());

    lines.join("\n")
}

/// Build description text for changes at this line.
fn build_description(
    boundary: Option<&BoundaryState>,
    ctx: &RenderContext,
    line_num: u32,
) -> String {
    let mut parts = Vec::new();

    if let Some(state) = boundary {
        // Describe new variables
        for var in &state.new_vars {
            let state_desc = state_short_desc(&var.state, var.mutable);
            parts.push(format!(
                "<span class=\"new\">NEW:</span> <code>{}</code> introduced as {}",
                var.name, state_desc
            ));
        }

        // Describe state transitions
        for (name, transition) in &state.delta_vars {
            let from_desc = state_short_desc(&transition.from, false);
            let to_desc = state_short_desc(&transition.to, false);
            let reason_desc = format_transition_reason(&transition.reason);
            parts.push(format!(
                "<span class=\"transition\">{}</span> <code>{}</code>: {} → {}{}",
                "↔",
                name,
                from_desc,
                to_desc,
                reason_desc
            ));
        }

        // Describe drops
        for var in &state.dropped_vars {
            parts.push(format!(
                "<span class=\"drop-msg\">†</span> <code>{}</code> dropped (NLL)",
                var.name
            ));
        }
    }

    // Show current line of code if no other description
    if let Some(line) = ctx.lines.get(line_num as usize) {
        let trimmed = line.trim();
        if !trimmed.is_empty() && parts.is_empty() {
            parts.push(format!("<code>{}</code>", html_escape(trimmed)));
        }
    }

    if parts.is_empty() {
        String::new()
    } else {
        parts.join(" &nbsp;│&nbsp; ")
    }
}

fn state_short_desc(state: &SetEntryState, mutable: bool) -> &'static str {
    match state {
        SetEntryState::Owned => if mutable { "owned mut" } else { "owned" },
        SetEntryState::Shared => "shared",
        SetEntryState::Frozen => "frozen",
        SetEntryState::SharedBorrow => "&T",
        SetEntryState::MutBorrow => "&mut T",
        SetEntryState::Dropped => "dropped",
    }
}

fn format_transition_reason(reason: &TransitionReason) -> String {
    match reason {
        TransitionReason::SharedBorrowTaken { borrow_name } => {
            format!(" (borrow: {})", borrow_name)
        }
        TransitionReason::MutBorrowTaken { borrow_name } => {
            format!(" (mut borrow: {})", borrow_name)
        }
        TransitionReason::BorrowEnded { borrow_name } => {
            if borrow_name.is_empty() {
                " (borrow ended)".to_string()
            } else {
                format!(" ({} dropped)", borrow_name)
            }
        }
        TransitionReason::Moved => " (moved)".to_string(),
        TransitionReason::ExplicitDrop => " (explicit drop)".to_string(),
        TransitionReason::ScopeExit => " (scope exit)".to_string(),
        TransitionReason::Other => String::new(),
    }
}

fn html_escape(s: &str) -> String {
    s.replace('&', "&amp;")
        .replace('<', "&lt;")
        .replace('>', "&gt;")
        .replace('"', "&quot;")
}

const CSS_STYLES: &str = r#"
    :root {
      --bg: #1e1e2e;
      --fg: #cdd6f4;
      --line-num: #6c7086;
      --highlight: #45475a;
      --owned-mut: #a6e3a1;
      --owned: #94e2d5;
      --shared: #f9e2af;
      --frozen: #fab387;
      --borrow: #89b4fa;
      --borrow-mut: #cba6f7;
      --dropped: #f38ba8;
      --new: #a6e3a1;
      --changed: #f9e2af;
    }

    .reveal {
      font-family: 'JetBrains Mono', 'Fira Code', 'SF Mono', monospace;
    }

    .reveal h1 {
      font-size: 2em;
      color: var(--owned-mut);
    }

    .reveal .slides section {
      text-align: left;
      padding: 20px;
    }

    /* Function links on title slide */
    .fn-link {
      background: var(--borrow);
      color: var(--bg);
      padding: 4px 12px;
      border-radius: 4px;
      cursor: pointer;
      transition: background 0.2s;
    }

    .fn-link:hover {
      background: var(--borrow-mut);
    }

    /* Legend on title slide */
    .legend-box {
      background: rgba(0,0,0,0.3);
      padding: 16px 24px;
      border-radius: 8px;
      margin-top: 40px;
      font-size: 0.7em;
    }

    .legend-box code {
      color: var(--owned-mut);
      background: none;
    }

    /* Step header */
    .step-header {
      display: flex;
      justify-content: space-between;
      margin-bottom: 12px;
    }

    .step-num {
      background: var(--owned-mut);
      color: var(--bg);
      padding: 4px 12px;
      border-radius: 4px;
      font-size: 0.75em;
      font-weight: bold;
    }

    .scope-badge {
      background: var(--borrow);
      color: var(--bg);
      padding: 4px 12px;
      border-radius: 4px;
      font-size: 0.75em;
      font-weight: bold;
      margin-left: 8px;
    }

    /* Two column layout */
    .two-col {
      display: grid;
      grid-template-columns: 1.5fr 1fr;
      gap: 20px;
      margin-bottom: 16px;
    }

    .code-panel, .state-panel {
      background: #181825;
      border-radius: 8px;
      overflow: hidden;
    }

    .panel-title {
      background: #313244;
      padding: 8px 16px;
      font-size: 0.75em;
      font-weight: bold;
      text-transform: uppercase;
      letter-spacing: 0.05em;
      color: var(--line-num);
    }

    /* Code block */
    .code-block {
      margin: 0;
      padding: 12px 16px;
      font-size: 0.65em;
      line-height: 1.4;
      max-height: 400px;
      overflow-y: auto;
    }

    .code-block .line {
      white-space: pre;
      padding: 1px 0;
    }

    .code-block .line.current {
      background: var(--highlight);
      margin: 0 -16px;
      padding: 1px 16px;
      border-left: 3px solid var(--owned-mut);
    }

    .code-block .ln {
      color: var(--line-num);
      user-select: none;
      margin-right: 16px;
    }

    /* State block */
    .state-block {
      margin: 0;
      padding: 12px 16px;
      font-size: 0.7em;
      line-height: 1.5;
    }

    .state-header {
      color: var(--line-num);
      font-weight: bold;
    }

    .state-block .changed {
      color: var(--changed);
      font-weight: bold;
    }

    .state-block .dropped {
      color: var(--dropped);
    }

    .state-block .empty {
      color: var(--line-num);
      font-style: italic;
    }

    /* Description */
    .description {
      background: #181825;
      padding: 12px 16px;
      border-radius: 8px;
      font-size: 0.75em;
      min-height: 1.5em;
    }

    .description code {
      background: rgba(0,0,0,0.3);
      padding: 2px 6px;
      border-radius: 4px;
    }

    .description .new {
      color: var(--new);
      font-weight: bold;
    }

    .description .transition {
      color: var(--changed);
      font-weight: bold;
    }

    .description .drop-msg {
      color: var(--dropped);
      font-weight: bold;
    }

    .description .call-transfer {
      color: var(--borrow);
    }

    /* Reveal.js overrides */
    .reveal .progress {
      color: var(--owned-mut);
    }

    .reveal .controls {
      color: var(--owned-mut);
    }
"#;
