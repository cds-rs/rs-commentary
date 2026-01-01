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
use crate::output::context::{AnnotationTracker, LineContent, RenderContext};
use crate::output::traits::RichTextRenderer;
use crate::util::{TransitionReason, VarTransition};

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
  <div id="zoom-indicator" title="Zoom: +/- or 0 to reset">100%</div>
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

    // Center the current line in the code panel when slide changes
    function scrollToCurrentLine() {{
      const currentSlide = Reveal.getCurrentSlide();
      if (!currentSlide) return;
      const currentLine = currentSlide.querySelector('.line.current');
      if (currentLine) {{
        currentLine.scrollIntoView({{ block: 'center', behavior: 'instant' }});
      }}
    }}

    Reveal.on('slidechanged', scrollToCurrentLine);
    Reveal.on('ready', scrollToCurrentLine);

    // Zoom controls: +/= to zoom in, - to zoom out, 0 to reset
    let zoomLevel = 1.0;
    const zoomStep = 0.15;
    const zoomMin = 0.5;
    const zoomMax = 2.5;
    const zoomIndicator = document.getElementById('zoom-indicator');
    const baseFontSizes = {{ code: 0.65, state: 0.7, desc: 0.75 }};

    function updateZoom() {{
      // Scale font sizes within content areas
      document.querySelectorAll('.code-block').forEach(el => {{
        el.style.fontSize = `${{baseFontSizes.code * zoomLevel}}em`;
      }});
      document.querySelectorAll('.state-block').forEach(el => {{
        el.style.fontSize = `${{baseFontSizes.state * zoomLevel}}em`;
      }});
      document.querySelectorAll('.description').forEach(el => {{
        el.style.fontSize = `${{baseFontSizes.desc * zoomLevel}}em`;
      }});
      if (zoomIndicator) {{
        zoomIndicator.textContent = `${{Math.round(zoomLevel * 100)}}%`;
        zoomIndicator.style.opacity = '1';
        setTimeout(() => zoomIndicator.style.opacity = '0.5', 1000);
      }}
      // Re-center after zoom
      setTimeout(scrollToCurrentLine, 50);
    }}

    document.addEventListener('keydown', (e) => {{
      // Ignore if typing in an input
      if (e.target.tagName === 'INPUT' || e.target.tagName === 'TEXTAREA') return;

      if (e.key === '=' || e.key === '+') {{
        e.preventDefault();
        zoomLevel = Math.min(zoomMax, zoomLevel + zoomStep);
        updateZoom();
      }} else if (e.key === '-') {{
        e.preventDefault();
        zoomLevel = Math.max(zoomMin, zoomLevel - zoomStep);
        updateZoom();
      }} else if (e.key === '0') {{
        e.preventDefault();
        zoomLevel = 1.0;
        updateZoom();
      }}
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
    // Use unified annotation tracker from context
    let mut tracker = AnnotationTracker::new();

    // Collect lines that have pedagogical value using unified LineContent
    let interesting_lines: Vec<LineContent> = func
        .lines()
        .filter_map(|line_num| {
            let content = ctx.get_line_content(line_num, &mut tracker);
            if content.is_interesting() {
                Some(content)
            } else {
                None
            }
        })
        .collect();

    let total = interesting_lines.len();

    for (step_idx, content) in interesting_lines.into_iter().enumerate() {
        let is_first = step_idx == 0;
        let slide = build_function_slide_from_content(ctx, func, &content, step_idx, total, is_first);
        slides.push_str(&slide);
    }
}

/// Build a slide from unified LineContent.
fn build_function_slide_from_content(
    ctx: &RenderContext,
    func: &FunctionView,
    content: &LineContent,
    step_idx: usize,
    total_steps: usize,
    is_first: bool,
) -> String {
    let current_line = content.line_num;

    // Build code panel - show only this function's lines, with current highlighted
    let code_html = build_code_panel_for_function(ctx, func, current_line);

    // Build state panel using unified transitions
    let state_html = build_state_panel(ctx, current_line);

    // Build description from unified LineContent
    let desc = build_description_from_content(content);

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
            <div class="state-block">{}</div>
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

/// Build description from unified LineContent.
fn build_description_from_content(content: &LineContent) -> String {
    let mut parts = Vec::new();

    // Describe new variables
    for var in &content.new_vars {
        let state_desc = state_short_desc(&var.state, var.mutable);
        parts.push(format!(
            "<span class=\"new\">NEW:</span> <code>{}</code> introduced as {}",
            var.name, state_desc
        ));
    }

    // Describe state transitions
    for (name, transition) in &content.transitions {
        let from_desc = state_short_desc(&transition.from, false);
        let to_desc = state_short_desc(&transition.to, false);
        let reason_desc = format_transition_reason(&transition.reason);
        parts.push(format!(
            "<span class=\"transition\">↔</span> <code>{}</code>: {} → {}{}",
            name,
            from_desc,
            to_desc,
            reason_desc
        ));
    }

    // Describe copy events (using pre-computed "still valid" from LineContent)
    for copy in &content.copy_events {
        let still_valid = if copy.show_still_valid {
            format!("; <code>{}</code> still valid", copy.from)
        } else {
            String::new()
        };
        parts.push(format!(
            "<span class=\"call-transfer\">⟳</span> <code>{}</code> copied → {} (Copy){}",
            copy.from, copy.to, still_valid
        ));
    }

    // Describe drops
    for var in &content.drops {
        parts.push(format!(
            "<span class=\"drop-msg\">†</span> <code>{}</code> dropped (NLL)",
            var.name
        ));
    }

    if parts.is_empty() {
        // Show current line code as context
        let trimmed = content.line_text.trim();
        if !trimmed.is_empty() {
            return format!("<code>{}</code>", html_escape(trimmed));
        }
        String::new()
    } else {
        parts.join(" &nbsp;│&nbsp; ")
    }
}

/// Build the code panel showing only the current function's lines.
fn build_code_panel_for_function(ctx: &RenderContext, func: &FunctionView, current_line: u32) -> String {
    let mut code_html = String::new();

    for line_num in func.lines() {
        let line = ctx.lines.get(line_num as usize).copied().unwrap_or("");

        // Skip empty lines (e.g., from stripped comments)
        if line.trim().is_empty() {
            continue;
        }

        let escaped = html_escape(line);
        let class = if line_num == current_line { "line current" } else { "line" };

        code_html.push_str(&format!(
            r#"<div class="{}"><span class="ln">{:3}</span> {}</div>"#,
            class,
            line_num + 1,
            &escaped
        ));
        code_html.push('\n');
    }

    code_html
}

/// Build state panel as an HTML table using unified transitions.
fn build_state_panel(ctx: &RenderContext, line_num: u32) -> String {
    let mut html = String::from(r#"<table class="state-table"><thead><tr><th>Variable</th><th>State</th></tr></thead><tbody>"#);

    let transitions = ctx.timeline.get_var_transitions(line_num);

    if transitions.is_empty() {
        html.push_str(r#"<tr><td colspan="2" class="empty">(no tracked variables)</td></tr>"#);
        html.push_str("</tbody></table>");
        return html;
    }

    let mut has_vars = false;

    for trans in &transitions {
        // Skip Copy types if configured (unless they're borrows)
        let is_borrow = trans.curr.as_ref().map_or(false, |(s, _)| {
            matches!(s, SetEntryState::SharedBorrow | SetEntryState::MutBorrow)
        });
        if ctx.config.filter_copy_types && ctx.is_copy_type(&trans.name) && !is_borrow {
            continue;
        }

        has_vars = true;

        // Determine row class and build state cell using (prev, curr) window
        let (row_class, state_cell) = render_transition(trans);

        html.push_str(&format!(
            "<tr{}><td>{}</td><td>{}</td></tr>",
            row_class, trans.name, state_cell
        ));
    }

    if !has_vars {
        html.push_str(r#"<tr><td colspan="2" class="empty">(no tracked variables)</td></tr>"#);
    }

    html.push_str("</tbody></table>");
    html
}

/// Render a single variable transition as (row_class, state_cell).
fn render_transition(trans: &VarTransition) -> (&'static str, String) {
    match (&trans.prev, &trans.curr) {
        // New: ∅ → state
        (None, Some((state, mutable))) => {
            let dots = state_to_dots(state, *mutable);
            let desc = state_short_desc(state, *mutable);
            (
                " class=\"new\"",
                format!("<span class=\"prev-state\">∅</span> → <span class=\"dots\">{}</span> {}", dots, desc)
            )
        }
        // Dropped: state → ∅
        (Some((state, mutable)), None) => {
            let prev_dots = state_to_dots(state, *mutable);
            (
                " class=\"dropped\"",
                format!("<span class=\"prev-state\">{}</span> → <span class=\"dots\">†</span> dropped", prev_dots)
            )
        }
        // Transition: prev → curr (different states)
        (Some((prev_state, _)), Some((curr_state, mutable))) if prev_state != curr_state => {
            let prev_dots = state_to_dots(prev_state, false);
            let curr_dots = state_to_dots(curr_state, *mutable);
            let desc = state_short_desc(curr_state, *mutable);
            (
                " class=\"transition\"",
                format!("<span class=\"prev-state\">{}</span> → <span class=\"dots\">{}</span> {}", prev_dots, curr_dots, desc)
            )
        }
        // Unchanged: just show current state
        (Some(_), Some((state, mutable))) => {
            let dots = state_to_dots(state, *mutable);
            let desc = state_short_desc(state, *mutable);
            (
                "",
                format!("<span class=\"dots\">{}</span> {}", dots, desc)
            )
        }
        // Shouldn't happen: both None
        (None, None) => ("", String::new()),
    }
}

fn state_short_desc(state: &SetEntryState, mutable: bool) -> &'static str {
    match state {
        SetEntryState::Owned => if mutable { "owned mut" } else { "owned" },
        SetEntryState::Shared { .. } => "shared",
        SetEntryState::Frozen { .. } => "frozen",
        SetEntryState::SharedBorrow => "&T",
        SetEntryState::MutBorrow => "&mut T",
        SetEntryState::Moved { .. } => "moved",
        SetEntryState::Dropped => "dropped",
    }
}

fn state_to_dots(state: &SetEntryState, mutable: bool) -> &'static str {
    match state {
        SetEntryState::Owned => if mutable { "●●●" } else { "●●○" },
        SetEntryState::Shared { .. } => "●●○",
        SetEntryState::Frozen { .. } => "●○○",
        SetEntryState::SharedBorrow => "○●○",
        SetEntryState::MutBorrow => "○●●",
        SetEntryState::Moved { .. } => "───",
        SetEntryState::Dropped => "───",
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
        TransitionReason::MovedTo { target } => {
            if let Some(t) = target {
                format!(" (moved to {})", t)
            } else {
                " (moved)".to_string()
            }
        }
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

    /* State block with table */
    .state-block {
      margin: 0;
      padding: 12px 16px;
      font-size: 0.7em;
      line-height: 1.5;
    }

    .state-table {
      width: 100%;
      border-collapse: collapse;
      font-family: inherit;
    }

    .state-table th {
      text-align: left;
      color: var(--line-num);
      font-weight: bold;
      padding: 4px 8px;
      border-bottom: 1px solid var(--highlight);
    }

    .state-table td {
      padding: 4px 8px;
      vertical-align: top;
    }

    .state-table tr.new td {
      border-left: 3px solid var(--new);
    }

    .state-table tr.new td:first-child {
      color: var(--new);
      font-weight: bold;
    }

    .state-table tr.transition td {
      border-left: 3px solid var(--changed);
    }

    .state-table tr.transition td:first-child {
      color: var(--changed);
      font-weight: bold;
    }

    .state-table .prev-state {
      font-size: 0.85em;
      opacity: 0.5;
      margin-right: 0.25em;
    }

    .state-table tr.dropped td {
      color: var(--dropped);
    }

    .state-table .dots {
      font-family: inherit;
      margin-right: 6px;
    }

    .state-table .empty {
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

    /* Zoom indicator */
    #zoom-indicator {
      position: fixed;
      bottom: 60px;
      right: 20px;
      background: var(--highlight);
      color: var(--fg);
      padding: 6px 12px;
      border-radius: 4px;
      font-size: 0.75em;
      font-weight: bold;
      opacity: 0.5;
      transition: opacity 0.3s;
      cursor: help;
      z-index: 100;
    }

    #zoom-indicator:hover {
      opacity: 1;
    }
"#;
