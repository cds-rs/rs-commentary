//! HTML renderer - interactive visualization inspired by Aquascope.
//!
//! Generates standalone HTML with:
//! - Syntax-highlighted source code
//! - Hover tooltips showing ownership state
//! - Visual indicators for borrows and moves
//! - Color-coded capability indicators

use crate::analysis::{SetEntry, SetEntryState};
use super::context::RenderContext;
use super::helpers::format_capability_dots;
use super::traits::RichTextRenderer;

/// HTML renderer - produces interactive HTML visualization.
pub struct HtmlRenderer;

impl RichTextRenderer for HtmlRenderer {
    fn render(&self, ctx: &mut RenderContext) -> String {
        let mut lines_html = String::new();

        for (line_num, line) in ctx.lines.iter().enumerate() {
            let line_num = line_num as u32;
            let escaped_line = html_escape(line);

            // Get state for this line
            let state_html = if let Some(set) = ctx.sets_by_line.get(&line_num) {
                let entries = ctx.filter_entries(set);
                format_html_state(&entries)
            } else {
                String::new()
            };

            // Get any drops for this line
            let drops = ctx.timeline.get_pending_drops(line_num);
            let drops_html = if !drops.is_empty() {
                let drop_spans: Vec<String> = drops
                    .iter()
                    .map(|d| {
                        format!(
                            r#"<span class="drop" title="{} dropped (NLL)">{}†</span>"#,
                            d.name, d.name
                        )
                    })
                    .collect();
                format!(r#"<div class="nll-drop">{}</div>"#, drop_spans.join(" "))
            } else {
                String::new()
            };

            lines_html.push_str(&format!(
                r#"<div class="line" data-line="{}">
  <span class="line-num">{}</span>
  <code class="source">{}</code>
  <span class="state">{}</span>
  {}
</div>
"#,
                line_num + 1,
                line_num + 1,
                escaped_line,
                state_html,
                drops_html
            ));
        }

        format!(
            r#"<!DOCTYPE html>
<html>
<head>
  <meta charset="utf-8">
  <title>Ownership Visualization</title>
  <style>
{CSS_STYLES}
  </style>
</head>
<body>
  <div class="container">
    <div class="code-view">
{lines_html}
    </div>
    <div class="legend">
      <h3>Legend</h3>
      <div class="legend-item"><span class="dot owned-mut"></span> Owned (mutable)</div>
      <div class="legend-item"><span class="dot owned"></span> Owned (immutable)</div>
      <div class="legend-item"><span class="dot shared"></span> Shared / Frozen</div>
      <div class="legend-item"><span class="dot borrow"></span> Borrow active</div>
      <div class="legend-item"><span class="dot dropped"></span> Dropped</div>
    </div>
  </div>
  <script>
{JS_SCRIPT}
  </script>
</body>
</html>"#,
            lines_html = lines_html,
            CSS_STYLES = CSS_STYLES,
            JS_SCRIPT = JS_SCRIPT
        )
    }
}

fn html_escape(s: &str) -> String {
    s.replace('&', "&amp;")
        .replace('<', "&lt;")
        .replace('>', "&gt;")
        .replace('"', "&quot;")
}

fn format_html_state(entries: &[&SetEntry]) -> String {
    entries
        .iter()
        .filter(|e| !matches!(e.state, SetEntryState::Dropped))
        .map(|e| {
            let (class, title) = match &e.state {
                SetEntryState::Owned => {
                    if e.mutable {
                        ("owned-mut", "Owned (mutable) - full access")
                    } else {
                        ("owned", "Owned (immutable) - read only")
                    }
                }
                SetEntryState::Shared => ("shared", "Shared - read only while borrowed"),
                SetEntryState::Frozen => ("frozen", "Frozen - no access while &mut active"),
                SetEntryState::SharedBorrow => ("borrow", "Shared borrow (&T)"),
                SetEntryState::MutBorrow => ("borrow-mut", "Mutable borrow (&mut T)"),
                SetEntryState::Dropped => ("dropped", "Dropped"),
            };
            let borrow_info = if let Some(from) = &e.borrows_from {
                format!(" (from {})", from)
            } else {
                String::new()
            };
            format!(
                r#"<span class="var {}" title="{}{}">{} {}</span>"#,
                class,
                title,
                borrow_info,
                e.name,
                format_capability_dots(e)
            )
        })
        .collect::<Vec<_>>()
        .join(" ")
}

const CSS_STYLES: &str = r#"
    :root {
      --bg: #1e1e2e;
      --fg: #cdd6f4;
      --line-num: #6c7086;
      --owned-mut: #a6e3a1;
      --owned: #94e2d5;
      --shared: #f9e2af;
      --frozen: #fab387;
      --borrow: #89b4fa;
      --borrow-mut: #cba6f7;
      --dropped: #f38ba8;
    }
    body {
      background: var(--bg);
      color: var(--fg);
      font-family: 'JetBrains Mono', 'Fira Code', monospace;
      margin: 0;
      padding: 20px;
    }
    .container {
      display: flex;
      gap: 40px;
      max-width: 1200px;
      margin: 0 auto;
    }
    .code-view {
      flex: 1;
      background: #181825;
      border-radius: 8px;
      padding: 16px;
      overflow-x: auto;
    }
    .line {
      display: flex;
      align-items: baseline;
      gap: 16px;
      padding: 2px 0;
      min-height: 1.5em;
    }
    .line:hover {
      background: rgba(255,255,255,0.05);
    }
    .line-num {
      color: var(--line-num);
      min-width: 2em;
      text-align: right;
      user-select: none;
    }
    .source {
      flex: 1;
      white-space: pre;
    }
    .state {
      display: flex;
      gap: 12px;
      font-size: 0.9em;
    }
    .var {
      padding: 2px 6px;
      border-radius: 4px;
      cursor: help;
    }
    .var.owned-mut { background: rgba(166,227,161,0.2); color: var(--owned-mut); }
    .var.owned { background: rgba(148,226,213,0.2); color: var(--owned); }
    .var.shared { background: rgba(249,226,175,0.2); color: var(--shared); }
    .var.frozen { background: rgba(250,179,135,0.2); color: var(--frozen); }
    .var.borrow { background: rgba(137,180,250,0.2); color: var(--borrow); }
    .var.borrow-mut { background: rgba(203,166,247,0.2); color: var(--borrow-mut); }
    .nll-drop {
      margin-left: 8px;
    }
    .drop {
      color: var(--dropped);
      font-size: 0.85em;
      cursor: help;
    }
    .legend {
      background: #181825;
      border-radius: 8px;
      padding: 16px;
      min-width: 200px;
    }
    .legend h3 {
      margin: 0 0 12px 0;
      font-size: 1em;
    }
    .legend-item {
      display: flex;
      align-items: center;
      gap: 8px;
      padding: 4px 0;
      font-size: 0.9em;
    }
    .dot {
      width: 12px;
      height: 12px;
      border-radius: 50%;
    }
    .dot.owned-mut { background: var(--owned-mut); }
    .dot.owned { background: var(--owned); }
    .dot.shared { background: var(--shared); }
    .dot.borrow { background: var(--borrow); }
    .dot.dropped { background: var(--dropped); }
"#;

const JS_SCRIPT: &str = r#"
    // Add interactivity - highlight related variables on hover
    document.querySelectorAll('.var').forEach(el => {
      el.addEventListener('mouseenter', () => {
        const varName = el.textContent.split(' ')[0];
        document.querySelectorAll('.var').forEach(other => {
          if (other.textContent.startsWith(varName + ' ')) {
            other.style.outline = '2px solid currentColor';
          }
        });
      });
      el.addEventListener('mouseleave', () => {
        document.querySelectorAll('.var').forEach(other => {
          other.style.outline = 'none';
        });
      });
    });
"#;
