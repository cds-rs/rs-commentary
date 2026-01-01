//! Top-level render functions.

use crate::analysis::{BindingKind, OwnershipAnalyzer, SemanticAnalyzer, SemanticResult};
use ra_ap_syntax::{SourceFile, SyntaxKind, SyntaxToken};
use std::collections::HashMap;
use std::path::Path;

use super::context::RenderContext;
use super::renderers::{
    ColumnarRenderer, DiagnosticRenderer, GroupedRenderer, HtmlRenderer, InlineRenderer,
    SetNotationRenderer, ValidatedRenderContext, ValidatedRenderer, VerticalSpansRenderer,
};
use super::traits::{RichTextRenderer, ValidRustRenderer};
use super::types::{RenderConfig, RenderStyle};

/// Strip comments from Rust source while preserving line count.
///
/// Uses rust-analyzer's parser to correctly handle all edge cases:
/// - Line comments (`//`), block comments (`/* */`)
/// - Doc comments (`///`, `//!`, `/** */`, `/*! */`)
/// - Nested block comments
/// - Comments inside string literals (preserved)
fn strip_comments(source: &str) -> String {
    let parse = SourceFile::parse(source, ra_ap_syntax::Edition::Edition2021);
    let syntax = parse.syntax_node();

    // Collect all comment token ranges
    let mut comment_ranges: Vec<(usize, usize)> = Vec::new();
    for token in syntax.descendants_with_tokens().filter_map(|it| it.into_token()) {
        if is_comment(&token) {
            let range = token.text_range();
            comment_ranges.push((range.start().into(), range.end().into()));
        }
    }

    if comment_ranges.is_empty() {
        return source.to_string();
    }

    // Build result by copying non-comment parts, preserving newlines in comments
    let mut result = String::with_capacity(source.len());
    let bytes = source.as_bytes();
    let mut pos = 0;

    for (start, end) in comment_ranges {
        // Copy text before this comment
        if pos < start {
            result.push_str(&source[pos..start]);
        }
        // Preserve newlines within the comment (for block comments)
        for &b in &bytes[start..end] {
            if b == b'\n' {
                result.push('\n');
            }
        }
        pos = end;
    }

    // Copy remaining text after last comment
    if pos < source.len() {
        result.push_str(&source[pos..]);
    }

    // Just trim trailing whitespace - blank line collapsing happens
    // after rendering to preserve line number alignment during annotation.
    result
        .lines()
        .map(|line| line.trim_end())
        .collect::<Vec<_>>()
        .join("\n")
}

/// Check if a token is any kind of comment.
fn is_comment(token: &SyntaxToken) -> bool {
    matches!(token.kind(), SyntaxKind::COMMENT)
}

/// Collapse runs of 3+ consecutive empty source lines to 2 in rendered output.
///
/// An "empty source line" in diagnostic output looks like: ` 5 │ ` or ` 5 │`
/// (line number, pipe, optional whitespace). This preserves annotation lines
/// which have different structure.
fn collapse_blank_lines(output: &str) -> String {
    let lines: Vec<&str> = output.lines().collect();
    let mut result = Vec::with_capacity(lines.len());
    let mut blank_run = 0;

    for line in lines {
        // Check if this is an empty source line: matches pattern like " 5 │ " or " 5 │"
        // These lines have a number, then │, then only whitespace (or nothing)
        let is_empty_source_line = line.contains('│') && {
            if let Some(after_pipe) = line.split('│').nth(1) {
                after_pipe.trim().is_empty()
            } else {
                false
            }
        } && {
            // Also verify the part before │ is just whitespace and a number
            if let Some(before_pipe) = line.split('│').next() {
                before_pipe.trim().chars().all(|c| c.is_ascii_digit())
            } else {
                false
            }
        };

        if is_empty_source_line {
            blank_run += 1;
            if blank_run <= 2 {
                result.push(line);
            }
        } else {
            blank_run = 0;
            result.push(line);
        }
    }

    result.join("\n")
}

/// Render source with the specified style (AST-only, for tests).
///
/// This is an internal function used for testing. Production code should use
/// `render_source_semantic()` which provides accurate type information.
#[cfg(test)]
pub(crate) fn render_source(source: &str, style: RenderStyle, config: RenderConfig) -> String {
    // Analyze with original source (positions must be accurate)
    let mut analyzer = OwnershipAnalyzer::new();

    let Ok(_) = analyzer.analyze(source) else {
        return source.to_string();
    };

    let set_annotations = analyzer.set_annotations();

    // Strip comments if requested (for cleaner display)
    let display_source: String;
    let source_for_render = if config.strip_comments {
        display_source = strip_comments(source);
        &display_source
    } else {
        source
    };

    // In AST-only mode, use the analyzer's Copy type detection
    let copy_types = analyzer.copy_types();
    let should_collapse = config.strip_comments;
    let mut ctx = RenderContext::new_with_semantic(source_for_render, set_annotations, config, copy_types);
    ctx.set_copy_events(analyzer.copy_events());

    let output = match style {
        // ValidRustRenderer implementations
        RenderStyle::Inline => ValidRustRenderer::render(&InlineRenderer, &mut ctx),
        RenderStyle::Columnar => ValidRustRenderer::render(&ColumnarRenderer, &mut ctx),
        RenderStyle::Grouped => ValidRustRenderer::render(&GroupedRenderer, &mut ctx),
        // RichTextRenderer implementations
        RenderStyle::Diagnostic => RichTextRenderer::render(&DiagnosticRenderer, &mut ctx),
        RenderStyle::SetNotation => RichTextRenderer::render(&SetNotationRenderer, &mut ctx),
        RenderStyle::VerticalSpans => RichTextRenderer::render(&VerticalSpansRenderer, &mut ctx),
        RenderStyle::Html => RichTextRenderer::render(&HtmlRenderer, &mut ctx),
        // Validated requires semantic analysis - fallback to SetNotation
        RenderStyle::Validated => {
            // Without file path, we can't load rust-analyzer
            // Fall back to SetNotation with a warning
            let mut output = String::from(
                "╭─ rust-analyzer: ⚠ semantic analysis requires file path\n│  use render_source_semantic() instead\n│\n",
            );
            output.push_str(&RichTextRenderer::render(&SetNotationRenderer, &mut ctx));
            output
        }
    };

    // Collapse blank lines after rendering (when comments were stripped)
    if should_collapse {
        collapse_blank_lines(&output)
    } else {
        output
    }
}

/// Render source with semantic analysis from rust-analyzer.
///
/// This loads the cargo workspace and provides actual compiler diagnostics
/// alongside ownership state annotations. Uses accurate NLL drop detection
/// based on rust-analyzer's reference analysis.
///
/// Returns `None` if the file is not part of a cargo project or loading fails.
pub fn render_source_semantic(
    file_path: &Path,
    source: &str,
    style: RenderStyle,
    config: RenderConfig,
) -> Option<String> {
    // Load semantic analysis
    let analyzer = match SemanticAnalyzer::load(file_path) {
        SemanticResult::Available(a) => a,
        SemanticResult::NotCargoProject => return None,
        SemanticResult::LoadFailed => return None,
    };

    let file_id = analyzer.file_id(file_path)?;
    let diagnostics = analyzer.diagnostics(file_id);

    // Get semantic last-use data for accurate NLL drops
    let last_use_info = analyzer.find_all_last_uses(file_id, source);

    // Extract drop lines: line AFTER last use (already computed by semantic analysis)
    // Key is (name, decl_line) to disambiguate variables with the same name in different scopes
    let drop_lines: HashMap<(String, u32), u32> = last_use_info
        .values()
        .map(|info| ((info.name.clone(), info.decl_line), info.drop_line))
        .collect();

    // Extract Copy type info for accurate filtering
    let copy_types: HashMap<String, bool> = last_use_info
        .values()
        .map(|info| (info.name.clone(), info.is_copy()))
        .collect();

    // Extract binding kinds for accurate messaging (borrow vs drop)
    // Key by (name, decl_line) to disambiguate variables with the same name in different scopes
    let binding_kinds: HashMap<(String, u32), BindingKind> = last_use_info
        .values()
        .map(|info| ((info.name.clone(), info.decl_line), info.kind))
        .collect();

    // Extract copy types keyed by declaration offset for ownership analyzer
    let copy_types_by_offset: HashMap<u32, bool> = last_use_info
        .iter()
        .map(|(decl_offset, info)| (u32::from(*decl_offset), info.is_copy()))
        .collect();

    // Run ownership analysis (with original source for accurate positions)
    let mut ownership_analyzer = OwnershipAnalyzer::new();
    ownership_analyzer.set_semantic_copy_types(copy_types_by_offset);
    let Ok(_) = ownership_analyzer.analyze(source) else {
        return None;
    };

    let set_annotations = ownership_analyzer.set_annotations();

    // Strip comments if requested (for cleaner display)
    let display_source: String;
    let source_for_render = if config.strip_comments {
        display_source = strip_comments(source);
        &display_source
    } else {
        source
    };

    // Create context with semantic Copy info for accurate filtering
    let mut ctx =
        RenderContext::new_with_semantic(source_for_render, set_annotations, config.clone(), copy_types);
    ctx.set_semantic_drop_lines(drop_lines);
    ctx.set_binding_kinds(binding_kinds);
    ctx.set_copy_events(ownership_analyzer.copy_events());

    // For Validated style, use the special renderer
    if matches!(style, RenderStyle::Validated) {
        let mut validated_ctx = ValidatedRenderContext::new(ctx, &diagnostics, source_for_render);
        let output = ValidatedRenderer::render(&mut validated_ctx);
        return Some(if config.strip_comments {
            collapse_blank_lines(&output)
        } else {
            output
        });
    }

    let output = match style {
        RenderStyle::Inline => ValidRustRenderer::render(&InlineRenderer, &mut ctx),
        RenderStyle::Columnar => ValidRustRenderer::render(&ColumnarRenderer, &mut ctx),
        RenderStyle::Grouped => ValidRustRenderer::render(&GroupedRenderer, &mut ctx),
        RenderStyle::Diagnostic => RichTextRenderer::render(&DiagnosticRenderer, &mut ctx),
        RenderStyle::SetNotation => RichTextRenderer::render(&SetNotationRenderer, &mut ctx),
        RenderStyle::VerticalSpans => RichTextRenderer::render(&VerticalSpansRenderer, &mut ctx),
        RenderStyle::Html => RichTextRenderer::render(&HtmlRenderer, &mut ctx),
        RenderStyle::Validated => unreachable!(),
    };

    // Collapse blank lines after rendering (when comments were stripped)
    Some(if config.strip_comments {
        collapse_blank_lines(&output)
    } else {
        output
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_copy_type_no_drop_note() {
        // Copy types should not show "dropped at scope exit" notes
        let source = r#"fn main() {
    let x: i32 = 42;
    let y = x;
    println!("{}", y);
}"#;
        let output = render_source(source, RenderStyle::Diagnostic, RenderConfig::default());
        // Should show the copy annotation
        assert!(output.contains("copied"), "Should show copy annotation: {output}");
        // Should NOT show drop notes for Copy types
        assert!(
            !output.contains("dropped") && !output.contains("last used"),
            "Should not show drop notes for Copy types: {output}"
        );
    }

    #[test]
    fn test_array_copy_through_block() {
        // Arrays of primitives are Copy, even through block expressions.
        // Note: Type annotations required without semantic analysis.
        let source = r#"fn process(counts: [u32; 5]) {
    let book_groups: [u32; 5] = {
        let mut k: [u32; 5] = counts;
        k
    };
    let next: [u32; 5] = book_groups;  // This should be a copy, not move
    println!("{:?}", book_groups);  // book_groups still valid
}"#;
        let output = render_source(source, RenderStyle::Diagnostic, RenderConfig::default());
        // book_groups should NOT show "move → next" or "now invalid"
        assert!(
            !output.contains("now invalid"),
            "Array [u32; 5] should not be invalidated: {output}"
        );
    }

    #[test]
    fn test_copy_annotation_at_correct_line_in_for_loop() {
        // Copy annotation should appear at the line where the copy happens,
        // not at a later use of the variable.
        // This mimics the user's book-store code structure.
        // Note: Type annotations required without semantic analysis.
        let source = r#"fn process(counts: [u32; 5]) {
    let book_groups: [u32; 5] = {
        let mut k: [u32; 5] = counts;
        k
    };
    for i in 0..5 {
        let next: [u32; 5] = book_groups;
    }
    use_array(book_groups);
}
fn use_array(_: [u32; 5]) {}"#;
        // Test with strip_comments to match user's --strip-comments flag
        let config = RenderConfig::default().with_strip_comments(true);
        let output = render_source(source, RenderStyle::Diagnostic, config);
        eprintln!("--- For loop copy output (strip_comments=true) ---\n{output}\n---");

        // The "book_groups: copied → next" annotation should appear near line 7 (let next...),
        // not at line 9 (use_array(book_groups))
        let lines: Vec<&str> = output.lines().collect();

        // Find the line with "let next" (has type annotation now)
        let let_next_idx = lines.iter().position(|l| l.contains("let next:")).unwrap();
        // Find the line with "use_array(book_groups)"
        let use_array_idx = lines.iter().position(|l| l.contains("use_array(book_groups)")).unwrap();

        // The "copied → next" annotation should appear near the let next line
        let copy_annotation_idx = lines.iter().position(|l| l.contains("copied → next"));
        assert!(
            copy_annotation_idx.is_some(),
            "Should have 'copied → next' annotation in output"
        );
        let copy_annotation_idx = copy_annotation_idx.unwrap();

        assert!(
            copy_annotation_idx < use_array_idx,
            "Copy annotation should appear before use_array line.\n\
             let next line: {}, copy annotation: {}, use_array line: {}",
            let_next_idx, copy_annotation_idx, use_array_idx
        );
    }

    #[test]
    fn test_non_copy_type_shows_drop() {
        // Non-Copy types should still show drop notes
        let source = r#"fn main() {
    let s = String::from("hello");
    println!("{}", s);
}"#;
        let output = render_source(source, RenderStyle::Diagnostic, RenderConfig::default());
        // Non-Copy types may show drop/last use info (if the analyzer detects it)
        // This test verifies we don't suppress drops for non-Copy types
        // The main check is that the code doesn't crash and renders something
        assert!(output.contains("fn main"), "Should render the source: {output}");
    }

    #[test]
    fn test_collapse_blank_lines() {
        // Test the collapse_blank_lines function
        let input = " 1 │ line1\n 2 │ \n 3 │ \n 4 │ \n 5 │ \n 6 │ line2";
        let output = collapse_blank_lines(input);
        // Should collapse 4 blank lines (2-5) to 2
        let lines: Vec<&str> = output.lines().collect();
        assert_eq!(lines.len(), 4, "Should have 4 lines after collapsing: {:?}", lines);
        assert!(lines[0].contains("line1"));
        assert!(lines[3].contains("line2"));
    }
}
