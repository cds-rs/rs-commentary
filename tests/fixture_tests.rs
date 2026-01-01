//! Integration tests using the test-fixtures workspace.
//!
//! These tests analyze real Rust code from the fixtures workspace
//! using rust-analyzer for accurate NLL drop detection.
//!
//! The test-fixtures/ directory is a separate Cargo workspace containing
//! real, compilable Rust crates. This allows rs-commentary to analyze
//! them with full rust-analyzer support.

use rs_commentary::analysis::{SemanticAnalyzer, SemanticResult};
use rs_commentary::output::{render_source_semantic, RenderConfig, RenderStyle};
use std::path::PathBuf;

fn fixtures_dir() -> PathBuf {
    // test-fixtures is inside rs-commentary as a separate workspace
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("test-fixtures")
}

fn fixture_path(crate_name: &str) -> PathBuf {
    fixtures_dir().join(crate_name).join("src").join("main.rs")
}

fn read_fixture(crate_name: &str) -> String {
    let path = fixture_path(crate_name);
    std::fs::read_to_string(&path)
        .unwrap_or_else(|e| panic!("Failed to read fixture {:?}: {}", path, e))
}

#[test]
fn test_drop_removes_variable_from_subsequent_lines() {
    // Fixture (0-indexed lines):
    // 0: fn main() {
    // 1:     let x = String::new();
    // 2:     let y = &x;
    // 3:     println!("{}", y);
    // 4:     drop(x);
    // 5:     println!("after drop");
    // 6: }
    let path = fixture_path("drop_test");
    let source = read_fixture("drop_test");

    // Use semantic analysis to get accurate NLL drop detection
    let config = RenderConfig::default();
    let result = render_source_semantic(&path, &source, RenderStyle::Html, config);

    let html = result.expect("Semantic analysis should work for fixtures workspace");

    eprintln!("=== HTML output ===\n{}", html);

    // After drop(x) on line 4, x should NOT appear as a live variable on line 5
    // The HTML should show x as dropped (†) on line 4, and not show x at all on line 5

    // Check that x appears on line 4 (the drop line)
    assert!(
        html.contains("drop(x)"),
        "HTML should contain the drop(x) call"
    );

    // Verify NLL drop semantics:
    // 1. x should be shown as dropped (x†) at its last use line
    // 2. The closing brace is skipped (no pedagogical value) unless something drops there

    // x should be shown as dropped
    assert!(
        html.contains("x†") || html.contains("<code>x</code> dropped"),
        "x should be shown as dropped"
    );

    // x should NOT appear as Owned (●●●) after being dropped
    // It can appear as dropped (x†) on the drop line, but not as live
    let after_drop_section = html.split("after drop").last().unwrap_or("");
    assert!(
        !after_drop_section.contains("●●●") || !after_drop_section.contains(">x<"),
        "x should not appear as Owned after drop(x)"
    );
}

#[test]
fn test_param_last_use_detection() {
    // Verify that function parameters are correctly tracked:
    // their last use should be in the function body, not at the declaration.
    let path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("test-fixtures/drop_test/src/allergies.rs");
    let source = std::fs::read_to_string(&path).expect("allergies.rs should exist");

    let analyzer = match SemanticAnalyzer::load(&path) {
        SemanticResult::Available(a) => a,
        SemanticResult::NotCargoProject => panic!("Not a cargo project"),
        SemanticResult::LoadFailed => panic!("Load failed"),
    };

    let file_id = analyzer.file_id(&path).expect("should get file_id");
    let last_uses = analyzer.find_all_last_uses(file_id, &source);

    // Find 'score' parameter in fn new(score: u32)
    let score_info = last_uses.values().find(|i| i.name == "score");
    assert!(score_info.is_some(), "Should find 'score' parameter");
    let score = score_info.unwrap();

    // score is declared on line 28 (0-indexed), used on line 30 (score as u8)
    // last_use_line should be 30, not 28
    assert!(
        score.last_use_line > score.decl_line,
        "score's last use ({}) should be after declaration ({})",
        score.last_use_line,
        score.decl_line
    );
}

#[test]
fn test_for_loop_variable_is_copy() {
    // Test that for-loop iteration variables are correctly detected as Copy types.
    // Previously, for-loop patterns were always marked as non-Copy because the
    // offset-based lookup failed to find the semantic type info.
    //
    // With the TypeOracle refactor, we query rust-analyzer directly at the AST node,
    // which correctly identifies `group_size: usize` as Copy.
    let path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("test-fixtures/drop_test/src/for_loop.rs");
    let source = std::fs::read_to_string(&path).expect("for_loop.rs should exist");

    let config = RenderConfig::default();
    let result = render_source_semantic(&path, &source, RenderStyle::Diagnostic, config);

    let output = result.expect("Semantic analysis should work for fixtures workspace");

    eprintln!("=== Diagnostic output ===\n{}", output);

    // The output should show "copied → take" not "moved → take"
    // This verifies that group_size (usize) is correctly detected as Copy
    assert!(
        output.contains("copied") || output.contains("⊕→"),
        "For-loop variable (usize) should be detected as Copy, showing 'copied' annotation.\nOutput:\n{}",
        output
    );
    assert!(
        !output.contains("moved → take"),
        "For-loop variable should NOT show as 'moved' - it's a Copy type.\nOutput:\n{}",
        output
    );
}
