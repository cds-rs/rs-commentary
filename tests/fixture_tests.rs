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
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("test-fixtures")
}

fn specs_dir() -> PathBuf {
    fixtures_dir().join("specs").join("src")
}

#[test]
fn test_drop_removes_variable_from_subsequent_lines() {
    let path = specs_dir().join("drop_nll.rs");
    let source = std::fs::read_to_string(&path).expect("drop_nll.rs should exist");

    let config = RenderConfig::default();
    let result = render_source_semantic(&path, &source, RenderStyle::Html, config);

    let html = result.expect("Semantic analysis should work for fixtures workspace");

    // Basic sanity check - the file should render without errors
    assert!(!html.is_empty(), "HTML output should not be empty");
}

#[test]
fn test_param_last_use_detection() {
    // Verify that function parameters are correctly tracked:
    // their last use should be in the function body, not at the declaration.
    let path = specs_dir().join("helpers.rs");
    let source = std::fs::read_to_string(&path).expect("helpers.rs should exist");

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

    // score's last use should be after its declaration
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
    let path = specs_dir().join("copy_types.rs");
    let source = std::fs::read_to_string(&path).expect("copy_types.rs should exist");

    let config = RenderConfig::default();
    let result = render_source_semantic(&path, &source, RenderStyle::Diagnostic, config);

    let output = result.expect("Semantic analysis should work for fixtures workspace");

    eprintln!("=== Diagnostic output ===\n{}", output);

    // The output should show "copied" for Copy types
    assert!(
        output.contains("copied") || output.contains("⊕→"),
        "Copy types should be detected, showing 'copied' annotation.\nOutput:\n{}",
        output
    );
}
