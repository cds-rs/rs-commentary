//! Test infrastructure for ownership expectation tests.
//!
//! This module provides a framework for writing ownership state tests where
//! the expected state is embedded directly in the source code using `//~` comments.
//!
//! # Test Organization
//!
//! Each fixture file is a test suite ("describe"), each function is a test case ("it"):
//!
//! ```rust,ignore
//! // File: borrows.rs - describes borrow behavior
//! fn shared_borrow_downgrades_owner() {
//!     let x = String::new();      //~ x: owned_mut
//!     let y = &x;                 //~ y: ref_shared, x: shared
//! }
//! ```
//!
//! # Usage
//!
//! ```rust,ignore
//! use rs_commentary::testing::verify_file;
//!
//! let result = verify_file(&path);
//! if !result.passed() {
//!     panic!("{}", result);
//! }
//! ```

pub mod error;
pub mod expectation;
pub mod matcher;

pub use error::{ExpectationFailure, FileTestResult, FnTestResult, VerificationError};
pub use expectation::{ExpectedState, Expectation, ExpectationSet};

use std::collections::HashMap;
use std::path::Path;

use ra_ap_syntax::ast::{self, HasModuleItem, HasName};
use ra_ap_syntax::{AstNode, SourceFile};

use crate::analysis::{OwnershipAnalyzer, SemanticAnalyzer, SemanticResult};
use crate::output::{RenderContext, RenderConfig};
use crate::util::VarSnapshot;
use matcher::{states_match, MatchResult};

/// A test function discovered in a fixture file.
#[derive(Debug)]
pub struct FnTestCase {
    /// Function name (becomes the test name).
    pub name: String,
    /// Line range (start, end) - 1-indexed, inclusive.
    pub line_range: (u32, u32),
    /// Expectations for this function.
    pub expectations: ExpectationSet,
}

/// Discover all test functions in a source file.
///
/// Parses the AST to find function definitions and filters
/// expectations to those within each function's line range.
pub fn discover_test_functions(source: &str) -> Vec<FnTestCase> {
    let parse = SourceFile::parse(source, ra_ap_syntax::Edition::Edition2021);
    let file = parse.tree();

    // Parse all expectations from the file (0-indexed line numbers)
    let (all_expectations, _errors) = ExpectationSet::parse(source);

    let mut test_cases = Vec::new();

    // Find all functions and filter expectations by line range
    for item in file.items() {
        if let ast::Item::Fn(func) = item {
            if let Some(name) = func.name() {
                let syntax = func.syntax();
                let range = syntax.text_range();

                // Get 0-indexed line range
                let start_line = byte_offset_to_line(source, range.start().into());
                let end_line = byte_offset_to_line(source, range.end().into());

                // Filter expectations to this function's range
                let fn_expectations: HashMap<u32, Vec<Expectation>> = all_expectations
                    .by_line
                    .iter()
                    .filter(|(&line, _)| line >= start_line && line <= end_line)
                    .map(|(&line, exps)| (line, exps.clone()))
                    .collect();

                // Only include if there are expectations
                if !fn_expectations.is_empty() {
                    test_cases.push(FnTestCase {
                        name: name.text().to_string(),
                        line_range: (start_line, end_line),
                        expectations: ExpectationSet { by_line: fn_expectations },
                    });
                }
            }
        }
    }

    test_cases
}

/// Convert byte offset to 0-indexed line number.
fn byte_offset_to_line(source: &str, offset: usize) -> u32 {
    source[..offset.min(source.len())]
        .chars()
        .filter(|&c| c == '\n')
        .count() as u32
}

/// Verify expectations in a fixture file.
///
/// Loads the file, discovers test functions, runs ownership analysis,
/// and compares actual state against expectations.
pub fn verify_file(path: &Path) -> Result<FileTestResult, VerificationError> {
    let source = std::fs::read_to_string(path)
        .map_err(|e| VerificationError::AnalysisFailed(format!("Failed to read file: {}", e)))?;

    verify_source(path, &source)
}

/// Verify expectations in source code.
///
/// This is the main entry point for testing. It:
/// 1. Discovers test functions in the source
/// 2. Runs semantic analysis on the file
/// 3. Compares actual ownership state against `//~` expectations
/// 4. Returns detailed results for each function
pub fn verify_source(path: &Path, source: &str) -> Result<FileTestResult, VerificationError> {
    // Load semantic analyzer
    let analyzer = match SemanticAnalyzer::load(path) {
        SemanticResult::Available(a) => a,
        SemanticResult::NotCargoProject => {
            return Err(VerificationError::SemanticLoadFailed(
                "Not a cargo project".to_string(),
            ))
        }
        SemanticResult::LoadFailed => {
            return Err(VerificationError::SemanticLoadFailed(
                "Load failed".to_string(),
            ))
        }
    };

    verify_source_with_analyzer(path, source, &analyzer)
}

/// Verify expectations in source code using a pre-loaded analyzer.
///
/// Use this when verifying multiple files in the same workspace to avoid
/// reloading rust-analyzer for each file.
pub fn verify_source_with_analyzer(
    path: &Path,
    source: &str,
    analyzer: &SemanticAnalyzer,
) -> Result<FileTestResult, VerificationError> {
    // Discover test functions
    let test_cases = discover_test_functions(source);

    if test_cases.is_empty() {
        // No test functions with expectations - return empty result
        return Ok(FileTestResult {
            path: path.to_path_buf(),
            functions: vec![],
        });
    }

    let file_id = analyzer.file_id(path).ok_or_else(|| {
        VerificationError::SemanticLoadFailed("Failed to get file ID".to_string())
    })?;

    // Get semantic info for accurate type detection
    let last_use_info = analyzer.find_all_last_uses(file_id, source);
    let copy_types: HashMap<String, bool> = last_use_info
        .values()
        .map(|info| (info.name.clone(), info.is_copy()))
        .collect();

    // Create type oracle for analysis
    let type_oracle = analyzer.type_oracle(file_id);

    // Run ownership analysis
    let mut ownership_analyzer = OwnershipAnalyzer::new();
    ownership_analyzer.set_type_oracle(&type_oracle);
    ownership_analyzer
        .analyze(source)
        .map_err(|e| VerificationError::AnalysisFailed(format!("Analysis failed: {:?}", e)))?;

    let set_annotations = ownership_analyzer.set_annotations();

    // Build render context to get computed states
    let config = RenderConfig::default();
    let ctx = RenderContext::new_with_semantic(source, set_annotations, config, copy_types);

    // Verify each test function
    let lines: Vec<&str> = source.lines().collect();
    let mut fn_results = Vec::new();

    for test_case in test_cases {
        let failures = verify_function(&ctx, &test_case, &lines);
        fn_results.push(FnTestResult {
            name: test_case.name,
            failures,
        });
    }

    let result = FileTestResult {
        path: path.to_path_buf(),
        functions: fn_results,
    };

    if result.passed() {
        Ok(result)
    } else {
        Err(VerificationError::TestFailures(result))
    }
}

/// Verify multiple files in a workspace, loading the analyzer once.
///
/// This is more efficient than calling `verify_file` repeatedly.
pub fn verify_workspace(
    files: &[std::path::PathBuf],
) -> Result<Vec<FileTestResult>, VerificationError> {
    if files.is_empty() {
        return Ok(vec![]);
    }

    // Load analyzer once using the first file
    let analyzer = match SemanticAnalyzer::load(&files[0]) {
        SemanticResult::Available(a) => a,
        SemanticResult::NotCargoProject => {
            return Err(VerificationError::SemanticLoadFailed(
                "Not a cargo project".to_string(),
            ))
        }
        SemanticResult::LoadFailed => {
            return Err(VerificationError::SemanticLoadFailed(
                "Load failed".to_string(),
            ))
        }
    };

    let mut results = Vec::new();
    for path in files {
        let source = std::fs::read_to_string(path)
            .map_err(|e| VerificationError::AnalysisFailed(format!("Failed to read file: {}", e)))?;

        match verify_source_with_analyzer(path, &source, &analyzer) {
            Ok(result) => results.push(result),
            Err(VerificationError::TestFailures(result)) => results.push(result),
            Err(e) => return Err(e),
        }
    }

    Ok(results)
}

/// Verify expectations for a single function.
fn verify_function(
    ctx: &RenderContext,
    test_case: &FnTestCase,
    lines: &[&str],
) -> Vec<ExpectationFailure> {
    let mut failures = Vec::new();

    for (line_num, expectations) in &test_case.expectations.by_line {
        // Get actual state at this line from the timeline
        let boundary = ctx.timeline.boundary_state(*line_num);

        // Build a map of actual states at this line
        let actual_states: HashMap<&str, &VarSnapshot> = boundary
            .as_ref()
            .map(|b| {
                b.variables
                    .iter()
                    .map(|v| (v.name.as_str(), v))
                    .collect()
            })
            .unwrap_or_default();

        let source_line = lines
            .get((*line_num - 1) as usize)
            .map(|s| s.to_string())
            .unwrap_or_default();

        for exp in expectations {
            let result = check_expectation(exp, &actual_states);

            if !result.is_match() {
                failures.push(ExpectationFailure {
                    function: test_case.name.clone(),
                    line: *line_num,
                    var: exp.var.clone(),
                    result,
                    raw: exp.raw.clone(),
                    source_line: source_line.clone(),
                });
            }
        }
    }

    failures
}

/// Check a single expectation against actual state.
fn check_expectation(
    exp: &Expectation,
    actual_states: &HashMap<&str, &VarSnapshot>,
) -> MatchResult {
    let actual = actual_states.get(exp.var.as_str());

    if exp.negated {
        // Negative assertion: variable should NOT be live
        match actual {
            Some(v) => MatchResult::UnexpectedVariable {
                var: exp.var.clone(),
                actual: v.state.clone(),
            },
            None => MatchResult::Match,
        }
    } else {
        // Positive assertion: variable should be live with specific state
        let expected_state = exp.state.as_ref().expect("non-negated expectation should have state");

        match actual {
            Some(v) => {
                if states_match(expected_state, &v.state, v.mutable) {
                    MatchResult::Match
                } else {
                    MatchResult::StateMismatch {
                        expected: expected_state.clone(),
                        actual: v.state.clone(),
                    }
                }
            }
            None => {
                // Variable not found - check if it's dropped
                MatchResult::VariableNotFound {
                    var: exp.var.clone(),
                }
            }
        }
    }
}

/// Format test results for display.
pub fn format_results(result: &FileTestResult) -> String {
    let mut output = String::new();
    output.push_str(&format!("{}\n", result.path.display()));

    for func in &result.functions {
        if func.passed() {
            output.push_str(&format!("  \u{2713} {}\n", func.name));
        } else {
            output.push_str(&format!("  \u{2717} {}\n", func.name));
            for failure in &func.failures {
                output.push_str(&format!("      {}\n", failure));
            }
        }
    }

    output.push_str(&format!(
        "\n{} passed, {} failed\n",
        result.pass_count(),
        result.fail_count()
    ));

    output
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_discover_functions() {
        let source = r#"
fn first_test() {
    let x = String::new();  //~ x: owned_mut
}

fn no_expectations() {
    let y = 42;
}

fn second_test() {
    let a = String::new();  //~ a: owned_mut
    let b = &a;             //~ b: ref_shared
}
"#;
        let cases = discover_test_functions(source);

        // Should find 2 functions with expectations (no_expectations has none)
        assert_eq!(cases.len(), 2);
        assert_eq!(cases[0].name, "first_test");
        assert_eq!(cases[1].name, "second_test");

        // Check expectation counts
        assert_eq!(cases[0].expectations.len(), 1);
        assert_eq!(cases[1].expectations.len(), 2);
    }

    #[test]
    fn test_byte_offset_to_line() {
        let source = "line 0\nline 1\nline 2";
        assert_eq!(byte_offset_to_line(source, 0), 0);
        assert_eq!(byte_offset_to_line(source, 7), 1); // after first newline
        assert_eq!(byte_offset_to_line(source, 14), 2); // after second newline
    }
}
