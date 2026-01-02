//! Expectation-based integration tests.
//!
//! This test runner discovers all `.rs` files in test-fixtures/specs that contain
//! `//~` expectation comments and verifies the ownership analysis matches.
//!
//! # Test Organization
//!
//! - Each `.rs` file is a test suite ("describe") for a concept
//! - Each `fn` in the file is a test case ("it") for a specific behavior
//! - `//~` comments define expected ownership states
//!
//! # Example
//!
//! ```rust,ignore
//! // test-fixtures/specs/src/borrow_shared.rs
//! fn downgrades_owner_to_shared() {
//!     let x = String::new();  //~ x: owned
//!     let y = &x;             //~ y: ref_shared, x: shared
//! }
//! ```

use rs_commentary::testing::{format_results, verify_file, VerificationError};
use std::path::PathBuf;

fn fixtures_dir() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("test-fixtures")
}

fn specs_dir() -> PathBuf {
    fixtures_dir().join("specs").join("src")
}

/// Discover all `.rs` files in the specs directory.
fn discover_spec_files() -> Vec<PathBuf> {
    let mut files = Vec::new();

    if let Ok(entries) = std::fs::read_dir(specs_dir()) {
        for entry in entries.flatten() {
            let path = entry.path();
            // Skip lib.rs and helpers.rs
            let name = path.file_name().and_then(|n| n.to_str()).unwrap_or("");
            if path.extension().map(|e| e == "rs").unwrap_or(false)
                && name != "lib.rs"
                && name != "helpers.rs"
            {
                files.push(path);
            }
        }
    }

    files.sort();
    files
}

#[test]
fn run_expectation_tests() {
    let files = discover_spec_files();

    if files.is_empty() {
        println!("No spec files found in {:?}", specs_dir());
        return;
    }

    let mut total_pass = 0;
    let mut total_fail = 0;
    let mut all_output = String::new();

    for path in &files {
        match verify_file(path) {
            Ok(result) => {
                if result.functions.is_empty() {
                    // No expectations in this file, skip
                    continue;
                }
                total_pass += result.pass_count();
                total_fail += result.fail_count();
                all_output.push_str(&format_results(&result));
            }
            Err(VerificationError::TestFailures(result)) => {
                total_pass += result.pass_count();
                total_fail += result.fail_count();
                all_output.push_str(&format_results(&result));
            }
            Err(e) => {
                all_output.push_str(&format!("{}: {}\n", path.display(), e));
                total_fail += 1;
            }
        }
    }

    println!("\n{}", all_output);
    println!("Total: {} passed, {} failed", total_pass, total_fail);

    if total_fail > 0 {
        panic!("{} expectation test(s) failed", total_fail);
    }
}

/// Test shared borrow specs.
#[test]
fn test_borrow_shared_specs() {
    let path = specs_dir().join("borrow_shared.rs");
    run_spec_file(&path, "borrow_shared");
}

/// Test mutable borrow specs.
#[test]
fn test_borrow_mut_specs() {
    let path = specs_dir().join("borrow_mut.rs");
    run_spec_file(&path, "borrow_mut");
}

/// Test move semantics specs.
#[test]
fn test_move_semantics_specs() {
    let path = specs_dir().join("move_semantics.rs");
    run_spec_file(&path, "move_semantics");
}

fn run_spec_file(path: &PathBuf, name: &str) {
    if !path.exists() {
        println!("{}.rs not found, skipping", name);
        return;
    }

    match verify_file(path) {
        Ok(result) => {
            println!("{}", format_results(&result));
            assert!(result.passed(), "All {} tests should pass", name);
        }
        Err(VerificationError::TestFailures(result)) => {
            println!("{}", format_results(&result));
            panic!("{} tests failed", name);
        }
        Err(e) => {
            panic!("Error running {} tests: {}", name, e);
        }
    }
}
