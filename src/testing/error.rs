//! Error types for expectation verification.

use std::path::PathBuf;

use super::expectation::ParseError;
use super::matcher::MatchResult;

/// A single expectation failure.
#[derive(Debug, Clone)]
pub struct ExpectationFailure {
    /// Function name where the failure occurred.
    pub function: String,
    /// Line number (1-indexed).
    pub line: u32,
    /// Variable name.
    pub var: String,
    /// The match result describing the failure.
    pub result: MatchResult,
    /// Original expectation text.
    pub raw: String,
    /// The source line content.
    pub source_line: String,
}

impl std::fmt::Display for ExpectationFailure {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "line {}: ", self.line)?;
        match &self.result {
            MatchResult::Match => write!(f, "ok"),
            MatchResult::StateMismatch { expected, actual } => {
                write!(
                    f,
                    "expected {}: {}, actual: {}",
                    self.var,
                    expected.name(),
                    super::matcher::state_to_name(actual, false) // TODO: pass actual mutability
                )
            }
            MatchResult::VariableNotFound { var } => {
                write!(f, "expected {} to be live, but it was not found", var)
            }
            MatchResult::UnexpectedVariable { var, actual } => {
                write!(
                    f,
                    "expected {} to NOT be live, but found it as {}",
                    var,
                    super::matcher::state_to_name(actual, false)
                )
            }
        }
    }
}

/// Result of running a single test function.
#[derive(Debug)]
pub struct FnTestResult {
    /// Function name.
    pub name: String,
    /// List of failures (empty if passed).
    pub failures: Vec<ExpectationFailure>,
}

impl FnTestResult {
    /// Returns true if the test passed (no failures).
    pub fn passed(&self) -> bool {
        self.failures.is_empty()
    }
}

/// Result of running all tests in a fixture file.
#[derive(Debug)]
pub struct FileTestResult {
    /// Path to the fixture file.
    pub path: PathBuf,
    /// Module documentation (//! comments) - shown on failure.
    pub module_doc: Option<String>,
    /// Results for each function in the file.
    pub functions: Vec<FnTestResult>,
}

impl FileTestResult {
    /// Returns true if all tests passed.
    pub fn passed(&self) -> bool {
        self.functions.iter().all(|f| f.passed())
    }

    /// Count of passed tests.
    pub fn pass_count(&self) -> usize {
        self.functions.iter().filter(|f| f.passed()).count()
    }

    /// Count of failed tests.
    pub fn fail_count(&self) -> usize {
        self.functions.iter().filter(|f| !f.passed()).count()
    }

    /// Total number of tests.
    pub fn total(&self) -> usize {
        self.functions.len()
    }
}

/// Error during verification.
#[derive(Debug)]
pub enum VerificationError {
    /// Parse errors in expectations
    ParseErrors(Vec<ParseError>),
    /// Semantic analyzer failed to load
    SemanticLoadFailed(String),
    /// Analysis failed
    AnalysisFailed(String),
    /// Test failures
    TestFailures(FileTestResult),
}

impl std::fmt::Display for VerificationError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            VerificationError::ParseErrors(errors) => {
                writeln!(f, "Parse errors:")?;
                for e in errors {
                    writeln!(f, "  {}", e)?;
                }
                Ok(())
            }
            VerificationError::SemanticLoadFailed(msg) => {
                write!(f, "Failed to load semantic analyzer: {}", msg)
            }
            VerificationError::AnalysisFailed(msg) => {
                write!(f, "Analysis failed: {}", msg)
            }
            VerificationError::TestFailures(result) => {
                writeln!(f, "{}", result.path.display())?;
                for func in &result.functions {
                    if func.passed() {
                        writeln!(f, "  \u{2713} {}", func.name)?;
                    } else {
                        writeln!(f, "  \u{2717} {}", func.name)?;
                        for failure in &func.failures {
                            writeln!(f, "      {}", failure)?;
                        }
                    }
                }
                Ok(())
            }
        }
    }
}

impl std::error::Error for VerificationError {}
