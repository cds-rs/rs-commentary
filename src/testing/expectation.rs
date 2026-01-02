//! Expectation parsing for ownership state tests.
//!
//! This module provides types and parsing for `//~` comments that express
//! expected ownership states in test fixtures.
//!
//! Uses rust-analyzer's AST to extract comments, ensuring accurate line numbers
//! and proper handling of edge cases.
//!
//! # Syntax
//!
//! ```text
//! //~ var: state              // Basic expectation
//! //~ var: state, var2: state // Multiple variables
//! //~ !var                    // Variable should NOT be live
//! //~^ var: state             // Applies to line above
//! //~| var: state             // Continuation (same line as previous)
//! ```
//!
//! # State Names
//!
//! | Name | Meaning |
//! |------|---------|
//! | `owned_mut` | Owned mutable (ORW) |
//! | `owned` | Owned immutable (OR) |
//! | `shared` | Downgraded by &T borrow |
//! | `frozen` | Downgraded by &mut borrow |
//! | `ref_shared` | Shared reference (&T) |
//! | `ref_mut` | Mutable reference (&mut T) |
//! | `moved` | Value moved |
//! | `dropped` | Value dropped |

use std::collections::HashMap;
use ra_ap_syntax::{SourceFile, SyntaxKind, SyntaxToken};

/// Expected ownership state for a variable.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExpectedState {
    /// Owned mutable binding (ORW capabilities)
    OwnedMut,
    /// Owned immutable binding (OR capabilities)
    Owned,
    /// Shared: downgraded due to active &T borrow
    Shared,
    /// Frozen: downgraded due to active &mut borrow
    Frozen,
    /// Shared reference (&T)
    RefShared,
    /// Mutable reference (&mut T)
    RefMut,
    /// Value was moved
    Moved,
    /// Value was dropped
    Dropped,
}

impl ExpectedState {
    /// Parse a state name from text.
    pub fn parse(text: &str) -> Result<Self, ParseError> {
        match text.trim() {
            "owned_mut" => Ok(ExpectedState::OwnedMut),
            "owned" => Ok(ExpectedState::Owned),
            "shared" => Ok(ExpectedState::Shared),
            "frozen" => Ok(ExpectedState::Frozen),
            "ref_shared" => Ok(ExpectedState::RefShared),
            "ref_mut" => Ok(ExpectedState::RefMut),
            "moved" => Ok(ExpectedState::Moved),
            "dropped" => Ok(ExpectedState::Dropped),
            other => Err(ParseError::UnknownState(other.to_string())),
        }
    }

    /// Get the canonical name for this state.
    pub fn name(&self) -> &'static str {
        match self {
            ExpectedState::OwnedMut => "owned_mut",
            ExpectedState::Owned => "owned",
            ExpectedState::Shared => "shared",
            ExpectedState::Frozen => "frozen",
            ExpectedState::RefShared => "ref_shared",
            ExpectedState::RefMut => "ref_mut",
            ExpectedState::Moved => "moved",
            ExpectedState::Dropped => "dropped",
        }
    }
}

/// A single expectation parsed from a `//~` comment.
#[derive(Debug, Clone)]
pub struct Expectation {
    /// Variable name.
    pub var: String,
    /// Expected state (None if this is a negative assertion).
    pub state: Option<ExpectedState>,
    /// Line number (1-indexed, matching source).
    pub line: u32,
    /// If true, this is a negative assertion: variable should NOT be live.
    pub negated: bool,
    /// Original comment text for error messages.
    pub raw: String,
}

/// Error during expectation parsing.
#[derive(Debug, Clone)]
pub enum ParseError {
    /// Unknown state name
    UnknownState(String),
    /// Malformed expectation syntax
    MalformedExpectation { line: u32, text: String },
    /// Missing variable name
    MissingVariable { line: u32 },
    /// Missing state after colon
    MissingState { line: u32, var: String },
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::UnknownState(s) => write!(f, "unknown state: '{}'", s),
            ParseError::MalformedExpectation { line, text } => {
                write!(f, "line {}: malformed expectation: '{}'", line, text)
            }
            ParseError::MissingVariable { line } => {
                write!(f, "line {}: missing variable name", line)
            }
            ParseError::MissingState { line, var } => {
                write!(f, "line {}: missing state for variable '{}'", line, var)
            }
        }
    }
}

impl std::error::Error for ParseError {}

/// All expectations for a source file, grouped by line.
#[derive(Debug, Default)]
pub struct ExpectationSet {
    /// Expectations grouped by line number (1-indexed).
    pub by_line: HashMap<u32, Vec<Expectation>>,
}

impl ExpectationSet {
    /// Create an empty expectation set.
    pub fn new() -> Self {
        Self {
            by_line: HashMap::new(),
        }
    }

    /// Parse all `//~` expectations from source code using rust-analyzer's AST.
    ///
    /// This uses the parser to find COMMENT tokens, ensuring accurate line numbers
    /// that match what the ownership analyzer sees.
    ///
    /// Returns the expectation set and any parse errors encountered.
    pub fn parse(source: &str) -> (Self, Vec<ParseError>) {
        let mut set = ExpectationSet::new();
        let mut errors = Vec::new();

        // Parse source with rust-analyzer
        let parse = SourceFile::parse(source, ra_ap_syntax::Edition::Edition2021);
        let syntax = parse.syntax_node();

        // Find all comment tokens
        for token in syntax.descendants_with_tokens().filter_map(|it| it.into_token()) {
            if !is_expectation_comment(&token) {
                continue;
            }

            let text = token.text();
            let range = token.text_range();

            // Get 0-indexed line number from byte offset (matches analyzer's line numbers)
            let line_num = byte_offset_to_line(source, range.start().into());

            // Extract the part after "//~"
            let after_marker = &text[3..]; // Skip "//~"

            // Determine target line based on offset markers (^)
            let (target_line, expectation_text) = parse_line_offset(after_marker, line_num);

            // Parse the expectation content
            match parse_expectation_content(expectation_text.trim(), target_line) {
                Ok(expectations) => {
                    set.by_line.entry(target_line).or_default().extend(expectations);
                }
                Err(e) => errors.push(e),
            }
        }

        (set, errors)
    }

    /// Get expectations for a specific line.
    pub fn get(&self, line: u32) -> Option<&Vec<Expectation>> {
        self.by_line.get(&line)
    }

    /// Check if there are any expectations.
    pub fn is_empty(&self) -> bool {
        self.by_line.is_empty()
    }

    /// Get total number of expectations.
    pub fn len(&self) -> usize {
        self.by_line.values().map(|v| v.len()).sum()
    }
}

/// Check if a token is a `//~` expectation comment.
fn is_expectation_comment(token: &SyntaxToken) -> bool {
    token.kind() == SyntaxKind::COMMENT && token.text().starts_with("//~")
}

/// Convert byte offset to 0-indexed line number.
fn byte_offset_to_line(source: &str, offset: usize) -> u32 {
    source[..offset.min(source.len())]
        .chars()
        .filter(|&c| c == '\n')
        .count() as u32
}

/// Parse line offset markers (^) and return (target_line, remaining_text).
fn parse_line_offset(text: &str, current_line: u32) -> (u32, &str) {
    let trimmed = text.trim_start();

    if trimmed.starts_with('^') {
        // Count carets for line offset
        let caret_count = trimmed.chars().take_while(|&c| c == '^').count();
        let target = current_line.saturating_sub(caret_count as u32);
        let remaining = &trimmed[caret_count..];
        (target, remaining)
    } else if trimmed.starts_with('|') {
        // Continuation: same as previous line (use current - 1 as approximation)
        // In practice, this would need context, but for simplicity we use current line
        (current_line, &trimmed[1..])
    } else {
        // No offset: applies to current line
        (current_line, trimmed)
    }
}

/// Parse the content of an expectation comment.
///
/// Format: `var: state` or `var: state, var2: state` or `!var`
fn parse_expectation_content(text: &str, line: u32) -> Result<Vec<Expectation>, ParseError> {
    if text.is_empty() {
        return Ok(Vec::new());
    }

    let mut expectations = Vec::new();

    // Split by comma for multiple expectations
    for part in text.split(',') {
        let part = part.trim();
        if part.is_empty() {
            continue;
        }

        let expectation = parse_single_expectation(part, line)?;
        expectations.push(expectation);
    }

    Ok(expectations)
}

/// Parse a single expectation like `var: state` or `!var`.
fn parse_single_expectation(text: &str, line: u32) -> Result<Expectation, ParseError> {
    let text = text.trim();

    // Check for negation
    if text.starts_with('!') {
        let var = text[1..].trim();
        if var.is_empty() {
            return Err(ParseError::MissingVariable { line });
        }
        return Ok(Expectation {
            var: var.to_string(),
            state: None,
            line,
            negated: true,
            raw: text.to_string(),
        });
    }

    // Parse `var: state` format
    let parts: Vec<&str> = text.splitn(2, ':').collect();
    if parts.len() != 2 {
        return Err(ParseError::MalformedExpectation {
            line,
            text: text.to_string(),
        });
    }

    let var = parts[0].trim();
    if var.is_empty() {
        return Err(ParseError::MissingVariable { line });
    }

    let state_text = parts[1].trim();

    // Strip optional borrow info like "(by x, y)" for now
    let state_name = state_text
        .split_once('(')
        .map(|(name, _)| name.trim())
        .unwrap_or(state_text);

    // Strip optional move target like "-> z" for now
    let state_name = state_name
        .split_once("->")
        .map(|(name, _)| name.trim())
        .unwrap_or(state_name);

    if state_name.is_empty() {
        return Err(ParseError::MissingState {
            line,
            var: var.to_string(),
        });
    }

    let state = ExpectedState::parse(state_name)?;

    Ok(Expectation {
        var: var.to_string(),
        state: Some(state),
        line,
        negated: false,
        raw: text.to_string(),
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_state_names() {
        assert_eq!(ExpectedState::parse("owned_mut").unwrap(), ExpectedState::OwnedMut);
        assert_eq!(ExpectedState::parse("owned").unwrap(), ExpectedState::Owned);
        assert_eq!(ExpectedState::parse("shared").unwrap(), ExpectedState::Shared);
        assert_eq!(ExpectedState::parse("frozen").unwrap(), ExpectedState::Frozen);
        assert_eq!(ExpectedState::parse("ref_shared").unwrap(), ExpectedState::RefShared);
        assert_eq!(ExpectedState::parse("ref_mut").unwrap(), ExpectedState::RefMut);
        assert_eq!(ExpectedState::parse("moved").unwrap(), ExpectedState::Moved);
        assert_eq!(ExpectedState::parse("dropped").unwrap(), ExpectedState::Dropped);
    }

    #[test]
    fn test_parse_unknown_state() {
        assert!(matches!(
            ExpectedState::parse("unknown"),
            Err(ParseError::UnknownState(_))
        ));
    }

    #[test]
    fn test_parse_single_expectation() {
        let exp = parse_single_expectation("x: owned_mut", 1).unwrap();
        assert_eq!(exp.var, "x");
        assert_eq!(exp.state, Some(ExpectedState::OwnedMut));
        assert!(!exp.negated);
    }

    #[test]
    fn test_parse_negated_expectation() {
        let exp = parse_single_expectation("!x", 1).unwrap();
        assert_eq!(exp.var, "x");
        assert_eq!(exp.state, None);
        assert!(exp.negated);
    }

    #[test]
    fn test_parse_expectation_with_borrow_info() {
        let exp = parse_single_expectation("x: shared (by y)", 1).unwrap();
        assert_eq!(exp.var, "x");
        assert_eq!(exp.state, Some(ExpectedState::Shared));
    }

    #[test]
    fn test_parse_expectation_set() {
        let source = r#"
fn test() {
    let x = String::new();  //~ x: owned_mut
    let y = &x;             //~ y: ref_shared, x: shared
}
"#;
        let (set, errors) = ExpectationSet::parse(source);
        assert!(errors.is_empty(), "errors: {:?}", errors);
        assert_eq!(set.len(), 3); // x: owned_mut, y: ref_shared, x: shared

        // Line 2 (0-indexed): x: owned_mut
        let line2 = set.get(2).unwrap();
        assert_eq!(line2.len(), 1);
        assert_eq!(line2[0].var, "x");

        // Line 3 (0-indexed): y: ref_shared, x: shared
        let line3 = set.get(3).unwrap();
        assert_eq!(line3.len(), 2);
    }

    #[test]
    fn test_parse_line_above() {
        let source = r#"
fn test() {
    let x = String::new();
    //~^ x: owned_mut
}
"#;
        let (set, errors) = ExpectationSet::parse(source);
        assert!(errors.is_empty());

        // The expectation on line 3 (0-indexed) applies to line 2
        let line2 = set.get(2).unwrap();
        assert_eq!(line2.len(), 1);
        assert_eq!(line2[0].var, "x");
    }
}
