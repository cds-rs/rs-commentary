//! Layer 2: Type Oracle using rust-analyzer.
//!
//! Provides type information to refine Layer 1's AST-based analysis.

use ra_ap_syntax::{TextRange, TextSize};
use ra_ap_ide::{AnalysisHost, FileId, FileRange, HoverConfig, HoverResult, SubstTyLen};
use ra_ap_vfs::Vfs;
use std::path::Path;

use super::layers::ParamMode;

/// Layer 2 implementation using rust-analyzer.
pub struct RaTypeOracle {
    host: AnalysisHost,
    vfs: Vfs,
    file_id: Option<FileId>,
}

impl RaTypeOracle {
    /// Create from an already-loaded rust-analyzer host.
    pub fn new(host: AnalysisHost, vfs: Vfs) -> Self {
        Self {
            host,
            vfs,
            file_id: None,
        }
    }

    /// Set the current file for queries.
    pub fn set_file(&mut self, path: &Path) {
        // Find the FileId for this path
        for (file_id, vfs_path) in self.vfs.iter() {
            if let Some(abs_path) = vfs_path.as_path() {
                if abs_path == path {
                    self.file_id = Some(file_id);
                    return;
                }
            }
        }
        self.file_id = None;
    }

    /// Query hover info at a position.
    fn hover_at(&self, offset: TextSize) -> Option<HoverResult> {
        let file_id = self.file_id?;
        let analysis = self.host.analysis();

        let config = HoverConfig {
            links_in_hover: false,
            memory_layout: None,
            documentation: false,
            format: ra_ap_ide::HoverDocFormat::Markdown,
            keywords: false,
            max_trait_assoc_items_count: None,
            max_fields_count: None,
            max_enum_variants_count: None,
            max_subst_ty_len: SubstTyLen::Unlimited,
        };

        // Create a zero-width range at the position
        let range = FileRange {
            file_id,
            range: TextRange::new(offset, offset),
        };

        // hover returns RangeInfo<HoverResult>, extract the info
        analysis.hover(&config, range).ok().flatten().map(|ri| ri.info)
    }

    /// Extract type string from hover result.
    fn type_from_hover(&self, hover: &HoverResult) -> Option<String> {
        let markup = &hover.markup;
        let text = markup.as_str();

        // Parse type from hover text (usually in code blocks)
        // Format: ```rust\ntype_info\n```
        if let Some(start) = text.find("```") {
            if let Some(end) = text[start + 3..].find("```") {
                let code_block = &text[start + 3..start + 3 + end];
                // Skip language identifier line
                let type_line = code_block.lines()
                    .skip(1)
                    .next()
                    .unwrap_or(code_block);
                return Some(type_line.trim().to_string());
            }
        }

        // Fallback: look for type patterns
        for line in text.lines() {
            let trimmed: &str = line.trim();
            if trimmed.starts_with("let ") || trimmed.contains(": ") {
                return Some(trimmed.to_string());
            }
        }

        None
    }
}

/// Type oracle trait for querying type information.
/// This is a simpler version without Send+Sync requirements.
pub trait TypeOracleQuery {
    fn is_copy_at(&self, offset: TextSize) -> Option<bool>;
    fn param_mode(&self, call_offset: TextSize, arg_index: usize) -> Option<ParamMode>;
    fn is_type_copy(&self, type_name: &str) -> Option<bool>;
}

impl TypeOracleQuery for RaTypeOracle {
    fn is_copy_at(&self, offset: TextSize) -> Option<bool> {
        let hover = self.hover_at(offset)?;
        let type_str = self.type_from_hover(&hover)?;

        // Check if it's a known Copy type
        Some(is_copy_type(&type_str))
    }

    fn param_mode(&self, _call_offset: TextSize, _arg_index: usize) -> Option<ParamMode> {
        // TODO: Resolve function signature and check parameter types
        // This requires more complex analysis - navigate to definition,
        // parse signature, find parameter at index
        None
    }

    fn is_type_copy(&self, type_name: &str) -> Option<bool> {
        Some(is_copy_type(type_name))
    }
}

/// Check if a type string represents a Copy type.
fn is_copy_type(ty: &str) -> bool {
    let ty = ty.trim();

    // Primitives
    if matches!(ty,
        "i8" | "i16" | "i32" | "i64" | "i128" | "isize" |
        "u8" | "u16" | "u32" | "u64" | "u128" | "usize" |
        "f32" | "f64" | "bool" | "char" | "()"
    ) {
        return true;
    }

    // References are Copy
    if ty.starts_with('&') && !ty.starts_with("&mut") {
        return true;
    }

    // Option/Result of Copy types (simplified)
    if ty.starts_with("Option<") || ty.starts_with("Result<") {
        // Would need to check inner type
        return false; // Conservative
    }

    // Known non-Copy types
    if ty.starts_with("String")
        || ty.starts_with("Vec<")
        || ty.starts_with("Box<")
        || ty.starts_with("Rc<")
        || ty.starts_with("Arc<")
        || ty.starts_with("HashMap<")
        || ty.starts_with("HashSet<")
    {
        return false;
    }

    // Arrays of primitives
    if ty.starts_with('[') && ty.ends_with(']') {
        if let Some(inner) = ty.strip_prefix('[').and_then(|s| s.strip_suffix(']')) {
            // [T; N] format
            if let Some(elem) = inner.split(';').next() {
                return is_copy_type(elem.trim());
            }
        }
    }

    // Tuples of Copy types
    if ty.starts_with('(') && ty.ends_with(')') {
        let inner = &ty[1..ty.len() - 1];
        return inner.split(',').all(|t| is_copy_type(t.trim()));
    }

    // Unknown - conservative default
    false
}

/// Heuristic-based oracle (no rust-analyzer needed).
/// Used as fallback when Layer 2 is not available.
pub struct HeuristicOracle;

impl TypeOracleQuery for HeuristicOracle {
    fn is_copy_at(&self, _offset: TextSize) -> Option<bool> {
        None // Can't determine from position alone
    }

    fn param_mode(&self, _call_offset: TextSize, _arg_index: usize) -> Option<ParamMode> {
        None
    }

    fn is_type_copy(&self, type_name: &str) -> Option<bool> {
        Some(is_copy_type(type_name))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_is_copy_type() {
        assert!(is_copy_type("i32"));
        assert!(is_copy_type("bool"));
        assert!(is_copy_type("&str"));
        assert!(is_copy_type("&i32"));
        assert!(is_copy_type("[i32; 5]"));
        assert!(is_copy_type("(i32, bool)"));

        assert!(!is_copy_type("String"));
        assert!(!is_copy_type("Vec<i32>"));
        assert!(!is_copy_type("Box<i32>"));
        assert!(!is_copy_type("&mut i32"));
    }
}
