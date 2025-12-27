//! Type information queries using rust-analyzer.
//!
//! This module provides utilities for querying type information,
//! particularly whether types implement Copy, Drop, etc.

use anyhow::Result;

/// Information about a type relevant to ownership analysis.
#[derive(Debug, Clone, Default)]
pub struct TypeInfo {
    /// The type name as a string
    pub name: String,
    /// Whether the type implements Copy
    pub is_copy: bool,
    /// Whether the type implements Drop
    pub is_drop: bool,
    /// Whether the type implements Clone
    pub is_clone: bool,
}

impl TypeInfo {
    /// Create a new TypeInfo with default values.
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            is_copy: false,
            is_drop: false,
            is_clone: false,
        }
    }

    /// Create TypeInfo for a known Copy type.
    pub fn copy_type(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            is_copy: true,
            is_drop: false,
            is_clone: true,
        }
    }

    /// Create TypeInfo for a known owned type (like String, Vec).
    pub fn owned_type(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            is_copy: false,
            is_drop: true,
            is_clone: true,
        }
    }
}

/// Known primitive types that implement Copy.
pub const COPY_PRIMITIVES: &[&str] = &[
    "bool", "char", "i8", "i16", "i32", "i64", "i128", "isize", "u8", "u16", "u32", "u64", "u128",
    "usize", "f32", "f64",
];

/// Check if a type name is a known Copy primitive.
pub fn is_copy_primitive(name: &str) -> bool {
    COPY_PRIMITIVES.contains(&name)
}

/// Known types that own heap data (non-Copy).
pub const OWNED_HEAP_TYPES: &[&str] = &["String", "Vec", "Box", "Rc", "Arc", "HashMap", "HashSet"];

/// Check if a type name is a known heap-owning type.
pub fn is_heap_type(name: &str) -> bool {
    OWNED_HEAP_TYPES.iter().any(|&t| name.starts_with(t))
}

/// Heuristically determine type info from a type name.
/// This is a fallback when we don't have full type resolution.
pub fn infer_type_info(type_name: &str) -> TypeInfo {
    // Strip references and mutability
    let base_type = type_name
        .trim_start_matches('&')
        .trim_start_matches("mut ")
        .trim();

    // Check for array types like [u32; 5]
    if base_type.starts_with('[') && base_type.ends_with(']') {
        // Arrays of Copy types are Copy
        if let Some(inner) = base_type.strip_prefix('[').and_then(|s| s.strip_suffix(']')) {
            let element_type = inner.split(';').next().unwrap_or("").trim();
            if is_copy_primitive(element_type) {
                return TypeInfo::copy_type(base_type);
            }
        }
    }

    // Check for reference types
    if type_name.starts_with('&') {
        // References are Copy
        return TypeInfo::copy_type(type_name);
    }

    // Check primitives
    if is_copy_primitive(base_type) {
        return TypeInfo::copy_type(base_type);
    }

    // Check heap types
    if is_heap_type(base_type) {
        return TypeInfo::owned_type(base_type);
    }

    // Default: assume non-Copy
    TypeInfo::new(base_type)
}

/// Query interface for type information.
/// This will be implemented with rust-analyzer integration.
pub trait TypeQuery {
    /// Get type info for a binding at a given position.
    fn type_at(&self, file: &str, offset: u32) -> Result<Option<TypeInfo>>;

    /// Check if a type implements a specific trait.
    fn implements_trait(&self, type_name: &str, trait_name: &str) -> Result<bool>;
}

/// Stub implementation that uses heuristics.
pub struct HeuristicTypeQuery;

impl TypeQuery for HeuristicTypeQuery {
    fn type_at(&self, _file: &str, _offset: u32) -> Result<Option<TypeInfo>> {
        // Without full analysis, we can't determine types from position
        Ok(None)
    }

    fn implements_trait(&self, type_name: &str, trait_name: &str) -> Result<bool> {
        let info = infer_type_info(type_name);
        Ok(match trait_name {
            "Copy" => info.is_copy,
            "Drop" => info.is_drop,
            "Clone" => info.is_clone,
            _ => false,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_copy_primitives() {
        assert!(is_copy_primitive("u32"));
        assert!(is_copy_primitive("bool"));
        assert!(!is_copy_primitive("String"));
    }

    #[test]
    fn test_infer_type_info() {
        let info = infer_type_info("u32");
        assert!(info.is_copy);

        let info = infer_type_info("String");
        assert!(!info.is_copy);
        assert!(info.is_drop);

        let info = infer_type_info("[u32; 5]");
        assert!(info.is_copy);

        let info = infer_type_info("&str");
        assert!(info.is_copy);
    }
}
