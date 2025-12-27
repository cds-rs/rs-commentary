//! AST visitor utilities for ownership analysis.
//!
//! This module provides helpers for walking the syntax tree
//! and extracting ownership-relevant information.

use ra_ap_syntax::{ast, ast::HasName, SyntaxNode};

/// Trait for visiting AST nodes during ownership analysis.
pub trait OwnershipVisitor {
    /// Called when entering a new scope (block, function, etc.)
    fn enter_scope(&mut self);

    /// Called when exiting a scope
    fn exit_scope(&mut self);

    /// Called for each let binding
    fn visit_let_binding(&mut self, name: &str, is_mutable: bool, node: &SyntaxNode);

    /// Called for each variable use
    fn visit_variable_use(&mut self, name: &str, node: &SyntaxNode);

    /// Called for a shared borrow (&x)
    fn visit_shared_borrow(&mut self, name: &str, node: &SyntaxNode);

    /// Called for a mutable borrow (&mut x)
    fn visit_mut_borrow(&mut self, name: &str, node: &SyntaxNode);

    /// Called for a move (variable passed by value)
    fn visit_move(&mut self, name: &str, node: &SyntaxNode);
}

/// Extract the name from a path expression if it's a simple identifier.
pub fn path_to_name(path: &ast::Path) -> Option<String> {
    let segment = path.segment()?;
    let name_ref = segment.name_ref()?;
    Some(name_ref.text().to_string())
}

/// Check if an expression is a simple path (variable reference).
pub fn is_simple_path(expr: &ast::Expr) -> Option<String> {
    match expr {
        ast::Expr::PathExpr(path_expr) => {
            let path = path_expr.path()?;
            // Only simple paths (no ::, no generics)
            if path.qualifier().is_none() {
                path_to_name(&path)
            } else {
                None
            }
        }
        _ => None,
    }
}

/// Determine if an expression context implies a move.
/// This is a heuristic - full accuracy requires type information.
pub fn implies_move(expr: &ast::Expr, _parent: Option<&SyntaxNode>) -> bool {
    // For now, assume function arguments and assignments imply moves
    // unless they're references
    match expr {
        ast::Expr::RefExpr(_) => false, // &x or &mut x doesn't move
        ast::Expr::PathExpr(_) => {
            // A path might move - depends on context and type
            // We'll refine this with type info later
            true
        }
        _ => false,
    }
}

/// Check if a pattern is a reference pattern (&x or &mut x).
pub fn is_ref_pattern(pat: &ast::Pat) -> bool {
    matches!(pat, ast::Pat::RefPat(_))
}

/// Get all identifier patterns from a complex pattern.
pub fn collect_ident_pats(pat: &ast::Pat) -> Vec<(String, bool)> {
    let mut result = Vec::new();
    collect_ident_pats_inner(pat, false, &mut result);
    result
}

fn collect_ident_pats_inner(pat: &ast::Pat, is_mutable: bool, result: &mut Vec<(String, bool)>) {
    match pat {
        ast::Pat::IdentPat(ident_pat) => {
            if let Some(name) = ident_pat.name() {
                let is_mut = is_mutable || ident_pat.mut_token().is_some();
                result.push((name.text().to_string(), is_mut));
            }
        }
        ast::Pat::TuplePat(tuple_pat) => {
            for field in tuple_pat.fields() {
                collect_ident_pats_inner(&field, is_mutable, result);
            }
        }
        ast::Pat::RefPat(ref_pat) => {
            if let Some(inner) = ref_pat.pat() {
                collect_ident_pats_inner(&inner, is_mutable, result);
            }
        }
        ast::Pat::OrPat(or_pat) => {
            for pat in or_pat.pats() {
                collect_ident_pats_inner(&pat, is_mutable, result);
            }
        }
        ast::Pat::ParenPat(paren_pat) => {
            if let Some(inner) = paren_pat.pat() {
                collect_ident_pats_inner(&inner, is_mutable, result);
            }
        }
        ast::Pat::BoxPat(box_pat) => {
            if let Some(inner) = box_pat.pat() {
                collect_ident_pats_inner(&inner, is_mutable, result);
            }
        }
        ast::Pat::SlicePat(slice_pat) => {
            for pat in slice_pat.pats() {
                collect_ident_pats_inner(&pat, is_mutable, result);
            }
        }
        ast::Pat::RecordPat(record_pat) => {
            if let Some(field_list) = record_pat.record_pat_field_list() {
                for field in field_list.fields() {
                    if let Some(pat) = field.pat() {
                        collect_ident_pats_inner(&pat, is_mutable, result);
                    }
                }
            }
        }
        ast::Pat::TupleStructPat(tuple_struct_pat) => {
            for field in tuple_struct_pat.fields() {
                collect_ident_pats_inner(&field, is_mutable, result);
            }
        }
        _ => {}
    }
}
