//! Call transfer analysis - what's moved/borrowed at each call site.
//!
//! Analyzes function call arguments to determine ownership transfers:
//! - Move: ownership transfers to callee
//! - SharedBorrow: &T reference passed
//! - MutBorrow: &mut T reference passed
//! - Copy: value copied (no ownership change)

use ra_ap_syntax::ast::{self, HasArgList};
use ra_ap_syntax::AstNode;

/// A transfer happening at a call site.
#[derive(Debug, Clone)]
pub struct CallTransfer {
    /// Variable being transferred.
    pub variable: String,
    /// Kind of transfer.
    pub kind: TransferKind,
    /// Column position in source (for annotation).
    pub column: u32,
}

/// Kind of ownership transfer at a call.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TransferKind {
    /// Ownership moves to callee.
    Move,
    /// Shared borrow (&T).
    SharedBorrow,
    /// Mutable borrow (&mut T).
    MutBorrow,
    /// Copy (no ownership change).
    Copy,
}

impl TransferKind {
    pub fn symbol(&self) -> &'static str {
        match self {
            TransferKind::Move => "→",
            TransferKind::SharedBorrow => "&",
            TransferKind::MutBorrow => "&mut",
            TransferKind::Copy => "⊕",
        }
    }

    pub fn description(&self) -> &'static str {
        match self {
            TransferKind::Move => "moved",
            TransferKind::SharedBorrow => "borrowed",
            TransferKind::MutBorrow => "mutably borrowed",
            TransferKind::Copy => "copied",
        }
    }
}

/// Analyze transfers at a call expression.
pub fn analyze_call_transfers(call: &ast::CallExpr, copy_types: &[&str]) -> Vec<CallTransfer> {
    let mut transfers = Vec::new();

    if let Some(arg_list) = call.arg_list() {
        for arg in arg_list.args() {
            if let Some(transfer) = analyze_argument(&arg, copy_types) {
                transfers.push(transfer);
            }
        }
    }

    transfers
}

/// Analyze transfers at a method call expression.
pub fn analyze_method_call_transfers(
    call: &ast::MethodCallExpr,
    copy_types: &[&str],
) -> Vec<CallTransfer> {
    let mut transfers = Vec::new();

    // Receiver is implicitly borrowed/moved
    if let Some(receiver) = call.receiver() {
        if let Some(transfer) = analyze_argument(&receiver, copy_types) {
            transfers.push(transfer);
        }
    }

    if let Some(arg_list) = call.arg_list() {
        for arg in arg_list.args() {
            if let Some(transfer) = analyze_argument(&arg, copy_types) {
                transfers.push(transfer);
            }
        }
    }

    transfers
}

/// Analyze a single argument expression.
fn analyze_argument(expr: &ast::Expr, copy_types: &[&str]) -> Option<CallTransfer> {
    let column = expr
        .syntax()
        .text_range()
        .start()
        .into();

    match expr {
        // &x - shared borrow
        ast::Expr::RefExpr(ref_expr) => {
            if ref_expr.mut_token().is_some() {
                // &mut x
                if let Some(name) = extract_var_name(ref_expr.expr().as_ref()?) {
                    return Some(CallTransfer {
                        variable: name,
                        kind: TransferKind::MutBorrow,
                        column,
                    });
                }
            } else {
                // &x
                if let Some(name) = extract_var_name(ref_expr.expr().as_ref()?) {
                    return Some(CallTransfer {
                        variable: name,
                        kind: TransferKind::SharedBorrow,
                        column,
                    });
                }
            }
            None
        }

        // Plain variable - move or copy
        ast::Expr::PathExpr(path) => {
            let name = path.path()?.syntax().text().to_string();

            // Check if it's a Copy type (simple heuristic)
            let kind = if is_likely_copy(&name, copy_types) {
                TransferKind::Copy
            } else {
                TransferKind::Move
            };

            Some(CallTransfer {
                variable: name,
                kind,
                column,
            })
        }

        // Skip literals, they're always Copy
        ast::Expr::Literal(_) => None,

        // For other expressions, try to extract the base variable
        _ => None,
    }
}

/// Extract variable name from an expression.
fn extract_var_name(expr: &ast::Expr) -> Option<String> {
    match expr {
        ast::Expr::PathExpr(path) => {
            Some(path.path()?.syntax().text().to_string())
        }
        _ => None,
    }
}

/// Heuristic: is this variable likely a Copy type?
fn is_likely_copy(name: &str, copy_types: &[&str]) -> bool {
    // Check explicit copy types list
    if copy_types.contains(&name) {
        return true;
    }

    // Simple names that are conventionally Copy
    matches!(
        name,
        "i" | "j" | "k" | "n" | "x" | "y" | "z" | "len" | "count" | "index" | "idx"
    )
}

/// Format transfers for display.
pub fn format_transfers(transfers: &[CallTransfer]) -> String {
    if transfers.is_empty() {
        return String::new();
    }

    transfers
        .iter()
        .map(|t| format!("{} {}", t.kind.symbol(), t.variable))
        .collect::<Vec<_>>()
        .join(", ")
}

#[cfg(test)]
mod tests {
    use super::*;
    use ra_ap_syntax::SourceFile;

    fn parse_call(source: &str) -> ast::CallExpr {
        let wrapped = format!("fn test() {{ {}; }}", source);
        let parse = SourceFile::parse(&wrapped, ra_ap_syntax::Edition::Edition2021);
        let file = parse.tree();

        // Find the call expression
        file.syntax()
            .descendants()
            .find_map(ast::CallExpr::cast)
            .expect("no call found")
    }

    #[test]
    fn test_move_transfer() {
        let call = parse_call("foo(s)");
        let transfers = analyze_call_transfers(&call, &[]);

        assert_eq!(transfers.len(), 1);
        assert_eq!(transfers[0].variable, "s");
        assert_eq!(transfers[0].kind, TransferKind::Move);
    }

    #[test]
    fn test_shared_borrow_transfer() {
        let call = parse_call("foo(&s)");
        let transfers = analyze_call_transfers(&call, &[]);

        assert_eq!(transfers.len(), 1);
        assert_eq!(transfers[0].variable, "s");
        assert_eq!(transfers[0].kind, TransferKind::SharedBorrow);
    }

    #[test]
    fn test_mut_borrow_transfer() {
        let call = parse_call("foo(&mut s)");
        let transfers = analyze_call_transfers(&call, &[]);

        assert_eq!(transfers.len(), 1);
        assert_eq!(transfers[0].variable, "s");
        assert_eq!(transfers[0].kind, TransferKind::MutBorrow);
    }

    #[test]
    fn test_copy_transfer() {
        let call = parse_call("foo(x)");
        let transfers = analyze_call_transfers(&call, &[]);

        // x is conventionally a Copy type name
        assert_eq!(transfers.len(), 1);
        assert_eq!(transfers[0].kind, TransferKind::Copy);
    }

    #[test]
    fn test_multiple_args() {
        let call = parse_call("foo(&s, x, &mut y)");
        let transfers = analyze_call_transfers(&call, &[]);

        assert_eq!(transfers.len(), 3);
        assert_eq!(transfers[0].kind, TransferKind::SharedBorrow);
        assert_eq!(transfers[1].kind, TransferKind::Copy); // x
        assert_eq!(transfers[2].kind, TransferKind::MutBorrow);
    }
}
