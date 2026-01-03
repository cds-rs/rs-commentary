//! Transfer analysis - what's moved/borrowed at call sites and macros.
//!
//! Analyzes function call arguments and macro invocations to determine ownership transfers:
//! - Move: ownership transfers to callee
//! - SharedBorrow: &T reference passed
//! - MutBorrow: &mut T reference passed
//! - Copy: value copied (no ownership change)

use ra_ap_syntax::ast::{self, HasArgList};
use ra_ap_syntax::AstNode;

/// Context in which a transfer occurs.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TransferContext {
    /// Transfer as a function argument: `foo(x)`
    FunctionArg { callee: String },
    /// Transfer as a method receiver: `x.foo()`
    MethodReceiver { method: String },
    /// Transfer inside a macro: `println!("{x}")`
    MacroArg { macro_name: String },
    /// Transfer via let binding: `let y = x;`
    LetBinding { to: String },
}

impl TransferContext {
    /// Get the target name (function, method, macro, or binding).
    pub fn target_name(&self) -> &str {
        match self {
            TransferContext::FunctionArg { callee } => callee,
            TransferContext::MethodReceiver { method } => method,
            TransferContext::MacroArg { macro_name } => macro_name,
            TransferContext::LetBinding { to } => to,
        }
    }
}

/// A transfer happening at a call site (legacy, for standalone analysis).
#[derive(Debug, Clone)]
pub struct CallTransfer {
    /// Variable being transferred.
    pub variable: String,
    /// Kind of transfer.
    pub kind: TransferKind,
    /// Column position in source (for annotation).
    pub column: u32,
}

/// Kind of ownership transfer.
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
fn analyze_argument(expr: &ast::Expr, _copy_types: &[&str]) -> Option<CallTransfer> {
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

        // Plain variable - assume move (conservative; actual analysis uses TypeOracle)
        ast::Expr::PathExpr(path) => {
            let name = path.path()?.syntax().text().to_string();

            Some(CallTransfer {
                variable: name,
                kind: TransferKind::Move,
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
    fn test_plain_path_transfer() {
        let call = parse_call("foo(x)");
        let transfers = analyze_call_transfers(&call, &[]);

        // Without semantic analysis, plain paths are conservatively Move
        assert_eq!(transfers.len(), 1);
        assert_eq!(transfers[0].kind, TransferKind::Move);
    }

    #[test]
    fn test_multiple_args() {
        let call = parse_call("foo(&s, x, &mut y)");
        let transfers = analyze_call_transfers(&call, &[]);

        assert_eq!(transfers.len(), 3);
        assert_eq!(transfers[0].kind, TransferKind::SharedBorrow);
        assert_eq!(transfers[1].kind, TransferKind::Move); // x - Move without semantics
        assert_eq!(transfers[2].kind, TransferKind::MutBorrow);
    }
}
