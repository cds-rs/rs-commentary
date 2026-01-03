//! Macro expression analysis for formatting macros.
//!
//! Handles println!, format!, dbg!, assert!, and similar macros that
//! implicitly borrow their arguments.
//!
//! Uses rust-analyzer's macro expansion via `sema.expand_macro_call()` to
//! accurately find variable references in the expanded code.

use super::{BindingState, Annotation, OwnershipAnalyzer, TransferKind};
use crate::execution::TransferContext;
use ra_ap_syntax::ast;
use ra_ap_syntax::AstNode;

impl OwnershipAnalyzer<'_> {
    /// Visit a macro expression like println!, format!, etc.
    /// Formatting macros implicitly borrow their arguments.
    pub(super) fn visit_macro_expr(&mut self, macro_expr: &ast::MacroExpr) {
        let Some(macro_call) = macro_expr.macro_call() else { return };
        let Some(path) = macro_call.path() else { return };

        let macro_name = path.syntax().text().to_string();

        // Formatting macros that borrow their arguments
        let is_format_macro = matches!(
            macro_name.as_str(),
            "println" | "print" | "eprintln" | "eprint"
            | "format" | "write" | "writeln"
            | "dbg"  // dbg! moves but returns the value
            | "assert" | "assert_eq" | "assert_ne"
            | "debug_assert" | "debug_assert_eq" | "debug_assert_ne"
        );

        if !is_format_macro {
            return;
        }

        // Use semantic macro expansion (requires TypeOracle from cargo project)
        let Some(oracle) = self.type_oracle.as_ref() else {
            // No TypeOracle means no cargo project - skip macro analysis
            return;
        };

        let Some(vars) = oracle.expand_macro_vars(&macro_call) else {
            // Expansion failed - skip this macro
            return;
        };

        // Record borrows for each variable found in the expansion
        for var in vars {
            self.record_macro_borrow(&var, macro_expr, &macro_name);
        }
    }

    /// Record that a variable is borrowed by a formatting macro.
    fn record_macro_borrow(&mut self, name: &str, macro_expr: &ast::MacroExpr, macro_name: &str) {
        // Look up the binding in current scope
        if let Some(binding_id) = self.lookup_binding(name) {
            let range = macro_expr.syntax().text_range();

            // Emit transfer event for macro arg (always SharedBorrow for format macros)
            self.emit_transfer(
                name.to_string(),
                TransferContext::MacroArg { macro_name: macro_name.to_string() },
                TransferKind::SharedBorrow,
                range,
                false,
            );

            // Check if this binding is already a borrow - if so, don't re-borrow
            let is_already_borrow = self
                .bindings
                .get(&binding_id)
                .map(|b| {
                    matches!(
                        b.current_state,
                        BindingState::SharedBorrow { .. } | BindingState::MutBorrow { .. }
                    )
                })
                .unwrap_or(false);

            if is_already_borrow {
                // Just annotate the use, don't change state
                self.annotations.push(Annotation::new(
                    range,
                    name.to_string(),
                    self.bindings.get(&binding_id).unwrap().current_state.clone(),
                    format!("{}: read by {}!", name, macro_name),
                ));
            } else {
                // Record the shared borrow on owned bindings
                self.record_shared_borrow(binding_id, range);

                // Add annotation for the borrow
                self.annotations.push(Annotation::new(
                    range,
                    name.to_string(),
                    BindingState::SharedBorrow { from: binding_id },
                    format!("{}: borrowed by {}!", name, macro_name),
                ));
            }
        }
    }
}
