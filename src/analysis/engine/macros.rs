//! Macro expression analysis for formatting macros.
//!
//! Handles println!, format!, dbg!, assert!, and similar macros that
//! implicitly borrow their arguments.

use super::{BindingState, Annotation, OwnershipAnalyzer};
use ra_ap_syntax::ast;
use ra_ap_syntax::AstNode;
use std::collections::HashSet;

impl OwnershipAnalyzer {
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

        // Parse token tree to find variable references
        let Some(token_tree) = macro_call.token_tree() else { return };
        let text = token_tree.syntax().text().to_string();

        // Track variables we've already recorded to avoid duplicates
        let mut recorded: HashSet<String> = HashSet::new();

        // Parse inline format args and traditional format args
        self.parse_inline_format_args(&text, macro_expr, &macro_name, &mut recorded);
        self.parse_traditional_format_args(&text, macro_expr, &macro_name, &mut recorded);
    }

    /// Parse inline format args like {rect1:#?} inside the format string.
    /// Format: {[name][:format_spec]} where name is an identifier.
    fn parse_inline_format_args(
        &mut self,
        text: &str,
        macro_expr: &ast::MacroExpr,
        macro_name: &str,
        recorded: &mut HashSet<String>,
    ) {
        let mut in_string = false;
        let mut in_format_placeholder = false;
        let mut format_ident = String::new();

        let chars: Vec<char> = text.chars().collect();
        let mut i = 0;

        while i < chars.len() {
            let ch = chars[i];

            // Handle escape sequences in strings
            if in_string && ch == '\\' && i + 1 < chars.len() {
                i += 2; // Skip escaped char
                continue;
            }

            match ch {
                '"' => {
                    in_string = !in_string;
                    in_format_placeholder = false;
                    format_ident.clear();
                }
                '{' if in_string => {
                    // Check for escaped {{
                    if i + 1 < chars.len() && chars[i + 1] == '{' {
                        i += 2;
                        continue;
                    }
                    in_format_placeholder = true;
                    format_ident.clear();
                }
                '}' if in_string && in_format_placeholder => {
                    // End of format placeholder - record if we found an identifier
                    if !format_ident.is_empty() && !recorded.contains(&format_ident) {
                        self.record_macro_borrow(&format_ident, macro_expr, macro_name);
                        recorded.insert(format_ident.clone());
                    }
                    in_format_placeholder = false;
                    format_ident.clear();
                }
                ':' if in_format_placeholder => {
                    // Format spec starts - record the identifier we collected
                    if !format_ident.is_empty() && !recorded.contains(&format_ident) {
                        let first = format_ident.chars().next().unwrap();
                        if first.is_alphabetic() || first == '_' {
                            self.record_macro_borrow(&format_ident, macro_expr, macro_name);
                            recorded.insert(format_ident.clone());
                        }
                    }
                    format_ident.clear();
                    in_format_placeholder = false;
                }
                c if in_format_placeholder && (c.is_alphanumeric() || c == '_') => {
                    format_ident.push(c);
                }
                c if in_format_placeholder && !c.is_alphanumeric() && c != '_' => {
                    // Non-ident char in placeholder (might be positional like {0})
                    // Only record if it looks like an identifier (starts with letter or _)
                    let _ = c; // Silence unused warning
                    if !format_ident.is_empty() {
                        let first = format_ident.chars().next().unwrap();
                        if (first.is_alphabetic() || first == '_')
                            && !recorded.contains(&format_ident)
                        {
                            self.record_macro_borrow(&format_ident, macro_expr, macro_name);
                            recorded.insert(format_ident.clone());
                        }
                    }
                    format_ident.clear();
                }
                _ => {}
            }
            i += 1;
        }
    }

    /// Parse traditional format args after the format string: println!("{}", x).
    fn parse_traditional_format_args(
        &mut self,
        text: &str,
        macro_expr: &ast::MacroExpr,
        macro_name: &str,
        recorded: &mut HashSet<String>,
    ) {
        let mut found_comma = false;
        let mut current_ident = String::new();
        let mut in_string = false;

        for ch in text.chars() {
            match ch {
                '"' => in_string = !in_string,
                ',' if !in_string => {
                    if !current_ident.is_empty() && found_comma && !recorded.contains(&current_ident)
                    {
                        self.record_macro_borrow(&current_ident, macro_expr, macro_name);
                        recorded.insert(current_ident.clone());
                    }
                    current_ident.clear();
                    found_comma = true;
                }
                c if c.is_alphanumeric() || c == '_' => {
                    if !in_string {
                        current_ident.push(c);
                    }
                }
                _ => {
                    if !current_ident.is_empty()
                        && found_comma
                        && !in_string
                        && !recorded.contains(&current_ident)
                    {
                        self.record_macro_borrow(&current_ident, macro_expr, macro_name);
                        recorded.insert(current_ident.clone());
                    }
                    current_ident.clear();
                }
            }
        }

        // Handle last identifier
        if !current_ident.is_empty() && found_comma && !recorded.contains(&current_ident) {
            self.record_macro_borrow(&current_ident, macro_expr, macro_name);
        }
    }

    /// Record that a variable is borrowed by a formatting macro.
    fn record_macro_borrow(&mut self, name: &str, macro_expr: &ast::MacroExpr, macro_name: &str) {
        // Look up the binding in current scope
        if let Some(binding_id) = self.lookup_binding(name) {
            let range = macro_expr.syntax().text_range();

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
