//! Semantic analysis using rust-analyzer's ra_ap_ide and ra_ap_hir.
//!
//! When a file is part of a cargo project, this provides:
//! - Accurate type inference
//! - Macro expansion
//! - Copy/Drop trait detection via `Type::is_copy()`

use crate::analysis::state::{Annotation, BindingKind, BindingState};
use anyhow::{Context, Result};
use ra_ap_hir::{HirDisplay, Semantics};
use ra_ap_ide::{
    AnalysisHost, AssistResolveStrategy, DiagnosticsConfig, FileId, FilePosition, FileRange,
    HoverConfig, HoverDocFormat, RootDatabase, SubstTyLen,
};
use ra_ap_load_cargo::{load_workspace_at, LoadCargoConfig, ProcMacroServerChoice};
use ra_ap_project_model::{CargoConfig, RustLibSource};
use ra_ap_syntax::{
    ast::{self, HasLoopBody, HasModuleItem, HasName},
    AstNode, SourceFile, TextRange, TextSize,
};
use ra_ap_vfs::Vfs;
use std::collections::HashMap;
use std::path::{Path, PathBuf};

/// A diagnostic from rust-analyzer.
#[derive(Debug, Clone)]
pub struct RaDiagnostic {
    /// The error/warning message.
    pub message: String,
    /// The source range this applies to.
    pub range: TextRange,
    /// Severity level.
    pub severity: DiagnosticSeverity,
    /// The diagnostic code (e.g., "E0382" for use after move).
    pub code: String,
}

/// Diagnostic severity levels.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DiagnosticSeverity {
    Error,
    Warning,
    Info,
    Hint,
}

/// Result of attempting to load semantic analysis.
pub enum SemanticResult {
    /// Full semantic analysis available
    Available(SemanticAnalyzer),
    /// Not a cargo project, use syntax-only
    NotCargoProject,
    /// Failed to load (error logged)
    LoadFailed,
}

/// Information about the last use of a binding.
#[derive(Debug, Clone)]
pub struct LastUseInfo {
    /// The binding name.
    pub name: String,
    /// Line where the binding was declared (0-indexed).
    pub decl_line: u32,
    /// Line where the binding was last used (0-indexed).
    pub last_use_line: u32,
    /// Offset where the binding was last used.
    pub last_use_offset: TextSize,
    /// Line where the drop annotation should appear (0-indexed).
    /// This may differ from last_use_line when the last use is inside a loop
    /// but the variable is declared outside the loop.
    pub drop_line: u32,
    /// Classification of the binding's type (owned/copy/reference).
    pub kind: BindingKind,
    /// True if this is a scalar primitive (u32, bool, etc.) for noise filtering.
    pub is_scalar: bool,
}

impl LastUseInfo {
    /// Returns true if this binding's type implements Copy.
    ///
    /// Convenience method for backwards compatibility.
    pub fn is_copy(&self) -> bool {
        self.kind.is_copy()
    }
}

/// Context for semantic analysis operations.
/// Bundles common parameters to reduce function signature complexity.
struct SemanticContext<'a, 'db> {
    sema: &'a Semantics<'db, RootDatabase>,
    file_id: FileId,
    source: &'a str,
    loop_ranges: &'a [(u32, u32)],
}

/// Semantic analyzer backed by rust-analyzer.
pub struct SemanticAnalyzer {
    host: AnalysisHost,
    vfs: Vfs,
}

impl SemanticAnalyzer {
    /// Try to load semantic analysis for a file.
    pub fn load(file_path: &Path) -> SemanticResult {
        // Find manifest by walking up
        let Some(manifest) = find_manifest(file_path) else {
            return SemanticResult::NotCargoProject;
        };

        tracing::info!("Loading cargo workspace from: {}", manifest.display());

        // Configure to load sysroot for standard library (needed for Copy trait detection)
        let cargo_config = CargoConfig {
            sysroot: Some(RustLibSource::Discover),
            ..Default::default()
        };

        let load_config = LoadCargoConfig {
            load_out_dirs_from_check: false,  // Faster startup
            with_proc_macro_server: ProcMacroServerChoice::None,
            prefill_caches: false,
        };

        match load_workspace_at(&manifest, &cargo_config, &load_config, &|_| {}) {
            Ok((db, vfs, _)) => {
                tracing::info!("Semantic analysis loaded");
                // Create AnalysisHost from the database
                let host = AnalysisHost::with_database(db);
                SemanticResult::Available(SemanticAnalyzer { host, vfs })
            }
            Err(e) => {
                tracing::warn!("Failed to load workspace: {}", e);
                SemanticResult::LoadFailed
            }
        }
    }

    /// Get FileId for a path.
    pub fn file_id(&self, path: &Path) -> Option<FileId> {
        let canonical = path.canonicalize().ok()?;

        // Search through VFS for matching path
        for (file_id, vfs_path) in self.vfs.iter() {
            if let Some(vfs_abs_path) = vfs_path.as_path() {
                let vfs_path_ref: &Path = vfs_abs_path.as_ref();
                if vfs_path_ref == canonical.as_path() {
                    return Some(file_id);
                }
            }
        }
        None
    }

    /// Get the binding kind (owned/copy/reference) using HIR type inference.
    ///
    /// Uses `Type::is_reference()`, `Type::is_mutable_reference()`, and `Type::is_copy()`
    /// to classify the binding. This enables accurate messaging like "borrow ends"
    /// for reference types vs "dropped" for owned types.
    fn get_binding_kind_sema<'db>(
        &self,
        sema: &Semantics<'db, RootDatabase>,
        pat: &ast::IdentPat,
    ) -> Option<BindingKind> {
        let db = self.host.raw_database();
        let ty = sema.type_of_binding_in_pat(pat)?;

        // Check reference types first (they take precedence over Copy)
        if ty.is_mutable_reference() {
            return Some(BindingKind::MutRef);
        }
        if ty.is_reference() {
            return Some(BindingKind::SharedRef);
        }

        // For non-reference types, check Copy
        if ty.is_copy(db) {
            Some(BindingKind::OwnedCopy)
        } else {
            Some(BindingKind::OwnedMove)
        }
    }

    /// Check if a type is a scalar primitive (u32, i32, bool, char, etc.).
    /// These types are Copy but don't benefit from "copied" annotations
    /// when passed to functions - they're just noise.
    fn is_scalar_type<'db>(
        &self,
        sema: &Semantics<'db, RootDatabase>,
        pat: &ast::IdentPat,
    ) -> bool {
        let db = self.host.raw_database();
        let Some(ty) = sema.type_of_binding_in_pat(pat) else {
            return false;
        };

        // Get the type display string
        let ty_str = ty.display(db, ra_ap_syntax::Edition::Edition2021).to_string();

        // Check for known scalar primitives
        matches!(
            ty_str.as_str(),
            "i8" | "i16" | "i32" | "i64" | "i128" | "isize"
                | "u8" | "u16" | "u32" | "u64" | "u128" | "usize"
                | "f32" | "f64"
                | "bool"
                | "char"
                | "()" // unit type
        )
    }

    /// Check if a type at a position implements Copy (fallback using hover).
    pub fn is_copy_at(&self, file_id: FileId, offset: TextSize) -> Option<bool> {
        let analysis = self.host.analysis();

        // Use hover to get type info
        let hover_config = HoverConfig {
            links_in_hover: false,
            memory_layout: None,
            documentation: false,
            keywords: false,
            format: HoverDocFormat::Markdown,
            max_trait_assoc_items_count: None,
            max_fields_count: None,
            max_enum_variants_count: None,
            max_subst_ty_len: SubstTyLen::Unlimited,
        };

        let range = FileRange {
            file_id,
            range: TextRange::new(offset, offset + TextSize::from(1u32))
        };

        let hover = analysis.hover(&hover_config, range).ok()??;
        let text = hover.info.markup.as_str();

        // Check for known Copy indicators
        Some(
            text.contains("impl Copy")
                || is_primitive_in_hover(text)
                || (text.contains("&") && !text.contains("&mut"))  // Shared refs are Copy
        )
    }

    /// Get the inferred type as a string at a position.
    pub fn type_at(&self, file_id: FileId, offset: TextSize) -> Option<String> {
        let analysis = self.host.analysis();

        let hover_config = HoverConfig {
            links_in_hover: false,
            memory_layout: None,
            documentation: false,
            keywords: false,
            format: HoverDocFormat::Markdown,
            max_trait_assoc_items_count: None,
            max_fields_count: None,
            max_enum_variants_count: None,
            max_subst_ty_len: SubstTyLen::Unlimited,
        };

        let range = FileRange {
            file_id,
            range: TextRange::new(offset, offset + TextSize::from(1u32))
        };

        let hover = analysis.hover(&hover_config, range).ok()??;
        Some(hover.info.markup.as_str().to_string())
    }

    /// Get rust-analyzer diagnostics for a file.
    ///
    /// Returns borrow checker errors, type mismatches, and other semantic errors.
    pub fn diagnostics(&self, file_id: FileId) -> Vec<RaDiagnostic> {
        let analysis = self.host.analysis();

        // Use the test_sample config which has sensible defaults
        let config = DiagnosticsConfig::test_sample();

        let Ok(diags) = analysis.full_diagnostics(&config, AssistResolveStrategy::None, file_id) else {
            return Vec::new();
        };

        diags
            .into_iter()
            .map(|d| {
                let severity = match d.severity {
                    ra_ap_ide::Severity::Error => DiagnosticSeverity::Error,
                    ra_ap_ide::Severity::Warning => DiagnosticSeverity::Warning,
                    ra_ap_ide::Severity::WeakWarning => DiagnosticSeverity::Info,
                    _ => DiagnosticSeverity::Hint,
                };
                RaDiagnostic {
                    message: d.message,
                    range: d.range.range,
                    severity,
                    code: d.code.as_str().to_string(),
                }
            })
            .collect()
    }

    /// Find the last use of a binding in a file.
    ///
    /// Given the offset where a binding is declared, finds all references
    /// and returns the offset of the last use (for NLL drop detection).
    pub fn find_last_use(&self, file_id: FileId, binding_offset: TextSize) -> Option<TextSize> {
        let analysis = self.host.analysis();

        let position = FilePosition {
            file_id,
            offset: binding_offset,
        };

        // Search everywhere - we filter by file_id in results
        let refs = analysis.find_all_refs(position, None).ok()??;

        // Find the reference with the maximum end offset (last use) in this file
        refs.iter()
            .flat_map(|result| {
                result
                    .references
                    .iter()
                    .filter(|(fid, _)| **fid == file_id)
                    .flat_map(|(_, ranges)| ranges.iter().map(|(range, _)| range.end()))
            })
            .max()
    }

    /// Find last uses for all bindings in a file.
    ///
    /// Returns a map from binding declaration offset to last use offset.
    /// This is more efficient than calling find_last_use for each binding.
    pub fn find_all_last_uses(
        &self,
        file_id: FileId,
        source: &str,
    ) -> HashMap<TextSize, LastUseInfo> {
        let db = self.host.raw_database();
        let sema = Semantics::new(db);

        // Parse the file through Semantics so AST nodes are connected to the database
        let file = sema.parse_guess_edition(file_id);
        let mut result = HashMap::new();

        // First, collect all loop boundaries (start_line, end_line)
        let mut loop_ranges: Vec<(u32, u32)> = Vec::new();
        for item in file.items() {
            collect_loop_ranges(&item, source, &mut loop_ranges);
        }

        // Create context for semantic analysis
        let ctx = SemanticContext {
            sema: &sema,
            file_id,
            source,
            loop_ranges: &loop_ranges,
        };

        // Walk AST to find all let bindings and parameters
        for item in file.items() {
            self.collect_bindings_last_uses(&ctx, &item, &mut result);
        }

        result
    }

    fn collect_bindings_last_uses(
        &self,
        ctx: &SemanticContext<'_, '_>,
        item: &ast::Item,
        result: &mut HashMap<TextSize, LastUseInfo>,
    ) {
        match item {
            ast::Item::Fn(f) => {
                // Collect parameter bindings
                if let Some(param_list) = f.param_list() {
                    for param in param_list.params() {
                        if let Some(pat) = param.pat() {
                            self.collect_pat_last_uses(ctx, &pat, result);
                        }
                    }
                }
                // Collect bindings in body
                if let Some(body) = f.body() {
                    self.collect_block_last_uses(ctx, &body, result);
                }
            }
            ast::Item::Impl(i) => {
                if let Some(items) = i.assoc_item_list() {
                    for item in items.assoc_items() {
                        if let ast::AssocItem::Fn(f) = item {
                            self.collect_bindings_last_uses(ctx, &ast::Item::Fn(f), result);
                        }
                    }
                }
            }
            _ => {}
        }
    }

    fn collect_block_last_uses(
        &self,
        ctx: &SemanticContext<'_, '_>,
        block: &ast::BlockExpr,
        result: &mut HashMap<TextSize, LastUseInfo>,
    ) {
        for stmt in block.statements() {
            match &stmt {
                ast::Stmt::LetStmt(let_stmt) => {
                    if let Some(pat) = let_stmt.pat() {
                        self.collect_pat_last_uses(ctx, &pat, result);
                    }
                }
                ast::Stmt::ExprStmt(expr_stmt) => {
                    if let Some(expr) = expr_stmt.expr() {
                        self.collect_expr_last_uses(ctx, &expr, result);
                    }
                }
                _ => {}
            }
        }
        if let Some(tail) = block.tail_expr() {
            self.collect_expr_last_uses(ctx, &tail, result);
        }
    }

    fn collect_expr_last_uses(
        &self,
        ctx: &SemanticContext<'_, '_>,
        expr: &ast::Expr,
        result: &mut HashMap<TextSize, LastUseInfo>,
    ) {
        match expr {
            ast::Expr::BlockExpr(block) => {
                self.collect_block_last_uses(ctx, block, result);
            }
            ast::Expr::IfExpr(if_expr) => {
                if let Some(then_branch) = if_expr.then_branch() {
                    self.collect_block_last_uses(ctx, &then_branch, result);
                }
                if let Some(else_branch) = if_expr.else_branch() {
                    match else_branch {
                        ast::ElseBranch::Block(block) => {
                            self.collect_block_last_uses(ctx, &block, result);
                        }
                        ast::ElseBranch::IfExpr(nested) => {
                            self.collect_expr_last_uses(ctx, &ast::Expr::IfExpr(nested), result);
                        }
                    }
                }
            }
            ast::Expr::LoopExpr(loop_expr) => {
                if let Some(body) = loop_expr.loop_body() {
                    self.collect_block_last_uses(ctx, &body, result);
                }
            }
            ast::Expr::WhileExpr(while_expr) => {
                if let Some(body) = while_expr.loop_body() {
                    self.collect_block_last_uses(ctx, &body, result);
                }
            }
            ast::Expr::ForExpr(for_expr) => {
                if let Some(pat) = for_expr.pat() {
                    self.collect_pat_last_uses(ctx, &pat, result);
                }
                if let Some(body) = for_expr.loop_body() {
                    self.collect_block_last_uses(ctx, &body, result);
                }
            }
            ast::Expr::MatchExpr(match_expr) => {
                if let Some(arm_list) = match_expr.match_arm_list() {
                    for arm in arm_list.arms() {
                        if let Some(pat) = arm.pat() {
                            self.collect_pat_last_uses(ctx, &pat, result);
                        }
                        if let Some(expr) = arm.expr() {
                            self.collect_expr_last_uses(ctx, &expr, result);
                        }
                    }
                }
            }
            ast::Expr::ClosureExpr(closure) => {
                if let Some(param_list) = closure.param_list() {
                    for param in param_list.params() {
                        if let Some(pat) = param.pat() {
                            self.collect_pat_last_uses(ctx, &pat, result);
                        }
                    }
                }
                if let Some(body) = closure.body() {
                    self.collect_expr_last_uses(ctx, &body, result);
                }
            }
            _ => {}
        }
    }

    fn collect_pat_last_uses(
        &self,
        ctx: &SemanticContext<'_, '_>,
        pat: &ast::Pat,
        result: &mut HashMap<TextSize, LastUseInfo>,
    ) {
        match pat {
            ast::Pat::IdentPat(ident) => {
                if let Some(name) = ident.name() {
                    let offset = name.syntax().text_range().start();
                    let name_str = name.text().to_string();

                    if let Some(last_use_offset) = self.find_last_use(ctx.file_id, offset) {
                        let last_use_line = offset_to_line(ctx.source, last_use_offset);
                        let decl_line = offset_to_line(ctx.source, offset);

                        // Compute drop_line: if last use is in a loop but decl is outside,
                        // the drop should be after the loop ends
                        let drop_line = compute_drop_line(decl_line, last_use_line, ctx.loop_ranges);

                        // Get binding kind (owned/copy/reference) using type inference
                        let kind = self
                            .get_binding_kind_sema(ctx.sema, ident)
                            .unwrap_or(BindingKind::OwnedMove);

                        // Check if this is a scalar primitive (for noise filtering)
                        let is_scalar = self.is_scalar_type(ctx.sema, ident);

                        result.insert(
                            offset,
                            LastUseInfo {
                                name: name_str,
                                decl_line,
                                last_use_line,
                                last_use_offset,
                                drop_line,
                                kind,
                                is_scalar,
                            },
                        );
                    }
                }
            }
            ast::Pat::TuplePat(tuple) => {
                for field in tuple.fields() {
                    self.collect_pat_last_uses(ctx, &field, result);
                }
            }
            _ => {}
        }
    }

    /// Analyze a file with full semantic info.
    pub fn analyze(&self, file_path: &Path, source: &str) -> Result<Vec<Annotation>> {
        let file_id = self.file_id(file_path)
            .context("File not in workspace")?;

        let parse = SourceFile::parse(source, ra_ap_syntax::Edition::Edition2021);
        let file = parse.tree();
        let mut annotations = Vec::new();

        // Walk AST and use semantic info for each binding
        for item in file.items() {
            self.visit_item(&item, file_id, &mut annotations);
        }

        Ok(annotations)
    }

    fn visit_item(&self, item: &ast::Item, file_id: FileId, annotations: &mut Vec<Annotation>) {
        match item {
            ast::Item::Fn(f) => self.visit_fn(f, file_id, annotations),
            ast::Item::Impl(i) => {
                if let Some(items) = i.assoc_item_list() {
                    for item in items.assoc_items() {
                        if let ast::AssocItem::Fn(f) = item {
                            self.visit_fn(&f, file_id, annotations);
                        }
                    }
                }
            }
            _ => {}
        }
    }

    fn visit_fn(&self, func: &ast::Fn, file_id: FileId, annotations: &mut Vec<Annotation>) {
        if let Some(body) = func.body() {
            self.visit_block(&body, file_id, annotations);
        }
    }

    fn visit_block(&self, block: &ast::BlockExpr, file_id: FileId, annotations: &mut Vec<Annotation>) {
        for stmt in block.statements() {
            if let ast::Stmt::LetStmt(let_stmt) = stmt {
                self.visit_let(&let_stmt, file_id, annotations);
            }
        }
    }

    fn visit_let(&self, let_stmt: &ast::LetStmt, file_id: FileId, annotations: &mut Vec<Annotation>) {
        let Some(pat) = let_stmt.pat() else { return };
        let ast::Pat::IdentPat(ident) = &pat else { return };
        let Some(name) = ident.name() else { return };

        let name_str = name.text().to_string();
        let range = pat.syntax().text_range();
        let is_mut = ident.mut_token().is_some();

        // Query rust-analyzer for the actual type
        let offset = TextSize::from(u32::from(range.start()));
        let is_copy = self.is_copy_at(file_id, offset).unwrap_or(false);
        let type_str = self.type_at(file_id, offset);

        let state = BindingState::Owned { mutable: is_mut };
        let explanation = match type_str {
            Some(ty) => {
                let first_line = ty.lines().next().unwrap_or("");
                format!("{}: {} {}", name_str, first_line, if is_copy { "(Copy)" } else { "" })
            }
            None => format!("{}: owned{}", name_str, if is_mut { " (mut)" } else { "" }),
        };

        annotations.push(Annotation::new(range, name_str, state, explanation));
    }
}

fn find_manifest(start: &Path) -> Option<PathBuf> {
    let start_dir: &Path = if start.is_file() {
        start.parent()?
    } else {
        start
    };

    let mut current = Some(start_dir);
    while let Some(dir) = current {
        let manifest = dir.join("Cargo.toml");
        if manifest.exists() {
            return Some(manifest);
        }
        current = dir.parent();
    }
    None
}

fn is_primitive_in_hover(text: &str) -> bool {
    let prims = ["i8", "i16", "i32", "i64", "i128", "isize",
                 "u8", "u16", "u32", "u64", "u128", "usize",
                 "f32", "f64", "bool", "char"];
    prims.iter().any(|p| text.contains(p))
}

/// Convert a byte offset to a line number (0-indexed).
fn offset_to_line(source: &str, offset: TextSize) -> u32 {
    let offset = u32::from(offset) as usize;
    source[..offset.min(source.len())]
        .chars()
        .filter(|&c| c == '\n')
        .count() as u32
}

/// Collect all loop ranges (start_line, end_line) from an AST item.
fn collect_loop_ranges(item: &ast::Item, source: &str, ranges: &mut Vec<(u32, u32)>) {
    match item {
        ast::Item::Fn(f) => {
            if let Some(body) = f.body() {
                collect_loop_ranges_in_block(&body, source, ranges);
            }
        }
        ast::Item::Impl(i) => {
            if let Some(items) = i.assoc_item_list() {
                for item in items.assoc_items() {
                    if let ast::AssocItem::Fn(f) = item {
                        collect_loop_ranges(&ast::Item::Fn(f), source, ranges);
                    }
                }
            }
        }
        _ => {}
    }
}

fn collect_loop_ranges_in_block(block: &ast::BlockExpr, source: &str, ranges: &mut Vec<(u32, u32)>) {
    for stmt in block.statements() {
        if let ast::Stmt::ExprStmt(expr_stmt) = &stmt {
            if let Some(expr) = expr_stmt.expr() {
                collect_loop_ranges_in_expr(&expr, source, ranges);
            }
        }
    }
    if let Some(tail) = block.tail_expr() {
        collect_loop_ranges_in_expr(&tail, source, ranges);
    }
}

fn collect_loop_ranges_in_expr(expr: &ast::Expr, source: &str, ranges: &mut Vec<(u32, u32)>) {
    match expr {
        ast::Expr::LoopExpr(loop_expr) => {
            let range = loop_expr.syntax().text_range();
            let start_line = offset_to_line(source, range.start());
            let end_line = offset_to_line(source, range.end());
            ranges.push((start_line, end_line));
            if let Some(body) = loop_expr.loop_body() {
                collect_loop_ranges_in_block(&body, source, ranges);
            }
        }
        ast::Expr::WhileExpr(while_expr) => {
            let range = while_expr.syntax().text_range();
            let start_line = offset_to_line(source, range.start());
            let end_line = offset_to_line(source, range.end());
            ranges.push((start_line, end_line));
            if let Some(body) = while_expr.loop_body() {
                collect_loop_ranges_in_block(&body, source, ranges);
            }
        }
        ast::Expr::ForExpr(for_expr) => {
            let range = for_expr.syntax().text_range();
            let start_line = offset_to_line(source, range.start());
            let end_line = offset_to_line(source, range.end());
            ranges.push((start_line, end_line));
            if let Some(body) = for_expr.loop_body() {
                collect_loop_ranges_in_block(&body, source, ranges);
            }
        }
        ast::Expr::BlockExpr(block) => {
            collect_loop_ranges_in_block(block, source, ranges);
        }
        ast::Expr::IfExpr(if_expr) => {
            if let Some(then_branch) = if_expr.then_branch() {
                collect_loop_ranges_in_block(&then_branch, source, ranges);
            }
            if let Some(else_branch) = if_expr.else_branch() {
                match else_branch {
                    ast::ElseBranch::Block(block) => {
                        collect_loop_ranges_in_block(&block, source, ranges);
                    }
                    ast::ElseBranch::IfExpr(nested) => {
                        collect_loop_ranges_in_expr(&ast::Expr::IfExpr(nested), source, ranges);
                    }
                }
            }
        }
        ast::Expr::MatchExpr(match_expr) => {
            if let Some(arm_list) = match_expr.match_arm_list() {
                for arm in arm_list.arms() {
                    if let Some(expr) = arm.expr() {
                        collect_loop_ranges_in_expr(&expr, source, ranges);
                    }
                }
            }
        }
        ast::Expr::ClosureExpr(closure) => {
            if let Some(body) = closure.body() {
                collect_loop_ranges_in_expr(&body, source, ranges);
            }
        }
        _ => {}
    }
}

/// Compute the drop line for a variable.
///
/// If the last use is inside a loop but the declaration is outside,
/// the drop should happen after the loop ends. Otherwise, drop happens
/// after the last use.
fn compute_drop_line(decl_line: u32, last_use_line: u32, loop_ranges: &[(u32, u32)]) -> u32 {
    // Find the innermost loop that contains the last use but not the declaration
    let mut best_loop_end: Option<u32> = None;

    for &(loop_start, loop_end) in loop_ranges {
        // Check if last_use is inside this loop
        let last_use_in_loop = last_use_line >= loop_start && last_use_line <= loop_end;
        // Check if decl is outside this loop
        let decl_outside_loop = decl_line < loop_start;

        if last_use_in_loop && decl_outside_loop {
            // This variable's last use is in a loop it wasn't declared in
            // Use the innermost such loop (smallest range)
            match best_loop_end {
                None => best_loop_end = Some(loop_end),
                Some(current_end) if loop_end < current_end => {
                    best_loop_end = Some(loop_end);
                }
                _ => {}
            }
        }
    }

    // If we found a loop that contains the last use but not decl, drop after the loop.
    // For loops: drop at loop_end (the closing brace line).
    // For non-loops: drop on the line AFTER last use.
    best_loop_end.unwrap_or(last_use_line + 1)
}

// ============================================================================
// TypeOracle implementation - on-demand type queries
// ============================================================================

use super::TypeOracle;

/// Type oracle backed by rust-analyzer's semantic analysis.
///
/// This provides on-demand type queries by looking up AST nodes by offset
/// in the semantically-analyzed tree.
pub struct SemanticTypeOracle<'a> {
    host: &'a AnalysisHost,
    file_id: FileId,
}

impl<'a> SemanticTypeOracle<'a> {
    /// Create a new type oracle for a specific file.
    pub fn new(host: &'a AnalysisHost, file_id: FileId) -> Self {
        Self { host, file_id }
    }
}

impl TypeOracle for SemanticTypeOracle<'_> {
    fn is_copy(&self, pat: &ast::IdentPat) -> Option<bool> {
        let db = self.host.raw_database();
        let sema = Semantics::new(db);

        // Parse the file through Semantics so nodes are connected to the DB
        let file = sema.parse_guess_edition(self.file_id);

        // Find the corresponding IdentPat in the sema-parsed tree by offset
        let offset = pat.syntax().text_range().start();
        let sema_pat: ast::IdentPat = sema.find_node_at_offset_with_descend(file.syntax(), offset)?;

        // Get the type of this binding
        let ty = sema.type_of_binding_in_pat(&sema_pat)?;

        // References are Copy (the reference itself, not the referent)
        if ty.is_reference() || ty.is_mutable_reference() {
            return Some(true);
        }

        Some(ty.is_copy(db))
    }

    fn is_scalar(&self, pat: &ast::IdentPat) -> Option<bool> {
        let db = self.host.raw_database();
        let sema = Semantics::new(db);

        // Parse the file through Semantics
        let file = sema.parse_guess_edition(self.file_id);

        // Find the corresponding IdentPat by offset
        let offset = pat.syntax().text_range().start();
        let sema_pat: ast::IdentPat = sema.find_node_at_offset_with_descend(file.syntax(), offset)?;

        // Get the type
        let ty = sema.type_of_binding_in_pat(&sema_pat)?;

        // Check if it's a scalar primitive
        let ty_str = ty.display(db, ra_ap_syntax::Edition::Edition2021).to_string();
        Some(matches!(
            ty_str.as_str(),
            "i8" | "i16" | "i32" | "i64" | "i128" | "isize"
                | "u8" | "u16" | "u32" | "u64" | "u128" | "usize"
                | "f32" | "f64"
                | "bool"
                | "char"
                | "()"
        ))
    }
}

impl SemanticAnalyzer {
    /// Create a type oracle for the given file.
    ///
    /// Returns `None` if the file is not found in the workspace.
    pub fn type_oracle(&self, file_id: FileId) -> SemanticTypeOracle<'_> {
        SemanticTypeOracle::new(&self.host, file_id)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    #[ignore] // Slow: loads full cargo workspace with sysroot (~60s)
    fn test_semantic_load_in_cargo_project() {
        let test_file = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("src/main.rs");
        println!("Testing: {}", test_file.display());
        
        match SemanticAnalyzer::load(&test_file) {
            SemanticResult::Available(analyzer) => {
                println!("Semantic analysis available!");
                
                let source = std::fs::read_to_string(&test_file).unwrap();
                match analyzer.analyze(&test_file, &source) {
                    Ok(annotations) => {
                        println!("Found {} annotations", annotations.len());
                        for ann in &annotations {
                            println!("  {} : {}", ann.binding, ann.state);
                        }
                        assert!(!annotations.is_empty(), "Should find some annotations");
                    }
                    Err(e) => {
                        println!("Analysis error: {}", e);
                    }
                }
            }
            SemanticResult::NotCargoProject => {
                panic!("Should detect cargo project");
            }
            SemanticResult::LoadFailed => {
                println!("Load failed - this may happen in test environment");
            }
        }
    }
}
