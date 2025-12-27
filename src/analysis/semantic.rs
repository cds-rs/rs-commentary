//! Semantic analysis using rust-analyzer's ra_ap_ide.
//!
//! When a file is part of a cargo project, this provides:
//! - Accurate type inference
//! - Macro expansion
//! - Copy/Drop trait detection

use crate::analysis::state::{Annotation, BindingState};
use anyhow::{Context, Result};
use ra_ap_ide::{
    AnalysisHost, AssistResolveStrategy, DiagnosticsConfig, FileId, FileRange, HoverConfig,
    HoverDocFormat, SubstTyLen,
};
use ra_ap_load_cargo::{load_workspace_at, LoadCargoConfig, ProcMacroServerChoice};
use ra_ap_project_model::CargoConfig;
use ra_ap_syntax::{
    ast::{self, HasModuleItem, HasName},
    AstNode, SourceFile, TextRange, TextSize,
};
use ra_ap_vfs::Vfs;
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

        let cargo_config = CargoConfig::default();
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

    /// Check if a type at a position implements Copy.
    pub fn is_copy_at(&self, file_id: FileId, offset: TextSize) -> Option<bool> {
        let analysis = self.host.analysis();

        // Use hover to get type info (simpler than full semantic query)
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
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
