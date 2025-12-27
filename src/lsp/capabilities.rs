//! LSP server capabilities registration.

use lsp_types::{
    HoverProviderCapability, InlayHintOptions, InlayHintServerCapabilities, ServerCapabilities,
    TextDocumentSyncCapability, TextDocumentSyncKind,
};

/// Build the server capabilities to advertise to clients.
pub fn server_capabilities() -> ServerCapabilities {
    ServerCapabilities {
        // Sync full document on open/change
        text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),

        // Provide hover information
        hover_provider: Some(HoverProviderCapability::Simple(true)),

        // Provide inlay hints (ownership annotations)
        inlay_hint_provider: Some(lsp_types::OneOf::Right(InlayHintServerCapabilities::Options(
            InlayHintOptions {
                resolve_provider: Some(false),
                work_done_progress_options: Default::default(),
            },
        ))),

        ..Default::default()
    }
}
