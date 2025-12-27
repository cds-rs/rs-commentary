//! LSP server implementation using lsp-server.

mod server;
mod handlers;
mod capabilities;

pub use server::run_server;
