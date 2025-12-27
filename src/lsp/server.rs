//! LSP server main loop using lsp-server.

use super::capabilities::server_capabilities;
use super::handlers::{handle_hover, handle_inlay_hints, ServerState};
use anyhow::Result;
use lsp_server::{Connection, ExtractError, Message, Notification, Request, RequestId, Response};
use lsp_types::{
    notification::{DidChangeTextDocument, DidCloseTextDocument, DidOpenTextDocument},
    request::{HoverRequest, InlayHintRequest},
    InitializeParams,
};

/// Run the LSP server.
pub fn run_server() -> Result<()> {
    tracing::info!("Starting rs-commentary LSP server...");

    // Create the transport. Uses stdin/stdout for communication.
    let (connection, io_threads) = Connection::stdio();

    // Run the server and wait for the two threads to end.
    let server_capabilities = serde_json::to_value(server_capabilities())?;

    let initialization_params = match connection.initialize(server_capabilities) {
        Ok(params) => params,
        Err(e) => {
            if e.channel_is_disconnected() {
                io_threads.join()?;
            }
            return Err(e.into());
        }
    };

    let _params: InitializeParams = serde_json::from_value(initialization_params)?;
    tracing::info!("Server initialized");

    main_loop(connection)?;

    io_threads.join()?;
    tracing::info!("Server shutdown complete");

    Ok(())
}

fn main_loop(connection: Connection) -> Result<()> {
    let mut state = ServerState::new();

    for msg in &connection.receiver {
        match msg {
            Message::Request(req) => {
                if connection.handle_shutdown(&req)? {
                    return Ok(());
                }
                handle_request(&mut state, &connection, req)?;
            }
            Message::Response(_resp) => {
                // We don't send requests, so we don't expect responses
            }
            Message::Notification(not) => {
                handle_notification(&mut state, not)?;
            }
        }
    }

    Ok(())
}

fn handle_request(
    state: &mut ServerState,
    connection: &Connection,
    req: Request,
) -> Result<()> {
    let req = match cast_request::<InlayHintRequest>(req) {
        Ok((id, params)) => {
            let result = handle_inlay_hints(state, params)?;
            let resp = Response::new_ok(id, result);
            connection.sender.send(Message::Response(resp))?;
            return Ok(());
        }
        Err(ExtractError::MethodMismatch(req)) => req,
        Err(e) => return Err(e.into()),
    };

    let req = match cast_request::<HoverRequest>(req) {
        Ok((id, params)) => {
            let result = handle_hover(state, params)?;
            let resp = Response::new_ok(id, result);
            connection.sender.send(Message::Response(resp))?;
            return Ok(());
        }
        Err(ExtractError::MethodMismatch(req)) => req,
        Err(e) => return Err(e.into()),
    };

    tracing::warn!("Unhandled request: {:?}", req.method);
    Ok(())
}

fn handle_notification(state: &mut ServerState, not: Notification) -> Result<()> {
    let not = match cast_notification::<DidOpenTextDocument>(not) {
        Ok(params) => {
            tracing::info!("Document opened: {}", params.text_document.uri.as_str());
            state.open_document(params.text_document.uri, params.text_document.text);
            return Ok(());
        }
        Err(ExtractError::MethodMismatch(not)) => not,
        Err(e) => return Err(e.into()),
    };

    let not = match cast_notification::<DidChangeTextDocument>(not) {
        Ok(params) => {
            tracing::debug!("Document changed: {}", params.text_document.uri.as_str());
            // For full sync, we get the entire content
            if let Some(change) = params.content_changes.into_iter().next() {
                state.update_document(params.text_document.uri, change.text);
            }
            return Ok(());
        }
        Err(ExtractError::MethodMismatch(not)) => not,
        Err(e) => return Err(e.into()),
    };

    let not = match cast_notification::<DidCloseTextDocument>(not) {
        Ok(params) => {
            tracing::info!("Document closed: {}", params.text_document.uri.as_str());
            state.close_document(&params.text_document.uri);
            return Ok(());
        }
        Err(ExtractError::MethodMismatch(not)) => not,
        Err(e) => return Err(e.into()),
    };

    tracing::trace!("Unhandled notification: {:?}", not.method);
    Ok(())
}

fn cast_request<R>(req: Request) -> Result<(RequestId, R::Params), ExtractError<Request>>
where
    R: lsp_types::request::Request,
    R::Params: serde::de::DeserializeOwned,
{
    req.extract(R::METHOD)
}

fn cast_notification<N>(not: Notification) -> Result<N::Params, ExtractError<Notification>>
where
    N: lsp_types::notification::Notification,
    N::Params: serde::de::DeserializeOwned,
{
    not.extract(N::METHOD)
}
