use lsp_server::{Message, Notification, Request, Response};
use lsp_types::notification::{DidChangeTextDocument, DidOpenTextDocument, Notification as _};
use lsp_types::{
    DidChangeTextDocumentParams, DidOpenTextDocumentParams, DocumentSymbol, SymbolKind,
};
use pion_utils::source::SourceFile;

use crate::{convert, diagnostics, Server};

pub fn handle_request(server: &Server, request: Request) -> anyhow::Result<()> {
    use lsp_types::request::{DocumentSymbolRequest, Request};

    match request.method.as_str() {
        DocumentSymbolRequest::METHOD => {
            use lsp_types::{DocumentSymbolParams, DocumentSymbolResponse};

            let params = serde_json::from_value::<DocumentSymbolParams>(request.params)?;
            let url = params.text_document.uri;
            let path = convert::url_to_path(&url)?;
            let file = SourceFile::read(path)?;

            let bump = bumpalo::Bump::new();

            let mut symbols = Vec::new();
            let (module, _) = pion_surface::syntax::parse_module(&file.contents, &bump);
            for item in module.items {
                let symbol = match item {
                    pion_surface::syntax::Item::Error(_) => continue,
                    pion_surface::syntax::Item::Def(def) => {
                        let name = file.contents[def.name.0].to_owned().into();
                        let range = convert::bytespan_to_lsp(def.span, &file)?;
                        let selection_range = convert::bytespan_to_lsp(def.name.0, &file)?;

                        #[allow(deprecated)]
                        DocumentSymbol {
                            name,
                            detail: None,
                            kind: SymbolKind::CONSTANT,
                            tags: None,
                            deprecated: None,
                            range,
                            selection_range,
                            children: None,
                        }
                    }
                };
                symbols.push(symbol);
            }

            let response = DocumentSymbolResponse::Nested(symbols);

            server.send_message(Message::Response(Response {
                id: request.id,
                result: Some(serde_json::to_value(response)?),
                error: None,
            }))?;
        }
        _ => eprintln!("TODO: handle request {request:?}"),
    }

    Ok(())
}

#[allow(clippy::unnecessary_wraps)]
#[allow(unused_variables)]
#[allow(clippy::needless_pass_by_value)]
pub fn handle_response(server: &Server, response: Response) -> anyhow::Result<()> {
    eprintln!("TODO: handle response {response:?}");
    Ok(())
}

pub fn handle_notification(server: &mut Server, notification: Notification) -> anyhow::Result<()> {
    match notification.method.as_str() {
        DidOpenTextDocument::METHOD => handle_open_text_document(server, notification),
        DidChangeTextDocument::METHOD => handle_did_change_text_document(server, notification),
        _ => {
            eprintln!("TODO: handle notification {notification:?}");
            Ok(())
        }
    }
}

fn handle_open_text_document(
    server: &mut Server,
    notification: Notification,
) -> anyhow::Result<()> {
    let params = serde_json::from_value::<DidOpenTextDocumentParams>(notification.params)?;
    let url = params.text_document.uri;
    let path = convert::url_to_path(&url)?;
    let file = SourceFile::read(path)?;
    server.insert_file(file);
    diagnostics::report_diagnostics(server)?;
    Ok(())
}

fn handle_did_change_text_document(
    server: &mut Server,
    notification: Notification,
) -> anyhow::Result<()> {
    let params = serde_json::from_value::<DidChangeTextDocumentParams>(notification.params)?;
    let url = params.text_document.uri;
    let path = convert::url_to_path(&url)?;
    let file = SourceFile::read(path)?;
    server.insert_file(file);
    diagnostics::report_diagnostics(server)?;
    Ok(())
}
