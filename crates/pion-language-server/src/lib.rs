use anyhow::anyhow;
use camino::{Utf8Path, Utf8PathBuf};
use codespan_reporting::diagnostic::LabelStyle;
use codespan_reporting::files::Files;
use lsp_server::{Connection, Message, Notification, Request, Response};
use lsp_types::{DocumentSymbol, ServerCapabilities, SymbolKind, TextDocumentSyncKind};
use pion_utils::interner::Interner;
use pion_utils::location::ByteSpan;
use pion_utils::source::{FileId, SourceFile, SourceMap};

pub fn run() -> anyhow::Result<()> {
    eprintln!("starting server");

    let (connection, io_threads) = lsp_server::Connection::stdio();
    let server_capabilities = serde_json::to_value(Server::capabilities())?;
    let init_params = connection.initialize(server_capabilities)?;
    let init_params: lsp_types::InitializeParams = serde_json::from_value(init_params)?;
    eprintln!("init params: {init_params:?}");

    let mut server = Server::new(connection);

    server.main_loop()?;
    io_threads.join()?;

    eprintln!("shutting down server");
    Ok(())
}

pub struct Server {
    connection: Connection,
    source_map: SourceMap,
}

impl Server {
    fn capabilities() -> ServerCapabilities {
        use lsp_types::{TextDocumentSyncCapability, TextDocumentSyncOptions};

        ServerCapabilities {
            position_encoding: None,
            text_document_sync: Some(TextDocumentSyncCapability::Options(
                TextDocumentSyncOptions {
                    open_close: Some(true),
                    change: Some(TextDocumentSyncKind::FULL),
                    ..Default::default()
                },
            )),
            selection_range_provider: None,
            hover_provider: None,
            completion_provider: None,
            signature_help_provider: None,
            definition_provider: None,
            type_definition_provider: None,
            implementation_provider: None,
            references_provider: None,
            document_highlight_provider: None,
            document_symbol_provider: Some(lsp_types::OneOf::Left(true)),
            workspace_symbol_provider: None,
            code_action_provider: None,
            code_lens_provider: None,
            document_formatting_provider: None,
            document_range_formatting_provider: None,
            document_on_type_formatting_provider: None,
            rename_provider: None,
            document_link_provider: None,
            color_provider: None,
            folding_range_provider: None,
            declaration_provider: None,
            execute_command_provider: None,
            workspace: None,
            call_hierarchy_provider: None,
            semantic_tokens_provider: None,
            moniker_provider: None,
            inline_value_provider: None,
            inlay_hint_provider: None,
            linked_editing_range_provider: None,
            experimental: None,
        }
    }

    fn new(connection: Connection) -> Self {
        Self {
            connection,
            source_map: SourceMap::default(),
        }
    }

    fn main_loop(&mut self) -> anyhow::Result<()> {
        while let Ok(msg) = self.connection.receiver.recv() {
            eprintln!("message: {msg:?}");
            match msg {
                Message::Request(request) => match self.connection.handle_shutdown(&request) {
                    Err(error) => {
                        eprintln!("error: {error}");
                        break;
                    }
                    Ok(true) => break,
                    Ok(false) => self.handle_request(request)?,
                },
                Message::Response(response) => self.handle_response(response)?,
                Message::Notification(notification) => self.handle_notification(notification)?,
            }
        }

        Ok(())
    }

    fn handle_notification(&mut self, notification: Notification) -> anyhow::Result<()> {
        use lsp_types::notification::{DidChangeTextDocument, DidOpenTextDocument, Notification};
        use lsp_types::{DidChangeTextDocumentParams, DidOpenTextDocumentParams};

        match notification.method.as_str() {
            DidOpenTextDocument::METHOD => {
                let params =
                    serde_json::from_value::<DidOpenTextDocumentParams>(notification.params)?;
                let url = params.text_document.uri;
                let path = url_to_path(&url)?;
                let file = SourceFile::read(path)?;
                self.source_map.insert_file(file);
                self.report_diagnostics()?;
            }
            DidChangeTextDocument::METHOD => {
                let params =
                    serde_json::from_value::<DidChangeTextDocumentParams>(notification.params)?;
                let url = params.text_document.uri;
                let path = url_to_path(&url)?;
                let file = SourceFile::read(path)?;
                self.source_map.insert_file(file);
                self.report_diagnostics()?;
            }
            _ => eprintln!("TODO: handle notification {notification:?}"),
        }

        Ok(())
    }

    fn handle_request(&mut self, request: Request) -> anyhow::Result<()> {
        use lsp_types::request::{DocumentSymbolRequest, Request};

        match request.method.as_str() {
            DocumentSymbolRequest::METHOD => {
                use lsp_types::{DocumentSymbolParams, DocumentSymbolResponse};

                let params = serde_json::from_value::<DocumentSymbolParams>(request.params)?;
                let url = params.text_document.uri;
                let path = url_to_path(&url)?;
                let file = SourceFile::read(path)?;

                let interner = Interner::new();
                let bump = bumpalo::Bump::new();

                let mut symbols = Vec::new();
                let (module, _) =
                    pion_surface::syntax::parse_module(&file.contents, &bump, &interner);
                for item in module.items {
                    let symbol = match item {
                        pion_surface::syntax::Item::Error(_) => continue,
                        pion_surface::syntax::Item::Def(def) => {
                            let name = file.contents[def.name.0].to_owned().into();
                            let range = bytespan_to_lsp(def.span, &file)?;
                            let selection_range = bytespan_to_lsp(def.name.0, &file)?;

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

                self.connection.sender.send(Message::Response(Response {
                    id: request.id,
                    result: Some(serde_json::to_value(response)?),
                    error: None,
                }))?;
            }
            _ => eprintln!("TODO: handle request {request:?}"),
        }

        Ok(())
    }

    fn handle_response(&mut self, response: Response) -> anyhow::Result<()> {
        eprintln!("TODO: handle response {response:?}");
        Ok(())
    }

    fn report_diagnostics(&mut self) -> anyhow::Result<()> {
        use lsp_types::notification::{Notification as _, PublishDiagnostics};
        use lsp_types::PublishDiagnosticsParams;

        let interner = Interner::new();
        let bump = bumpalo::Bump::new();

        for (file_id, file) in self.source_map.iter() {
            let uri = path_to_url(file.path.as_ref())?;

            let (_, errors) = pion_surface::syntax::parse_module(&file.contents, &bump, &interner);
            let mut diagnostics = Vec::with_capacity(errors.len());
            for error in errors {
                let diagnostic = error.to_diagnostic(file_id);
                let diagnostic = diagnostic_to_lsp(diagnostic, file)?;
                diagnostics.push(diagnostic);
            }

            self.connection
                .sender
                .send(Message::Notification(Notification {
                    method: PublishDiagnostics::METHOD.into(),
                    params: serde_json::to_value(PublishDiagnosticsParams {
                        uri,
                        diagnostics,
                        version: None,
                    })?,
                }))?;
        }

        Ok(())
    }
}

fn bytespan_to_lsp(span: ByteSpan, file: &SourceFile) -> anyhow::Result<lsp_types::Range> {
    range_to_lsp(span.into(), file)
}

#[allow(clippy::cast_possible_truncation)]
fn range_to_lsp(
    range: std::ops::Range<usize>,
    file: &SourceFile,
) -> anyhow::Result<lsp_types::Range> {
    let start = file.location((), range.start)?;
    let end = file.location((), range.end)?;

    Ok(lsp_types::Range {
        start: lsp_types::Position {
            line: (start.line_number - 1) as u32,
            character: start.column_number as u32,
        },
        end: lsp_types::Position {
            line: (end.line_number - 1) as u32,
            character: end.column_number as u32,
        },
    })
}

fn diagnostic_to_lsp(
    diagnostic: codespan_reporting::diagnostic::Diagnostic<FileId>,
    file: &SourceFile,
) -> anyhow::Result<lsp_types::Diagnostic> {
    Ok(lsp_types::Diagnostic {
        range: {
            let primary_label = diagnostic
                .labels
                .iter()
                .find(|label| label.style == LabelStyle::Primary)
                .unwrap();
            let range = primary_label.range.clone();
            range_to_lsp(range, file)?
        },
        severity: Some(match diagnostic.severity {
            codespan_reporting::diagnostic::Severity::Bug
            | codespan_reporting::diagnostic::Severity::Error => {
                lsp_types::DiagnosticSeverity::ERROR
            }
            codespan_reporting::diagnostic::Severity::Warning => {
                lsp_types::DiagnosticSeverity::WARNING
            }
            codespan_reporting::diagnostic::Severity::Note => {
                lsp_types::DiagnosticSeverity::INFORMATION
            }
            codespan_reporting::diagnostic::Severity::Help => lsp_types::DiagnosticSeverity::HINT,
        }),
        code: None,
        code_description: None,
        source: Some("pion".into()),
        message: diagnostic.message,
        related_information: None,
        tags: None,
        data: None,
    })
}

fn path_to_url(path: &Utf8Path) -> anyhow::Result<lsp_types::Url> {
    lsp_types::Url::from_file_path(path).map_err(|_| anyhow!("cannot convert path {path:?} to URL"))
}

fn url_to_path(url: &lsp_types::Url) -> anyhow::Result<Utf8PathBuf> {
    let path = url
        .to_file_path()
        .map_err(|_| anyhow::anyhow!("cannot convert url to path: {url}"))?;
    Ok(Utf8PathBuf::try_from(path)?)
}
