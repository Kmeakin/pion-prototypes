use lsp_server::{Connection, Message, Notification, Request, Response};
use lsp_types::{DiagnosticSeverity, ServerCapabilities, TextDocumentSyncKind};
use pion_utils::source::{line_index, SourceFile, SourceMap};

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
            document_symbol_provider: None,
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
                let path = url
                    .to_file_path()
                    .map_err(|_| anyhow::anyhow!("cannot convert url to path: {url}"))?;
                let path = path
                    .to_str()
                    .ok_or_else(|| anyhow::anyhow!("non-utf8 path: {}", path.display()))?;
                let file = SourceFile::read(path)?;
                self.source_map.insert_file(file);
                self.report_diagnostics()?;
            }
            DidChangeTextDocument::METHOD => {
                let params =
                    serde_json::from_value::<DidChangeTextDocumentParams>(notification.params)?;
                let url = params.text_document.uri;
                let path = url
                    .to_file_path()
                    .map_err(|_| anyhow::anyhow!("cannot convert url to path: {url}"))?;
                let path = path
                    .to_str()
                    .ok_or_else(|| anyhow::anyhow!("non-utf8 path: {}", path.display()))?;
                let file = SourceFile::read(path)?;
                self.source_map.insert_file(file);
                self.report_diagnostics()?;
            }
            _ => eprintln!("TODO: handle notification {notification:?}"),
        }

        Ok(())
    }

    fn handle_request(&mut self, request: Request) -> anyhow::Result<()> {
        eprintln!("TODO: handle request {request:?}");
        Ok(())
    }

    fn handle_response(&mut self, response: Response) -> anyhow::Result<()> {
        eprintln!("TODO: handle response {response:?}");
        Ok(())
    }

    fn report_diagnostics(&mut self) -> anyhow::Result<()> {
        use lsp_types::notification::{Notification as _, PublishDiagnostics};
        use lsp_types::{PublishDiagnosticsParams, Url};

        for (_file_id, file) in self.source_map.iter() {
            let mut diagnostics = Vec::new();
            let uri = Url::from_file_path(file.path.as_ref())
                .map_err(|_| anyhow::anyhow!("cannot convert path to url: {:?}", file.path))?;

            let tokens = pion_lexer::token::lex(&file.contents);
            for (result, span) in tokens {
                if let Err(error) = result {
                    let start = file.line_index.line_col(u32::from(span.start).into());
                    let start = file
                        .line_index
                        .to_wide(line_index::WideEncoding::Utf16, start)
                        .ok_or_else(|| {
                            anyhow::anyhow!("cannot convert utf8 LineCol to utf16: {start:?}")
                        })?;

                    let end = file.line_index.line_col(u32::from(span.end).into());
                    let end = file
                        .line_index
                        .to_wide(line_index::WideEncoding::Utf16, end)
                        .ok_or_else(|| {
                            anyhow::anyhow!("cannot convert utf8 LineCol to utf16: {end:?}")
                        })?;

                    let start = lsp_types::Position::new(start.line, start.col);
                    let end = lsp_types::Position::new(end.line, end.col);

                    let message = match error {
                        pion_lexer::token::TokenError::UnknownCharacter => "unknown character",
                        pion_lexer::token::TokenError::BlockComment { .. } => {
                            "unclosed block comment"
                        }
                    };
                    let diag = lsp_types::Diagnostic {
                        range: lsp_types::Range { start, end },
                        severity: Some(DiagnosticSeverity::ERROR),
                        code: None,
                        code_description: None,
                        source: Some("pion".into()),
                        message: message.into(),
                        related_information: None,
                        tags: None,
                        data: None,
                    };
                    diagnostics.push(diag);
                }
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
