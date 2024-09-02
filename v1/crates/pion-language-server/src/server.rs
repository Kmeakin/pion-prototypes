use lsp_server::{Connection, Message};
use lsp_types::{ServerCapabilities, TextDocumentSyncKind};
use pion_utils::source::{FileId, SourceFile, SourceMap};

use crate::handlers;

pub struct Server {
    connection: Connection,
    source_map: SourceMap,
}

impl Server {
    pub fn capabilities() -> ServerCapabilities {
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
            diagnostic_provider: None,
        }
    }

    pub fn new(connection: Connection) -> Self {
        Self {
            connection,
            source_map: SourceMap::default(),
        }
    }

    pub fn send_message(&self, message: Message) -> anyhow::Result<()> {
        self.connection.sender.send(message)?;
        Ok(())
    }

    pub fn files(&self) -> impl Iterator<Item = (FileId, &SourceFile)> { self.source_map.iter() }

    pub fn main_loop(&mut self) -> anyhow::Result<()> {
        while let Ok(msg) = self.connection.receiver.recv() {
            eprintln!("message: {msg:?}");
            match msg {
                Message::Request(request) => match self.connection.handle_shutdown(&request) {
                    Err(error) => {
                        eprintln!("error: {error}");
                        break;
                    }
                    Ok(true) => break,
                    Ok(false) => handlers::handle_request(self, request)?,
                },
                Message::Response(response) => handlers::handle_response(self, response)?,
                Message::Notification(notification) => {
                    handlers::handle_notification(self, notification)?;
                }
            }
        }

        Ok(())
    }

    pub fn insert_file(&mut self, file: SourceFile) -> Option<FileId> {
        self.source_map.insert_file(file)
    }
}
