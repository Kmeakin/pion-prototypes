use lsp_server::{Message, Notification};

use crate::convert;
use crate::server::Server;

pub fn report_diagnostics(server: &Server) -> anyhow::Result<()> {
    use lsp_types::notification::{Notification as _, PublishDiagnostics};
    use lsp_types::PublishDiagnosticsParams;

    let bump = bumpalo::Bump::new();

    for (file_id, file) in server.files() {
        let uri = convert::path_to_url(file.path.as_ref())?;

        let (_, errors) = pion_surface::syntax::parse_module(&file.contents, &bump);
        let mut diagnostics = Vec::with_capacity(errors.len());
        for error in errors {
            let diagnostic = error.to_diagnostic(file_id);
            let diagnostic = convert::diagnostic_to_lsp(diagnostic, file)?;
            diagnostics.push(diagnostic);
        }

        server.send_message(Message::Notification(Notification {
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
