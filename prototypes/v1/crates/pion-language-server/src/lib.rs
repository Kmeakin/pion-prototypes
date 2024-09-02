use crate::server::Server;

mod convert;
mod diagnostics;
mod handlers;
mod server;

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
