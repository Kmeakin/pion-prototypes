#[derive(clap::Args)]
pub struct Args {}

pub fn run(_args: Args) -> anyhow::Result<()> {
    eprintln!("starting server");

    let (connection, io_threads) = lsp_server::Connection::stdio();
    let server_capabilities = serde_json::to_value(lsp_types::ServerCapabilities::default())?;
    let initilization_params = connection.initialize(server_capabilities)?;
    main_loop(connection, initilization_params)?;
    io_threads.join()?;

    eprintln!("shutting down server");
    Ok(())
}

fn main_loop(connection: lsp_server::Connection, params: serde_json::Value) -> anyhow::Result<()> {
    let params: lsp_types::InitializeParams = serde_json::from_value(params)?;
    eprintln!("params: {params:?}");

    for msg in connection.receiver {
        eprintln!("message recieved: {msg:?}");
    }
    Ok(())
}
