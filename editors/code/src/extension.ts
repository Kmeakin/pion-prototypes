import { workspace, ExtensionContext } from 'vscode';

import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
} from 'vscode-languageclient/node';

let client: LanguageClient;

export function activate(context: ExtensionContext) {
  let debugServerPath = "/home/karl/git/me/pion/target/debug/pion";

  // If the extension is launched in debug mode then the debug server options are used
  // Otherwise the run options are used
  let serverOptions: ServerOptions = {
  debug: {
      command: debugServerPath,
      args: ["language-server"]
    },
    run: {
      command: debugServerPath,
      args: ["language-server"]
    },
  };

  // Options to control the language client
  let clientOptions: LanguageClientOptions = {
    // Register the server for plain text documents
    documentSelector: [{ scheme: 'file', language: 'pion' }],
    synchronize: {
      // Notify the server about file changes to `*.pion` files contained in the workspace
      fileEvents: workspace.createFileSystemWatcher('**/*.pion')
    }
  };

  // Create the language client and start the client.
  client = new LanguageClient(
    'PionLanguageClient',
    'Pion Language Client',
    serverOptions,
    clientOptions
  );

  // Start the client. This will also launch the server
  client.start();
}

export function deactivate(): Thenable<void> | undefined {
  if (!client) {
    return undefined;
  }
  return client.stop();
}
