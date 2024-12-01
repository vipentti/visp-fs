// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
import * as vscode from "vscode";
import {
  Executable,
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
  TransportKind,
} from "vscode-languageclient/node";
import fs from "node:fs";

let client: LanguageClient | null;

function getConfiguredClient(outputChannel: vscode.OutputChannel) {
  const languageServerPath = vscode.workspace
    .getConfiguration("visp-fs.server")
    .get("path", "");

  const command = process.env.VISP_FS_SERVER_PATH ?? languageServerPath;

  if ((command?.length ?? 0) > 0) {
    if (fs.existsSync(command)) {
      // Options to control the language client
      const clientOptions: LanguageClientOptions = {
        // Register the server for visp-fs
        documentSelector: [{ scheme: "file", language: "visp-fs" }],
        synchronize: {
          // Notify the server about file changes to '.clientrc files contained in the workspace
          fileEvents: vscode.workspace.createFileSystemWatcher("**/.clientrc"),
        },
        outputChannel,
      };

      const run: Executable = {
        command,
        options: { env: Object.assign({}, process.env) },
        transport: TransportKind.stdio,
      };

      outputChannel.appendLine(`Using server from: ${run.command}`);

      // If the extension is launched in debug mode then the debug server options are used
      // Otherwise the run options are used
      const serverOptions: ServerOptions = {
        run,
        debug: run,
      };

      // Create the language client
      return new LanguageClient(
        "visp-fs",
        "visp-fs Language Server",
        serverOptions,
        clientOptions
      );
    } else {
      outputChannel.appendLine(
        "LanguageServer path was not set ensure `visp-fs.server.path` points to a valid Visp.LanguageServer executable"
      );
    }
  } else {
    outputChannel.appendLine(
      "LanguageServer path was not set ensure `visp-fs.server.path` points to a valid Visp.LanguageServer executable"
    );
  }

  return null;
}

// This method is called when your extension is activated
// Your extension is activated the very first time the command is executed
export async function activate(context: vscode.ExtensionContext) {
  const outputChannel = vscode.window.createOutputChannel("visp-fs", "visp-fs");

  client = getConfiguredClient(outputChannel);

  context.subscriptions.push(
    vscode.commands.registerCommand("visp-fs.resetClient", async () => {
      if (client !== null) {
        await client.stop();
        client = null;
      }

      vscode.window.showInformationMessage(
        "Resetting visp-fs language client..."
      );
      outputChannel.appendLine("Resetting visp-fs language client...");

      client = getConfiguredClient(outputChannel);

      if (client) {
        vscode.window.showInformationMessage(
          "Starting visp-fs language server ..."
        );
        outputChannel.appendLine("Starting visp-fs language server ...");
        await client.start();
      }
    })
  );

  context.subscriptions.push(
    vscode.commands.registerCommand("visp-fs.startServer", async () => {
      if (client) {
        vscode.window.showInformationMessage(
          "Starting visp-fs language server & client..."
        );
        outputChannel.appendLine(
          "Starting visp-fs language server & client..."
        );
        await client.start();
      }
    })
  );

  context.subscriptions.push(
    vscode.commands.registerCommand("visp-fs.stopServer", async () => {
      if (client) {
        // Display a message box to the user
        vscode.window.showInformationMessage(
          "Stopping visp-fs language server & client..."
        );
        outputChannel.appendLine(
          "Stopping visp-fs language server & client..."
        );
        await client.stop();
      }
    })
  );

  context.subscriptions.push(
    vscode.commands.registerCommand("visp-fs.restartServer", async () => {
      if (client) {
        // Display a message box to the user
        vscode.window.showInformationMessage(
          "Restarting visp-fs language server & client..."
        );

        await client.stop();

        await new Promise((resolve) => setTimeout(resolve, 1000));

        outputChannel.appendLine(
          "Restarting visp-fs language server & client..."
        );

        await client.start();
      }
    })
  );

  // Start the client. This will also launch the server
  if (client) {
    outputChannel.appendLine("Starting visp language server & client...");
    await client.start();
  }
}

// This method is called when your extension is deactivated
export async function deactivate() {
  if (client) {
    await client.stop();
  }
}
