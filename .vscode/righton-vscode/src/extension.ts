import * as path from 'path';
import * as vscode from 'vscode';
import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
  TransportKind,
} from 'vscode-languageclient/node';

let client: LanguageClient | undefined;

export function activate(context: vscode.ExtensionContext) {
  const config = vscode.workspace.getConfiguration('righton');
  const commandPath = config.get<string>('lsp.path', 'righton-lsp');

  const serverOptions: ServerOptions = {
    command: commandPath,
    transport: TransportKind.stdio,
  };

  const clientOptions: LanguageClientOptions = {
    documentSelector: [{ scheme: 'file', language: 'righton' }],
    synchronize: {
      configurationSection: 'righton',
    },
  };

  client = new LanguageClient(
    'righton-lsp',
    'Righton Language Server',
    serverOptions,
    clientOptions,
  );

  client.start();

  const compileCmd = vscode.commands.registerCommand('righton.compile', async () => {
    const editor = vscode.window.activeTextEditor;
    if (!editor) {
      vscode.window.showErrorMessage('No active editor');
      return;
    }

    const document = editor.document;
    if (document.languageId !== 'righton') {
      vscode.window.showErrorMessage('Not a Righton file');
      return;
    }

    const compilerPath = vscode.workspace.getConfiguration('righton').get<string>('compiler.path', 'righton');
    const filePath = document.uri.fsPath;

    const terminal = vscode.window.createTerminal('Righton Compile');
    terminal.show();
    terminal.sendText(`${compilerPath} -i "${filePath}" -o "${filePath}.o" && gcc "${filePath}.o" -o "${path.basename(filePath, '.ro')}"`);
  });

  context.subscriptions.push(compileCmd);
}

export function deactivate() {
  if (client) {
    return client.stop();
  }
}
