# VS Code Extension for Righton

This extension provides syntax highlighting and LSP (language server protocol) support for the Righton programming language in Visual Studio Code.

## Features

- **Syntax highlighting** — keywords, strings, comments, numbers, operators, and built-in functions are colorized
- **Language server** — diagnostics (parse errors), completions, hover info, powered by `righton-lsp`
- **Compile command** — compile the current `.ro` file from the command palette (`Righton: Compile Current File`)

## Requirements

- Visual Studio Code 1.85+
- Node.js and npm (to build the extension)
- Rust toolchain (to build the LSP server)
- The `righton-lsp` binary on your `PATH` (comes with the Righton compiler)

## Install from source

1. **Build the LSP server**

   From the Righton project root:

   ```bash
   cargo build --release
   ```

   This produces `target/release/righton-lsp.exe` (Windows) or `target/release/righton-lsp` (Linux/macOS).

2. **Install Node.js dependencies and compile the extension**

   ```bash
   cd .vscode/righton-vscode
   npm install
   npx tsc -p tsconfig.json
   ```

3. **Copy the LSP binary to your PATH**

   Ensure `righton-lsp` is reachable. For example:

   ```bash
   cp ../../target/release/righton-lsp /usr/local/bin/   # Linux/macOS
   # or on Windows, add the target/release directory to your PATH
   ```

4. **Install the extension in VS Code**

   ```bash
   # From the extension directory (.vscode/righton-vscode)
   code --install-extension .
   ```

   Alternatively, package it as a `.vsix`:

   ```bash
   npx vsce package
   code --install-extension righton-language-0.1.0.vsix
   ```

5. **Reload VS Code** — open any `.ro` file and syntax highlighting + LSP will activate automatically.

## Usage

### Syntax Highlighting

Open any `.ro` file — keywords, strings, comments, and numbers are highlighted automatically.

### Language Server

The LSP starts automatically when you open a Righton file. It provides:
- **Diagnostics** — parse errors shown in the Problems panel
- **Completions** — keywords, built-in types, and stdlib functions
- **Hover** — shows type info for keywords, types, and stdlib functions

### Compile Command

Press `Ctrl+Shift+P` and run `Righton: Compile Current File`. This invokes `righton -i <file>.ro -o <file>.o && gcc <file>.o -o <file>` and shows the output in a terminal.

## Extension Settings

| Setting | Default | Description |
|---|---|---|
| `righton.lsp.path` | `righton-lsp` | Path to the righton-lsp binary |
| `righton.compiler.path` | `righton` | Path to the righton compiler binary |
