use righton::lexer::Lexer;
use righton::parser::Parser;
use serde_json::{json, Value};
use std::collections::HashMap;
use std::io::{self, BufRead, Read, Write};

const KEYWORDS: &[&str] = &[
    "fn", "let", "const", "return", "if", "else", "elif", "while",
    "for", "in", "break", "continue", "struct", "enum", "impl", "type",
    "match", "import", "extern", "mut", "and", "or", "not", "true", "false",
];

const BUILTIN_TYPES: &[&str] = &["i32", "f64", "bool", "str", "string", "ptr", "float", "double"];

const STDLIB_FUNCTIONS: &[&str] = &[
    "print", "print_int", "print_float", "len", "read_file", "write_file",
    "exit", "is_empty", "contains", "starts_with", "ends_with", "substr",
    "trim", "to_uppercase", "to_lowercase", "to_string", "str_repeat",
    "to_int", "to_float", "to_hex", "abs", "abs_f", "floor", "ceil", "round",
    "sqrt", "sin", "cos", "tan", "min", "max", "pow", "read_line",
    "read_int", "read_float", "asm", "list_len", "list_push", "list_pop", "list_free",
];

fn read_message() -> Option<String> {
    let stdin = io::stdin();
    let mut handle = stdin.lock();
    let mut content_length: Option<usize> = None;
    loop {
        let mut line = String::new();
        if handle.read_line(&mut line).ok()? == 0 {
            return None;
        }
        let line = line.trim();
        if line.is_empty() {
            break;
        }
        if let Some(len) = line.strip_prefix("Content-Length: ") {
            content_length = len.parse().ok();
        }
    }
    let len = content_length?;
    let mut buf = vec![0u8; len];
    let mut read = 0;
    while read < len {
        let n = handle.read(&mut buf[read..]).ok()?;
        if n == 0 { return None; }
        read += n;
    }
    String::from_utf8(buf).ok()
}

fn send_message(msg: &Value) {
    let body = serde_json::to_string(msg).unwrap();
    let mut stdout = io::stdout().lock();
    write!(stdout, "Content-Length: {}\r\n\r\n{}", body.len(), body).unwrap();
    stdout.flush().unwrap();
}

fn make_response(id: &Value, result: Value) -> Value {
    json!({"jsonrpc": "2.0", "id": id, "result": result})
}

fn make_error(id: &Value, code: i64, message: &str) -> Value {
    json!({"jsonrpc": "2.0", "id": id, "error": {"code": code, "message": message}})
}

fn make_notification(method: &str, params: Value) -> Value {
    json!({"jsonrpc": "2.0", "method": method, "params": params})
}

fn get_string(params: &Value, key: &str) -> Option<String> {
    params.get(key)?.as_str().map(|s| s.to_string())
}

fn run_diagnostics(_uri: &str, text: &str) -> Vec<Value> {
    let mut diagnostics = Vec::new();
    let mut lexer = Lexer::new(text);
    let mut parser = Parser::new(&mut lexer);
    match parser.parse_program("debug".to_string(), "lsp".to_string()) {
        Ok(_) => {}
        Err(e) => {
            let msg = e.to_string();
            let (line, col) = extract_line_col(&msg);
            let diag = json!({
                "range": {
                    "start": {"line": line.saturating_sub(1), "character": col},
                    "end": {"line": line.saturating_sub(1), "character": col + 1}
                },
                "severity": 1,
                "message": msg,
                "source": "righton"
            });
            diagnostics.push(diag);
        }
    }
    diagnostics
}

fn extract_line_col(msg: &str) -> (usize, usize) {
    if let Some(cap) = msg.split("(line=").nth(1) {
        if let Some(line_str) = cap.split(", ").next() {
            if let Ok(line) = line_str.parse::<usize>() {
                if let Some(col_str) = cap.split("col=").nth(1) {
                    if let Some(col) = col_str.split(')').next() {
                        if let Ok(c) = col.parse::<usize>() {
                            return (line, c);
                        }
                    }
                }
            }
        }
    }
    (0, 0)
}

fn keyword_completions() -> Vec<Value> {
    KEYWORDS.iter().map(|k| {
        json!({
            "label": k,
            "kind": 14,
            "detail": "keyword",
            "insertText": k
        })
    }).collect()
}

fn type_completions() -> Vec<Value> {
    BUILTIN_TYPES.iter().map(|t| {
        json!({
            "label": t,
            "kind": 22,
            "detail": "type"
        })
    }).collect()
}

fn stdlib_completions() -> Vec<Value> {
    STDLIB_FUNCTIONS.iter().map(|f| {
        json!({
            "label": f,
            "kind": 3,
            "detail": "function (std)",
            "insertText": f
        })
    }).collect()
}

fn context_keywords(line: &str) -> Vec<Value> {
    let trimmed = line.trim();
    let mut items = Vec::new();
    if trimmed.is_empty() || trimmed.ends_with(':') || trimmed.ends_with('{') {
        for k in &["fn", "let", "const", "if", "while", "for", "match", "return", "import", "struct", "enum", "impl", "type"] {
            items.push(json!({
                "label": k,
                "kind": 14,
                "detail": "keyword",
                "insertText": k
            }));
        }
    }
    if trimmed.starts_with("import ") || trimmed == "import" {
        items.push(json!({
            "label": "std",
            "kind": 9,
            "detail": "standard library module"
        }));
    }
    items
}

fn make_hover(text: &str, line: usize, character: usize) -> Option<Value> {
    let source_line = text.lines().nth(line)?;
    let word = get_word_at(source_line, character)?;
    if KEYWORDS.contains(&word.as_str()) {
        return Some(json!({
            "contents": {"kind": "markdown", "value": format!("**`{}`** (keyword)", word)}
        }));
    }
    if BUILTIN_TYPES.contains(&word.as_str()) {
        return Some(json!({
            "contents": {"kind": "markdown", "value": format!("**`{}`** (built-in type)", word)}
        }));
    }
    if STDLIB_FUNCTIONS.contains(&word.as_str()) {
        return Some(json!({
            "contents": {"kind": "markdown", "value": format!("**`{}`** (stdlib function)", word)}
        }));
    }
    Some(json!({
        "contents": {"kind": "markdown", "value": format!("**`{}`**", word)}
    }))
}

fn get_word_at(line: &str, character: usize) -> Option<String> {
    let bytes = line.as_bytes();
    if character >= bytes.len() { return None; }
    let mut start = character;
    let mut end = character;
    while start > 0 && (bytes[start - 1].is_ascii_alphanumeric() || bytes[start - 1] == b'_') {
        start -= 1;
    }
    while end < bytes.len() && (bytes[end].is_ascii_alphanumeric() || bytes[end] == b'_') {
        end += 1;
    }
    if start == end { return None; }
    Some(line[start..end].to_string())
}

struct DocumentStore {
    docs: HashMap<String, String>,
}

impl DocumentStore {
    fn new() -> Self {
        Self { docs: HashMap::new() }
    }
    fn open(&mut self, uri: String, text: String) {
        self.docs.insert(uri, text);
    }
    fn change(&mut self, uri: &str, text: &str) {
        self.docs.insert(uri.to_string(), text.to_string());
    }
    fn close(&mut self, uri: &str) {
        self.docs.remove(uri);
    }
    fn get(&self, uri: &str) -> Option<&str> {
        self.docs.get(uri).map(|s| s.as_str())
    }
}

fn run_server() {
    let mut store = DocumentStore::new();

    while let Some(body) = read_message() {
        let msg: Value = match serde_json::from_str(&body) {
            Ok(v) => v,
            Err(_) => continue,
        };

        let method = msg.get("method").and_then(|m| m.as_str()).unwrap_or("");
        let id = &msg["id"];
        let params = msg.get("params").cloned().unwrap_or(Value::Null);
        let is_notification = msg.get("id").is_none() || msg["id"] == Value::Null;

        match method {
            "initialize" => {
                let capabilities = json!({
                    "textDocumentSync": {
                        "openClose": true,
                        "change": 1
                    },
                    "completionProvider": {
                        "triggerCharacters": [".", ":"],
                        "resolveProvider": false
                    },
                    "hoverProvider": true
                });
                let result = json!({
                    "capabilities": capabilities,
                    "serverInfo": {
                        "name": "righton-lsp",
                        "version": "0.1.0"
                    }
                });
                send_message(&make_response(id, result));
            }
            "initialized" => {}
            "shutdown" => {
                send_message(&make_response(id, Value::Null));
                return;
            }
            "exit" => return,
            "textDocument/didOpen" => {
                if let (Some(uri), Some(text)) = (
                    get_string(&params, "uri").or_else(|| {
                        params.get("textDocument").and_then(|td| get_string(td, "uri"))
                    }),
                    get_string(&params, "text").or_else(|| {
                        params.get("textDocument").and_then(|td| get_string(td, "text"))
                    }),
                ) {
                    store.open(uri.clone(), text.clone());
                    let diagnostics = run_diagnostics(&uri, &text);
                    let diag_params = json!({"uri": uri, "diagnostics": diagnostics});
                    send_message(&make_notification("textDocument/publishDiagnostics", diag_params));
                }
            }
            "textDocument/didChange" => {
                if let Some(uri) = get_string(&params, "uri").or_else(|| {
                    params.get("textDocument").and_then(|td| get_string(td, "uri"))
                }) {
                    let text = params.get("contentChanges")
                        .and_then(|c| c.get(0))
                        .and_then(|c| c.get("text"))
                        .and_then(|t| t.as_str())
                        .unwrap_or("");
                    store.change(&uri, text);
                    let text = store.get(&uri).unwrap_or("");
                    let diagnostics = run_diagnostics(&uri, text);
                    let diag_params = json!({"uri": uri, "diagnostics": diagnostics});
                    send_message(&make_notification("textDocument/publishDiagnostics", diag_params));
                }
            }
            "textDocument/didClose" => {
                if let Some(uri) = get_string(&params, "uri").or_else(|| {
                    params.get("textDocument").and_then(|td| get_string(td, "uri"))
                }) {
                    store.close(&uri);
                }
            }
            "textDocument/completion" => {
                let uri_str = get_string(&params, "uri").or_else(|| {
                    params.get("textDocument").and_then(|td| get_string(td, "uri"))
                }).unwrap_or_default();
                let text = store.get(&uri_str).unwrap_or("");
                let line = params.pointer("/position/line").and_then(|v| v.as_u64()).unwrap_or(0) as usize;
                let source_line = text.lines().nth(line).unwrap_or("");
                let mut items = Vec::new();
                items.extend(keyword_completions());
                items.extend(type_completions());
                items.extend(stdlib_completions());
                items.extend(context_keywords(source_line));
                let result = json!({"isIncomplete": false, "items": items});
                if !is_notification {
                    send_message(&make_response(id, result));
                }
            }
            "textDocument/hover" => {
                let uri_str = get_string(&params, "uri").or_else(|| {
                    params.get("textDocument").and_then(|td| get_string(td, "uri"))
                }).unwrap_or_default();
                let text = store.get(&uri_str).unwrap_or("");
                let line = params.pointer("/position/line").and_then(|v| v.as_u64()).unwrap_or(0) as usize;
                let character = params.pointer("/position/character").and_then(|v| v.as_u64()).unwrap_or(0) as usize;
                if let Some(hover) = make_hover(text, line, character) {
                    if !is_notification {
                        send_message(&make_response(id, hover));
                    }
                } else {
                    if !is_notification {
                        send_message(&make_response(id, Value::Null));
                    }
                }
            }
            "textDocument/definition" => {
                send_message(&make_response(id, Value::Null));
            }
            _ => {
                if !is_notification {
                    send_message(&make_error(id, -32601, &format!("method not found: {}", method)));
                }
            }
        }
    }
}

fn main() {
    run_server();
}
