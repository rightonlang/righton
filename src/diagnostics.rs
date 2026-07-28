use crate::ast::SourceSpan;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Diagnostic {
    pub code: Option<&'static str>,
    pub severity: Severity,
    pub message: String,
    pub span: Option<SourceSpan>,
    pub hint: Option<String>,
    pub related: Vec<Diagnostic>,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "lowercase")]
pub enum Severity {
    Error,
    Warning,
    Info,
    Note,
}

impl Diagnostic {
    pub fn error(code: Option<&'static str>, message: impl Into<String>) -> Self {
        Self {
            code,
            severity: Severity::Error,
            message: message.into(),
            span: None,
            hint: None,
            related: Vec::new(),
        }
    }

    pub fn with_span(mut self, span: SourceSpan) -> Self {
        self.span = Some(span);
        self
    }

    pub fn with_hint(mut self, hint: impl Into<String>) -> Self {
        self.hint = Some(hint.into());
        self
    }

    pub fn with_related(mut self, related: Diagnostic) -> Self {
        self.related.push(related);
        self
    }

    pub fn to_user_message(&self) -> String {
        let mut msg = String::new();
        if let Some(span) = self.span {
            if span.line > 0 {
                msg.push_str(&format!("error[{}] at line {}, col {}: {}", 
                    self.code.unwrap_or("unknown"), 
                    span.line, 
                    span.column, 
                    self.message));
            } else {
                msg.push_str(&format!("error[{}]: {}", self.code.unwrap_or("unknown"), self.message));
            }
        } else {
            msg.push_str(&format!("error[{}]: {}", self.code.unwrap_or("unknown"), self.message));
        }
        if let Some(ref hint) = self.hint {
            msg.push_str(&format!("\n  hint: {}", hint));
        }
        for related in &self.related {
            msg.push('\n');
            msg.push_str(&related.to_user_message());
        }
        msg
    }

    pub fn to_json(&self) -> serde_json::Value {
        serde_json::json!({
            "code": self.code,
            "severity": self.severity,
            "message": self.message,
            "span": self.span,
            "hint": self.hint,
            "related": self.related.iter().map(|r| r.to_json()).collect::<Vec<_>>(),
        })
    }
}
