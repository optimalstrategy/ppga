//! This module defines the error handling and reporting facilities used by the transpiler.
use logos::Span;

/// A span with a line.
#[derive(Debug, Clone)]
pub struct LineSpan {
    /// The start of the span as an offset from the start of the source code.
    pub start: usize,
    /// The end of the span as an offset from the start of the source code.
    pub end: usize,
    /// The line this span starts on.
    pub line: usize,
}

/// A parse error.
#[derive(Debug, Clone)]
pub struct ParseError {
    /// The span of the token that produces this error.
    pub span: LineSpan,
    /// The error message.
    pub message: String,
}

impl ParseError {
    /// Create a new parse error.
    pub fn new<S: Into<String>>(line: usize, span: Span, message: S) -> ParseError {
        Self {
            span: LineSpan {
                line,
                start: span.start,
                end: span.end,
            },
            message: message.into(),
        }
    }

    /// Create a new parse error from a [`LineSpan`].
    ///
    /// [`LineSpan`]: crate::errors::LineSpan
    pub fn with_linespan<S: Into<String>>(span: LineSpan, message: S) -> ParseError {
        Self {
            span,
            message: message.into(),
        }
    }
}

/// The error context used for collecting and reporting errors.
#[derive(Debug, Clone)]
pub struct ErrCtx<'a> {
    /// The recorded errors.
    pub errors: Vec<ParseError>,
    /// The source code the errors originate from.
    pub source: &'a str,
    /// The source code split by `\n`.
    pub lines: Vec<&'a str>,
}

impl<'a> ErrCtx<'a> {
    /// Creates a new [`ErrCtx`] from the given PPGA source.
    ///
    /// [`ErrCtx`]: struct.ErrCtx
    pub fn new(source: &'a str) -> Self {
        Self {
            errors: vec![],
            source,
            lines: source.split("\n").collect::<Vec<_>>(),
        }
    }

    /// Records a [`ParseError`].
    ///
    /// [`ParseError`]: struct.ParseError
    pub fn record(&mut self, e: ParseError) {
        self.errors.push(e)
    }

    /// Returns `true` if one or more errors have been recorded.
    #[inline(always)]
    pub fn had_error(&self) -> bool {
        !self.errors.is_empty()
    }

    /// Reports all errors to STDERR.
    pub fn report_all(&self) {
        eprintln!("{}", self.report_to_string());
    }

    /// Reports all errors to a String.
    pub fn report_to_string(&self) -> String {
        let mut result = vec![];

        for e in &self.errors {
            let line = self
                .lines
                .get(e.span.line)
                .or_else(|| self.lines.last())
                .unwrap()
                .trim();
            let prefix = format!("[Line {:03}]", e.span.line + 1);
            result.push(format!(
                "{} ParseError at {}: {}",
                prefix,
                match self.source[e.span.start..e.span.end].trim() {
                    "" => "the end of the line".to_owned(),
                    lexeme => format!("`{}`", lexeme),
                },
                e.message
            ));
            result.push(format!("|\n|{}{}\n|", " ".repeat(4), line));
        }

        result.join("\n")
    }
}
