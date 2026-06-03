/// A document IR for pretty-printing, based on the Wadler-Lindig algorithm.
///
/// The key idea: describe document *structure* (groups, indentation, possible
/// line breaks) separately from *layout decisions* (does this group fit on one
/// line?). A single `Printer` pass then resolves every `Group` node: if its
/// flat content fits within the remaining line width the group stays flat;
/// otherwise it breaks.
///
/// A document tree that describes how to pretty-print some content.
#[derive(Clone)]
pub enum Doc {
    /// Literal text. Must not contain newlines — use `Hardline` for those.
    Text(String),
    /// A sequence of documents rendered one after another.
    Concat(Vec<Doc>),
    /// Increase the indent level for the inner document.
    Indent(Box<Doc>),
    /// Try to render the inner document flat (all on one line). If it doesn't
    /// fit within `max_width`, render it in broken mode instead.
    Group(Box<Doc>),
    /// In flat mode: a single space. In broken mode: newline + current indent.
    Line,
    /// In flat mode: nothing. In broken mode: newline + current indent.
    Softline,
    /// Always a newline + current indent, regardless of mode.
    Hardline,
    /// Greedy fill: items are placed on the current line if they fit,
    /// otherwise wrapped to a new line. Each item is a `Doc`.
    /// Separators between items are `Line` (space flat, newline broken).
    Fill(Vec<Doc>),
    /// Raw text emitted exactly as-is. The printer does NOT add indentation
    /// after embedded newlines. Used for `@format-ignore` blocks and as a
    /// bridge for pre-formatted string content during incremental migration.
    Verbatim(String),
    /// Two consecutive newlines (one blank line) + current indent.
    /// Used to preserve at most one blank line between expressions.
    BlankLine,
}

impl Doc {
    pub fn text(s: impl Into<String>) -> Self {
        Doc::Text(s.into())
    }

    pub fn concat(docs: Vec<Doc>) -> Self {
        Doc::Concat(docs)
    }

    pub fn indent(doc: Doc) -> Self {
        Doc::Indent(Box::new(doc))
    }

    pub fn group(doc: Doc) -> Self {
        Doc::Group(Box::new(doc))
    }

    pub fn verbatim(s: impl Into<String>) -> Self {
        Doc::Verbatim(s.into())
    }
}

#[derive(Clone, Copy)]
enum Mode {
    Flat,
    Break,
}

/// Renders a `Doc` tree to a string.
pub struct Printer<'a> {
    indent_unit: &'a str,
    max_width: usize,
    output: String,
    column: usize,
}

impl<'a> Printer<'a> {
    pub fn new(indent_unit: &'a str, max_width: usize, start_column: usize) -> Self {
        Printer {
            indent_unit,
            max_width,
            output: String::new(),
            column: start_column,
        }
    }

    /// Consume the printer and return the rendered string.
    pub fn render(mut self, doc: &Doc, base_indent: &str) -> String {
        self.print(doc, base_indent, Mode::Break);
        self.output
    }

    fn print(&mut self, doc: &Doc, indent_str: &str, mode: Mode) {
        match doc {
            Doc::Text(s) => {
                self.output.push_str(s);
                self.column += s.len();
            }
            Doc::Concat(docs) => {
                for d in docs {
                    self.print(d, indent_str, mode);
                }
            }
            Doc::Indent(inner) => {
                let deeper = format!("{indent_str}{}", self.indent_unit);
                self.print(inner, &deeper, mode);
            }
            Doc::Group(inner) => {
                if self.flat_fits(inner) {
                    self.print(inner, indent_str, Mode::Flat);
                } else {
                    self.print(inner, indent_str, Mode::Break);
                }
            }
            Doc::Line => match mode {
                Mode::Flat => {
                    self.output.push(' ');
                    self.column += 1;
                }
                Mode::Break => self.emit_newline(indent_str),
            },
            Doc::Softline => match mode {
                Mode::Flat => {}
                Mode::Break => self.emit_newline(indent_str),
            },
            Doc::Hardline => self.emit_newline(indent_str),
            Doc::Fill(items) => {
                for (i, item) in items.iter().enumerate() {
                    if i > 0 {
                        // Check if the next item fits on the current line
                        if self.flat_fits(item) {
                            self.output.push(' ');
                            self.column += 1;
                        } else {
                            self.emit_newline(indent_str);
                        }
                    }
                    self.print(item, indent_str, mode);
                }
            }
            Doc::Verbatim(s) => {
                self.output.push_str(s);
                // Update column to reflect the last line of the verbatim text
                if let Some(last_nl) = s.rfind('\n') {
                    self.column = s.len() - last_nl - 1;
                } else {
                    self.column += s.len();
                }
            }
            Doc::BlankLine => {
                self.output.push('\n');
                self.emit_newline(indent_str);
            }
        }
    }

    fn emit_newline(&mut self, indent_str: &str) {
        self.output.push('\n');
        self.output.push_str(indent_str);
        self.column = indent_str.len();
    }

    /// Check whether `doc` fits in the remaining line width when rendered flat.
    fn flat_fits(&self, doc: &Doc) -> bool {
        let remaining = self.max_width.saturating_sub(self.column) as isize;
        Self::measure_flat(doc, remaining)
    }

    fn measure_flat(doc: &Doc, mut remaining: isize) -> bool {
        let mut stack = vec![doc];
        while let Some(d) = stack.pop() {
            if remaining < 0 {
                return false;
            }
            match d {
                Doc::Text(s) => remaining -= s.len() as isize,
                Doc::Concat(docs) => {
                    for item in docs.iter().rev() {
                        stack.push(item);
                    }
                }
                Doc::Indent(inner) | Doc::Group(inner) => stack.push(inner),
                Doc::Line => remaining -= 1,
                Doc::Softline => {}
                Doc::Hardline | Doc::BlankLine => return false,
                Doc::Verbatim(s) => {
                    // Verbatim with newlines can't fit flat
                    if s.contains('\n') {
                        return false;
                    }
                    remaining -= s.len() as isize;
                }
                Doc::Fill(items) => {
                    for (i, item) in items.iter().enumerate().rev() {
                        if i > 0 {
                            remaining -= 1; // space separator in flat mode
                        }
                        stack.push(item);
                    }
                }
            }
        }
        remaining >= 0
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn print(doc: &Doc, width: usize) -> String {
        Printer::new("  ", width, 0).render(doc, "")
    }

    #[test]
    fn flat_group() {
        let doc = Doc::group(Doc::concat(vec![
            Doc::text("(ok"),
            Doc::indent(Doc::concat(vec![Doc::Line, Doc::text("true")])),
            Doc::Softline,
            Doc::text(")"),
        ]));
        assert_eq!(print(&doc, 80), "(ok true)");
    }

    #[test]
    fn broken_group() {
        let doc = Doc::group(Doc::concat(vec![
            Doc::text("(ok"),
            Doc::indent(Doc::concat(vec![Doc::Line, Doc::text("true")])),
            Doc::Softline,
            Doc::text(")"),
        ]));
        // width=5 forces break
        assert_eq!(print(&doc, 5), "(ok\n  true\n)");
    }

    #[test]
    fn nested_indent() {
        let inner = Doc::group(Doc::concat(vec![
            Doc::text("(inner"),
            Doc::indent(Doc::concat(vec![Doc::Line, Doc::text("a")])),
            Doc::Softline,
            Doc::text(")"),
        ]));
        let outer = Doc::group(Doc::concat(vec![
            Doc::text("(outer"),
            Doc::indent(Doc::concat(vec![Doc::Line, inner])),
            Doc::Softline,
            Doc::text(")"),
        ]));
        // Narrow enough to break outer but inner fits
        assert_eq!(print(&outer, 15), "(outer\n  (inner a)\n)");
    }

    #[test]
    fn hardline_forces_break() {
        let doc = Doc::group(Doc::concat(vec![
            Doc::text("(begin"),
            Doc::indent(Doc::concat(vec![
                Doc::Hardline,
                Doc::text("a"),
                Doc::Hardline,
                Doc::text("b"),
            ])),
            Doc::Hardline,
            Doc::text(")"),
        ]));
        // Hardline forces break even if it would fit flat
        assert_eq!(print(&doc, 80), "(begin\n  a\n  b\n)");
    }

    #[test]
    fn verbatim_single_line() {
        let doc = Doc::concat(vec![
            Doc::text("before "),
            Doc::verbatim("raw text"),
            Doc::text(" after"),
        ]);
        assert_eq!(print(&doc, 80), "before raw text after");
    }

    #[test]
    fn verbatim_multiline_no_indent() {
        // Verbatim content should NOT get indented by the printer
        let doc = Doc::indent(Doc::concat(vec![
            Doc::Hardline,
            Doc::text("normal"),
            Doc::Hardline,
            Doc::verbatim("line1\n  line2\nline3"),
        ]));
        // "normal" gets indented, but verbatim is emitted as-is
        assert_eq!(print(&doc, 80), "\n  normal\n  line1\n  line2\nline3");
    }

    #[test]
    fn verbatim_rejects_flat() {
        // A Group containing multiline Verbatim should break
        let doc = Doc::group(Doc::concat(vec![
            Doc::text("("),
            Doc::verbatim("a\nb"),
            Doc::text(")"),
        ]));
        // measure_flat returns false for multiline verbatim, so group breaks
        assert_eq!(print(&doc, 80), "(a\nb)");
    }

    #[test]
    fn blank_line_between_exprs() {
        let doc = Doc::concat(vec![Doc::text("a"), Doc::BlankLine, Doc::text("b")]);
        assert_eq!(print(&doc, 80), "a\n\nb");
    }

    #[test]
    fn blank_line_with_indent() {
        let doc = Doc::indent(Doc::concat(vec![
            Doc::Hardline,
            Doc::text("a"),
            Doc::BlankLine,
            Doc::text("b"),
        ]));
        assert_eq!(print(&doc, 80), "\n  a\n\n  b");
    }
}
