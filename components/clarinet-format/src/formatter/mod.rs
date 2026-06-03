pub mod doc;
pub mod helpers;
pub mod ignored;

use std::collections::HashMap;
use std::iter::Peekable;
use std::ops::Deref;
use std::{fmt, slice};

use clarinet_defaults::DEFAULT_EPOCH;
use clarity::types::StacksEpochId;
use clarity::vm::ast::stack_depth_checker::StackDepthLimits;
use clarity::vm::functions::define::DefineFunctions;
use clarity::vm::functions::NativeFunctions;
use clarity::vm::representations::{PreSymbolicExpression, PreSymbolicExpressionType};
use doc::{Doc, Printer};
use helpers::t;
use ignored::{extract_expr_source, extract_source_range};

pub enum Indentation {
    Space(usize),
    Tab,
}

impl fmt::Display for Indentation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Indentation::Space(count) => write!(f, "{}", " ".repeat(*count)),
            Indentation::Tab => write!(f, "\t"),
        }
    }
}

// or/and with > N comparisons will be split across multiple lines
// (or
//   true
//   (is-eq 1 1)
//   false
// )
const BOOLEAN_BREAK_LIMIT: usize = 2;

// commented blocks with this string included will not be formatted
const FORMAT_IGNORE_SYNTAX: &str = "@format-ignore";

pub struct Settings {
    pub indentation: Indentation,
    pub max_line_length: usize,
}

impl Settings {
    pub fn new(indentation: Indentation, max_line_length: usize) -> Self {
        Settings {
            indentation,
            max_line_length,
        }
    }
}

impl Default for Settings {
    fn default() -> Settings {
        Settings {
            indentation: Indentation::Space(2),
            max_line_length: 80,
        }
    }
}

/// Typed indentation that replaces raw `&str` threading.
/// Carries a precomputed string so indentation rendering is free.
/// Implements `Deref<Target=str>` so it works with `push_str`, `is_empty`, `len`, etc.
struct Indent {
    string: String,
}

impl Indent {
    fn new(base: &str) -> Self {
        Indent {
            string: base.to_string(),
        }
    }

    fn empty() -> Self {
        Indent {
            string: String::new(),
        }
    }

    /// Create a new Indent one level deeper.
    fn indented(&self, indent_unit: &str) -> Self {
        let mut string = self.string.clone();
        string.push_str(indent_unit);
        Indent { string }
    }
}

impl Deref for Indent {
    type Target = str;
    fn deref(&self) -> &str {
        &self.string
    }
}

impl fmt::Display for Indent {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(&self.string)
    }
}

pub struct ClarityFormatter {
    settings: Settings,
}
impl ClarityFormatter {
    pub fn new(settings: Settings) -> Self {
        Self { settings }
    }
    /// formatting for files to ensure a newline at the end
    pub fn format_file(&self, source: &str, epoch: Option<StacksEpochId>) -> String {
        let trimmed_source = source.trim_start_matches(['\n', '\r']);
        let pse = clarity::vm::ast::parser::v2::parse(
            trimmed_source,
            StackDepthLimits::for_epoch(epoch.unwrap_or(DEFAULT_EPOCH)),
        )
        .unwrap();
        let mut agg = Aggregator::new(&self.settings, &pse, Some(trimmed_source));
        let result = agg.generate();

        // make sure the file ends with a newline
        format!("{}\n", result.trim_end_matches(['\n', '\r']))
    }
    /// formatting an AST without a source file
    pub fn format_ast(&self, pse: &[PreSymbolicExpression]) -> String {
        let mut agg = Aggregator::new(&self.settings, pse, None);
        agg.generate()
    }
    /// Alias `format_file` to `format`
    pub fn format(&self, source: &str, epoch: Option<StacksEpochId>) -> String {
        self.format_file(source, epoch)
    }
    /// for range formatting within editors
    pub fn format_section(
        &self,
        source: &str,
        epoch: Option<StacksEpochId>,
    ) -> Result<String, String> {
        let pse = clarity::vm::ast::parser::v2::parse(
            source,
            StackDepthLimits::for_epoch(epoch.unwrap_or(DEFAULT_EPOCH)),
        )
        .map_err(|e| e.to_string())?;

        // range formatting specifies to the aggregator that we're
        // starting mid-source and thus should pre-populate
        // `previous_indentation` for format_source_exprs
        let indentation_level = source.chars().take_while(|c| c.is_whitespace()).count();
        let leading_spaces = &source[..indentation_level];
        let mut agg = Aggregator::new(&self.settings, &pse, Some(source));

        let result = agg.generate();
        Ok(if leading_spaces.is_empty() {
            result
        } else {
            format!("{leading_spaces}{result}")
        })
    }
}

/// Aggregator does the heavy lifting and generates the final output string.
/// all the formatting methods live within this struct.
pub struct Aggregator<'a> {
    settings: &'a Settings,
    pse: &'a [PreSymbolicExpression],
    source: Option<&'a str>,
    indentation_str: String,

    cache: HashMap<(usize, String), String>,
    ignored_exprs: HashMap<(u32, u32, u32, u32), String>,
}

impl<'a> Aggregator<'a> {
    pub fn new(
        settings: &'a Settings,
        pse: &'a [PreSymbolicExpression],
        source: Option<&'a str>,
    ) -> Self {
        let indentation_str = settings.indentation.to_string();
        Aggregator {
            settings,
            pse,
            source,
            indentation_str,
            cache: HashMap::new(),
            ignored_exprs: HashMap::new(),
        }
    }

    /// Render a Doc tree to a String using the current settings.
    fn render_doc(&self, doc: &Doc, indent: &Indent) -> String {
        Printer::new(
            &self.indentation_str,
            self.settings.max_line_length,
            indent.len(),
        )
        .render(doc, indent)
    }

    pub fn generate(&mut self) -> String {
        self.cache.clear();
        self.ignored_exprs.clear();
        // this handles if we're formatting a section of code rather than the whole file
        let indent = match self.source {
            Some(source) => {
                let level = source.chars().take_while(|c| c.is_whitespace()).count();
                Indent::new(&source[..level])
            }
            None => {
                let level = (self
                    .pse
                    .first()
                    .map_or(0, |expr| expr.span().start_column.saturating_sub(1)))
                    as usize;
                Indent::new(&" ".repeat(level))
            }
        };

        let formatted = self.format_source_exprs(self.pse, &indent);
        // If we're formatting an AST without a source and there's indentation,
        // we need to ensure it's applied to the beginning of the output
        if self.source.is_none() && !indent.is_empty() {
            // Only add indentation if we're formatting a top-level expression
            if self.pse.len() == 1 && formatted.starts_with('(') {
                return format!("{indent}{formatted}");
            }
        }

        formatted
    }

    // when format_source_exprs is called on one of these cached expressions the source will be returned as is
    fn cache_ignored_expression(&mut self, next_expr: &PreSymbolicExpression, source: &str) {
        let next_expr_span = next_expr.span();
        let next_expr_key = (
            next_expr_span.start_line,
            next_expr_span.start_column,
            next_expr_span.end_line,
            next_expr_span.end_column,
        );

        let lines: Vec<&str> = source.lines().collect();
        let end_line_usize = usize::try_from(next_expr_span.end_line).unwrap_or(0);
        let end_col = if end_line_usize > 0 && end_line_usize <= lines.len() {
            (lines[end_line_usize - 1].len() + 1) as u32
        } else {
            next_expr_span.end_column
        };

        let next_expr_extracted = extract_source_range(
            source,
            next_expr_span.start_line,
            next_expr_span.start_column,
            next_expr_span.end_line,
            end_col,
        );

        self.ignored_exprs
            .insert(next_expr_key, next_expr_extracted);
    }

    /// Handle a `@format-ignore` comment: extract the comment and the following
    /// expression as raw source text, cache the expression, and append to `result`.
    /// Returns the `end_line` of the last processed expression (for blank-line tracking).
    fn emit_format_ignored<'b>(
        &mut self,
        comment_expr: &'a PreSymbolicExpression,
        iter: &mut Peekable<impl Iterator<Item = &'b PreSymbolicExpression>>,
        result: &mut String,
        indent: &Indent,
    ) -> u32
    where
        'a: 'b,
    {
        let Some(source) = self.source else {
            // No source — just emit the comment text
            result.push_str(&self.display_pse(comment_expr, indent));
            return comment_expr.span().end_line;
        };

        // Check if the next expression is a list (the thing being ignored)
        let next_is_list = iter.peek().is_some_and(|next| next.match_list().is_some());
        if !next_is_list {
            // Next expression is not a list (or there is none) — just extract the comment
            result.push_str(&extract_expr_source(comment_expr, source));
            return comment_expr.span().end_line;
        }

        let next_expr = iter.next().unwrap();
        let end_line = next_expr.span().end_line;

        // Extend the end column to the end of the line so we capture trailing content
        let lines: Vec<&str> = source.lines().collect();
        let end_line_usize = usize::try_from(end_line).unwrap_or(0);
        let end_col = if end_line_usize > 0 && end_line_usize <= lines.len() {
            (lines[end_line_usize - 1].len() + 1) as u32
        } else {
            next_expr.span().end_column
        };

        // Extract comment + expression together from source
        let extracted = extract_source_range(
            source,
            comment_expr.span().start_line,
            comment_expr.span().start_column,
            end_line,
            end_col,
        );

        // Cache so other formatters return this expression verbatim
        self.cache_ignored_expression(next_expr, source);

        result.push_str(&extracted);
        if iter.peek().is_some() {
            result.push('\n');
        }

        end_line
    }

    /// If `expr` is a `@format-ignore` comment and `next_expr` is a list,
    /// cache the next expression so it's returned verbatim by `format_source_exprs`.
    fn check_and_cache_ignored(
        &mut self,
        expr: &PreSymbolicExpression,
        next_expr: Option<&PreSymbolicExpression>,
    ) {
        if !is_format_ignore(expr) {
            return;
        }
        let Some(next) = next_expr else { return };
        if next.match_list().is_none() {
            return;
        }
        let Some(src) = self.source else { return };
        self.cache_ignored_expression(next, src);
    }

    fn format_source_exprs(
        &mut self,
        expressions: &'a [PreSymbolicExpression],
        indent: &Indent,
    ) -> String {
        // if this expression was marked as ignored, return the cached source
        if expressions.len() == 1 {
            let expr = &expressions[0];
            let span_key = (
                expr.span().start_line,
                expr.span().start_column,
                expr.span().end_line,
                expr.span().end_column,
            );
            if let Some(ignored_source) = self.ignored_exprs.get(&span_key) {
                return ignored_source.clone();
            }
        }

        // Create a key based on the slice pointer and length for the whole array
        let key = (expressions.as_ptr() as usize, indent.to_string());

        if let Some(result) = self.cache.get(&key) {
            return result.clone();
        }
        // Track the end line of the previous expression
        let mut prev_end_line = 0;

        // use peekable to handle trailing comments nicely
        let mut iter = expressions.iter().peekable();
        let mut result = String::new(); // Accumulate results here

        while let Some(expr) = iter.next() {
            let trailing_comment = get_trailing_comment(expr, &mut iter);

            if is_format_ignore(expr) {
                prev_end_line = self.emit_format_ignored(expr, &mut iter, &mut result, indent);
                continue;
            }

            if prev_end_line > 0
                && expr.span().start_line > 0
                && expr.span().start_line > prev_end_line
            {
                let blank_lines = expr.span().start_line - prev_end_line - 1;
                let extra_newlines = std::cmp::min(blank_lines, 1);
                for _ in 0..extra_newlines {
                    result.push('\n');
                }
            }
            let (formatted, is_define) = if let Some(list) = expr.match_list() {
                if let Some(atom_name) = list.split_first().and_then(|(f, _)| f.match_atom()) {
                    (
                        self.format_list_call(atom_name, list, indent),
                        is_define_fn(atom_name),
                    )
                } else {
                    // list without a leading atom
                    (self.display_pse(expr, indent), false)
                }
            } else {
                (self.display_pse(expr, indent), false)
            };

            // Trailing comment handling (unified for all expression types)
            if let Some(comment) = trailing_comment {
                // if it's a top level expression, newline it
                if indent.is_empty() && result.ends_with(')') {
                    result.push('\n');
                }
                result.push_str(&formatted);
                result.push(' ');
                result.push_str(&self.display_pse(comment, indent));
                result.push('\n');
            } else if is_define {
                // Define-level expressions always get a trailing newline
                if indent.is_empty() && result.ends_with(')') {
                    result.push('\n');
                }
                result.push_str(&formatted);
                if !formatted.ends_with('\n') {
                    result.push('\n');
                }
            } else if let Some(list) = expr.match_list() {
                if list
                    .split_first()
                    .and_then(|(f, _)| f.match_atom())
                    .is_some()
                {
                    // Known or unknown function call — newline between top-level exprs
                    if indent.is_empty() && result.ends_with(')') {
                        result.push('\n');
                    }
                    result.push_str(&formatted);
                    if let Some(next) = iter.peek() {
                        if !is_same_line(expr, next) {
                            result.push('\n');
                        } else {
                            result.push(' ');
                        }
                    }
                } else {
                    result.push_str(&formatted);
                }
            } else {
                // Non-list expressions (atoms, values, etc.)
                result.push_str(&formatted);
                if let Some(next) = iter.peek() {
                    if !is_same_line(expr, next) || is_comment(expr) {
                        result.push('\n');
                    } else {
                        result.push(' ');
                    }
                }
            }

            prev_end_line = expr.span().end_line;
        }
        // Cache the result
        self.cache.insert(key, result.clone());
        result
    }

    /// Dispatch a list expression `(fn-name ...)` to the appropriate formatter.
    fn format_list_call(
        &mut self,
        atom_name: &str,
        list: &'a [PreSymbolicExpression],
        indent: &Indent,
    ) -> String {
        if let Some(native) = NativeFunctions::lookup_by_name(atom_name) {
            match native {
                NativeFunctions::Let => self.format_let(list, indent),
                NativeFunctions::Begin => self.format_begin(list, indent),
                NativeFunctions::Match => {
                    if contains_comments(list) {
                        self.match_with_comments(list, indent)
                    } else {
                        self.format_match(list, indent)
                    }
                }
                NativeFunctions::TupleCons => self.format_key_value(&list[1..], indent),
                NativeFunctions::If => self.format_if(list, indent),
                NativeFunctions::And | NativeFunctions::Or => self.format_booleans(list, indent),
                NativeFunctions::ListCons => self.format_list(list, indent),
                NativeFunctions::RestrictAssets => self.format_restrict_assets(list, indent),
                _ => self.format_inner_content(list, indent),
            }
        } else if let Some(define) = DefineFunctions::lookup_by_name(atom_name) {
            match define {
                DefineFunctions::PublicFunction
                | DefineFunctions::ReadOnlyFunction
                | DefineFunctions::PrivateFunction => self.function(list),
                DefineFunctions::Constant
                | DefineFunctions::PersistedVariable
                | DefineFunctions::FungibleToken
                | DefineFunctions::ImplTrait
                | DefineFunctions::UseTrait
                | DefineFunctions::NonFungibleToken => self.format_constant(list, indent),
                DefineFunctions::Map => self.format_map(list, indent),
                DefineFunctions::Trait => self.define_trait(list, indent),
            }
        } else {
            self.format_inner_content(list, indent)
        }
    }

    // (define-trait trait-name (
    //   (func1-name
    //     (arg1-type arg2-type ...)
    //     (return-type)
    //   )
    //   (func2-name (arg1-type arg2-type ...) (return-type))
    // )
    fn define_trait(&mut self, exprs: &'a [PreSymbolicExpression], indent: &Indent) -> String {
        let nested = indent.indented(&self.indentation_str);
        let name = self.format_source_exprs(slice::from_ref(&exprs[1]), indent);
        let methods = exprs[2].match_list().unwrap();

        if methods.is_empty() {
            let is_multiline = exprs[1].span().end_line != exprs[2].span().start_line;
            if is_multiline {
                let mut parts: Vec<Doc> = vec![Doc::text("(define-trait "), Doc::verbatim(name)];
                parts.push(Doc::indent(Doc::concat(vec![
                    Doc::Hardline,
                    Doc::verbatim(self.display_pse(&exprs[2], &nested)),
                ])));
                if exprs.len() > 3 && is_comment(&exprs[3]) {
                    parts.push(Doc::text(" "));
                    parts.push(Doc::verbatim(self.display_pse(&exprs[3], indent)));
                }
                parts.push(Doc::Hardline);
                parts.push(Doc::text(")"));
                return self.render_doc(&Doc::concat(parts), indent);
            } else {
                return self.render_doc(
                    &Doc::concat(vec![
                        Doc::text("(define-trait "),
                        Doc::verbatim(name),
                        Doc::text(" ())"),
                    ]),
                    indent,
                );
            }
        }

        let double_indent = nested.indented(&self.indentation_str);
        let mut method_parts: Vec<Doc> = Vec::new();

        let mut iter = methods.iter().peekable();
        while let Some(expr) = iter.next() {
            let trailing = get_trailing_comment(expr, &mut iter);

            if let Some(method_list) = expr.match_list() {
                let mut method_doc: Vec<Doc> = vec![Doc::text("(")];

                if let Some(method_name) = method_list.first() {
                    method_doc.push(Doc::verbatim(self.display_pse(method_name, indent)));
                }

                let mut items_iter = method_list.iter().skip(1).peekable();
                while let Some(arg) = items_iter.next() {
                    if let Some(element_list) = arg.match_list() {
                        method_doc.push(Doc::Hardline);
                        method_doc.push(Doc::verbatim(
                            self.display_list(element_list, &double_indent),
                        ));

                        if let Some(next_item) = items_iter.peek() {
                            if is_comment(next_item) {
                                let count =
                                    next_item.span().start_column - arg.span().end_column - 1;
                                let mut comment_text = String::new();
                                push_spaces(&mut comment_text, count);
                                comment_text.push_str(&self.display_pse(next_item, indent));
                                method_doc.push(Doc::verbatim(comment_text));
                                items_iter.next();
                            }
                        }
                    } else if is_comment(arg) {
                        method_doc.push(Doc::Hardline);
                        method_doc.push(Doc::verbatim(self.display_pse(arg, indent)));
                    }
                }

                if let Some(comment) = trailing {
                    if let Some(last_item) = method_list.last() {
                        let count = comment.span().start_column - last_item.span().end_column - 1;
                        let mut comment_text = String::new();
                        push_spaces(&mut comment_text, count);
                        comment_text.push_str(&self.display_pse(comment, indent));
                        method_doc.push(Doc::verbatim(comment_text));
                    }
                }

                // method_doc items are at double_indent level; wrap in indent(indent(...))
                method_parts.push(Doc::indent(Doc::concat(method_doc)));
                method_parts.push(Doc::Hardline);
                method_parts.push(Doc::text(")"));

                if iter.peek().is_some() {
                    method_parts.push(Doc::Hardline);
                }
            }
        }

        let doc = Doc::concat(vec![
            Doc::text("(define-trait "),
            Doc::verbatim(name),
            Doc::text(" ("),
            Doc::indent(Doc::concat(vec![Doc::Hardline, Doc::concat(method_parts)])),
            Doc::Hardline,
            Doc::text("))"),
        ]);
        self.render_doc(&doc, indent)
    }

    fn format_constant(&mut self, exprs: &'a [PreSymbolicExpression], indent: &Indent) -> String {
        let func_type = self.display_pse(exprs.first().unwrap(), &Indent::empty());
        let mut parts: Vec<Doc> = vec![Doc::text(format!("({func_type} "))];
        let mut iter = exprs[1..].iter().peekable();
        while let Some(expr) = iter.next() {
            let trailing = get_trailing_comment(expr, &mut iter);
            self.check_and_cache_ignored(expr, iter.peek().copied());
            parts.push(Doc::verbatim(
                self.format_source_exprs(slice::from_ref(expr), indent),
            ));
            if iter.peek().is_some() {
                parts.push(Doc::text(" "));
            }
            if let Some(comment) = trailing {
                parts.push(Doc::text(" "));
                parts.push(Doc::verbatim(self.display_pse(comment, indent)));
            }
        }
        parts.push(Doc::text(")"));
        self.render_doc(&Doc::concat(parts), indent)
    }

    fn format_map(&mut self, exprs: &'a [PreSymbolicExpression], indent: &Indent) -> String {
        let func_type = self.display_pse(exprs.first().unwrap(), &Indent::empty());
        let nested = indent.indented(&self.indentation_str);
        let name = self.format_source_exprs(slice::from_ref(&exprs[1]), indent);

        let mut body_parts: Vec<Doc> = Vec::new();
        let mut iter = exprs[2..].iter().peekable();
        while let Some(expr) = iter.next() {
            let trailing = get_trailing_comment(expr, &mut iter);
            self.check_and_cache_ignored(expr, iter.peek().copied());

            body_parts.push(Doc::Hardline);
            body_parts.push(Doc::verbatim(
                self.format_source_exprs(slice::from_ref(expr), &nested),
            ));
            if let Some(comment) = trailing {
                body_parts.push(Doc::text(" "));
                body_parts.push(Doc::verbatim(self.display_pse(comment, indent)));
            }
        }

        let doc = Doc::concat(vec![
            Doc::text(format!("({func_type} ")),
            Doc::verbatim(name),
            Doc::indent(Doc::concat(body_parts)),
            Doc::Hardline,
            Doc::text(")"),
        ]);
        self.render_doc(&doc, indent)
    }

    // *begin* never on one line
    fn format_begin(&mut self, exprs: &'a [PreSymbolicExpression], indent: &Indent) -> String {
        let nested = indent.indented(&self.indentation_str);
        let mut body_parts: Vec<Doc> = Vec::new();

        let mut iter = exprs.get(1..).unwrap_or_default().iter().peekable();
        let mut prev_end_line = None;

        while let Some(expr) = iter.next() {
            let trailing = get_trailing_comment(expr, &mut iter);

            // Preserve at most one blank line between expressions
            if has_blank_line(prev_end_line, expr.span().start_line) {
                body_parts.push(Doc::BlankLine);
            } else {
                body_parts.push(Doc::Hardline);
            }

            self.check_and_cache_ignored(expr, iter.peek().copied());

            let formatted = self.format_source_exprs(slice::from_ref(expr), &nested);
            body_parts.push(Doc::verbatim(formatted));

            if let Some(comment) = trailing {
                body_parts.push(Doc::text(" "));
                body_parts.push(Doc::verbatim(self.display_pse(comment, indent)));
            }

            prev_end_line = Some(expr.span().end_line);
        }

        let doc = Doc::concat(vec![
            Doc::text("(begin"),
            Doc::indent(Doc::concat(body_parts)),
            Doc::Hardline,
            Doc::text(")"),
        ]);
        self.render_doc(&doc, indent)
    }

    // formats (and ..) and (or ...)
    // if given more than BOOLEAN_BREAK_LIMIT expressions it will break it onto new lines
    fn format_booleans(&mut self, exprs: &'a [PreSymbolicExpression], indent: &Indent) -> String {
        let func_type = self.display_pse(exprs.first().unwrap(), indent);
        let nested = indent.indented(&self.indentation_str);
        // Estimate whether the expression fits on a single line
        let mut flat_width: usize = indent.len() + 1 + func_type.len() + 1; // "(or " + ")"
        let mut has_multiline_arg = false;
        for e in exprs[1..].iter().filter(|e| !is_comment(e)) {
            let formatted = self.format_source_exprs(slice::from_ref(e), indent);
            if formatted.contains('\n') {
                has_multiline_arg = true;
                break;
            }
            flat_width += formatted.len() + 1; // " arg"
        }
        let break_up = without_comments_len(&exprs[1..]) > BOOLEAN_BREAK_LIMIT
            || contains_comments(&exprs[1..])
            || has_multiline_arg
            || flat_width > self.settings.max_line_length;

        let mut iter = exprs.get(1..).unwrap_or_default().iter().peekable();
        let mut prev_end_line = None;
        let mut body_parts: Vec<Doc> = Vec::new();

        if break_up {
            while let Some(expr) = iter.next() {
                let trailing = get_trailing_comment(expr, &mut iter);
                self.check_and_cache_ignored(expr, iter.peek().copied());

                if has_blank_line(prev_end_line, expr.span().start_line) {
                    body_parts.push(Doc::BlankLine);
                } else {
                    body_parts.push(Doc::Hardline);
                }
                body_parts.push(Doc::verbatim(
                    self.format_source_exprs(slice::from_ref(expr), &nested),
                ));
                if let Some(comment) = trailing {
                    body_parts.push(Doc::text(" "));
                    body_parts.push(Doc::verbatim(self.display_pse(comment, indent)));
                }
                prev_end_line = Some(expr.span().end_line);
            }

            let doc = Doc::concat(vec![
                Doc::text(format!("({func_type}")),
                Doc::indent(Doc::concat(body_parts)),
                Doc::Hardline,
                Doc::text(")"),
            ]);
            self.render_doc(&doc, indent)
        } else {
            while let Some(expr) = iter.next() {
                let trailing = get_trailing_comment(expr, &mut iter);
                self.check_and_cache_ignored(expr, iter.peek().copied());
                body_parts.push(Doc::text(" "));
                body_parts.push(Doc::verbatim(
                    self.format_source_exprs(slice::from_ref(expr), indent),
                ));
                if let Some(comment) = trailing {
                    body_parts.push(Doc::text(" "));
                    body_parts.push(Doc::verbatim(self.display_pse(comment, indent)));
                    body_parts.push(Doc::Hardline);
                }
            }

            let doc = Doc::concat(vec![
                Doc::text(format!("({func_type}")),
                Doc::concat(body_parts),
                Doc::text(")"),
            ]);
            self.render_doc(&doc, indent)
        }
    }

    fn format_if(&mut self, exprs: &'a [PreSymbolicExpression], indent: &Indent) -> String {
        let opening = exprs.first().unwrap();
        let func_type = self.display_pse(opening, indent);
        let nested = indent.indented(&self.indentation_str);

        let mut body_parts: Vec<Doc> = Vec::new();
        let mut iter = exprs[1..].iter().peekable();
        let mut index = 0;
        let mut prev_end_line = None;

        while let Some(expr) = iter.next() {
            let trailing = get_trailing_comment(expr, &mut iter);
            self.check_and_cache_ignored(expr, iter.peek().copied());

            if index == 0 {
                // Condition goes on the same line as (if
                body_parts.push(Doc::text(" "));
            } else if has_blank_line(prev_end_line, expr.span().start_line) {
                body_parts.push(Doc::BlankLine);
            } else {
                body_parts.push(Doc::Hardline);
            }

            body_parts.push(Doc::verbatim(
                self.format_source_exprs(slice::from_ref(expr), &nested),
            ));

            if let Some(comment) = trailing {
                body_parts.push(Doc::text(" "));
                body_parts.push(Doc::verbatim(self.display_pse(comment, indent)));
            }

            index += 1;
            prev_end_line = Some(expr.span().end_line);
        }

        let doc = Doc::concat(vec![
            Doc::text(format!("({func_type}")),
            Doc::indent(Doc::concat(body_parts)),
            Doc::Hardline,
            Doc::text(")"),
        ]);
        self.render_doc(&doc, indent)
    }

    fn format_let(&mut self, exprs: &'a [PreSymbolicExpression], indent: &Indent) -> String {
        let nested = indent.indented(&self.indentation_str);
        let mut parts: Vec<Doc> = vec![Doc::text("(let (")];

        if let Some(args) = exprs[1].match_list() {
            if args.len() == 1 {
                parts.push(Doc::verbatim(
                    self.format_source_exprs(slice::from_ref(&args[0]), &nested),
                ));
                parts.push(Doc::text(")"));
            } else {
                let double_indent = nested.indented(&self.indentation_str);
                let mut binding_parts: Vec<Doc> = Vec::new();
                let mut iter = args.iter().peekable();
                while let Some(arg) = iter.next() {
                    let trailing = get_trailing_comment(arg, &mut iter);
                    self.check_and_cache_ignored(arg, iter.peek().copied());
                    binding_parts.push(Doc::Hardline);
                    binding_parts.push(Doc::verbatim(
                        self.format_source_exprs(slice::from_ref(arg), &double_indent),
                    ));
                    if let Some(comment) = trailing {
                        binding_parts.push(Doc::text(" "));
                        binding_parts.push(Doc::verbatim(self.display_pse(comment, indent)));
                    }
                }
                // Double-indent for bindings, close paren at single-indent
                parts.push(Doc::indent(Doc::concat(vec![
                    Doc::indent(Doc::concat(binding_parts)),
                    Doc::Hardline,
                    Doc::text(")"),
                ])));
            }
        }

        // let body
        let mut body_parts: Vec<Doc> = Vec::new();
        let mut prev_end_line = None;
        let body_exprs = exprs.get(2..).unwrap_or_default();
        for (i, e) in body_exprs.iter().enumerate() {
            if has_blank_line(prev_end_line, e.span().start_line) {
                body_parts.push(Doc::BlankLine);
            } else {
                body_parts.push(Doc::Hardline);
            }
            self.check_and_cache_ignored(e, body_exprs.get(i + 1));
            body_parts.push(Doc::verbatim(
                self.format_source_exprs(slice::from_ref(e), &nested),
            ));
            prev_end_line = Some(e.span().end_line);
        }
        parts.push(Doc::indent(Doc::concat(body_parts)));
        parts.push(Doc::Hardline);
        parts.push(Doc::text(")"));

        let doc = Doc::concat(parts);
        self.render_doc(&doc, indent)
    }

    // * match *
    // always multiple lines
    fn format_match(&mut self, exprs: &'a [PreSymbolicExpression], indent: &Indent) -> String {
        let nested = indent.indented(&self.indentation_str);

        // value to match on
        let match_value = self.format_source_exprs(slice::from_ref(&exprs[1]), indent);

        let mut branch_parts: Vec<Doc> = Vec::new();
        let mut iter = exprs[2..].iter().peekable();
        while let Some(branch) = iter.next() {
            let trailing = get_trailing_comment(branch, &mut iter);
            self.check_and_cache_ignored(branch, iter.peek().copied());
            let is_binding = branch.match_list().is_none() && iter.peek().is_some();

            branch_parts.push(Doc::Hardline);
            branch_parts.push(Doc::verbatim(
                self.format_source_exprs(slice::from_ref(branch), &nested),
            ));

            // If this is a binding pattern, add the next expression on the same line
            if is_binding {
                if let Some(expr_part) = iter.next() {
                    let expr_trailing = get_trailing_comment(expr_part, &mut iter);
                    branch_parts.push(Doc::text(" "));
                    branch_parts.push(Doc::verbatim(
                        self.format_source_exprs(slice::from_ref(expr_part), &nested),
                    ));
                    if let Some(comment) = expr_trailing {
                        branch_parts.push(Doc::text(" "));
                        branch_parts.push(Doc::verbatim(self.display_pse(comment, indent)));
                    }
                }
            }
            if let Some(comment) = trailing {
                branch_parts.push(Doc::text(" "));
                branch_parts.push(Doc::verbatim(self.display_pse(comment, indent)));
            }
        }

        let doc = Doc::concat(vec![
            Doc::text("(match "),
            Doc::verbatim(match_value),
            Doc::indent(Doc::concat(branch_parts)),
            Doc::Hardline,
            Doc::text(")"),
        ]);
        self.render_doc(&doc, indent)
    }

    /// Special case for match with comments in line.
    /// aligns all bindings and values
    fn match_with_comments(
        &mut self,
        exprs: &'a [PreSymbolicExpression],
        indent: &Indent,
    ) -> String {
        let nested = indent.indented(&self.indentation_str);

        // value to match on
        let match_value = self.format_source_exprs(slice::from_ref(&exprs[1]), indent);

        let mut branch_parts: Vec<Doc> = Vec::new();
        let mut iter = exprs[2..].iter().peekable();
        while let Some(branch) = iter.next() {
            let trailing = get_trailing_comment(branch, &mut iter);
            self.check_and_cache_ignored(branch, iter.peek().copied());
            branch_parts.push(Doc::Hardline);
            branch_parts.push(Doc::verbatim(
                self.format_source_exprs(slice::from_ref(branch), &nested),
            ));
            if let Some(comment) = trailing {
                branch_parts.push(Doc::text(" "));
                branch_parts.push(Doc::verbatim(self.display_pse(comment, indent)));
            }
        }

        let doc = Doc::concat(vec![
            Doc::text("(match "),
            Doc::verbatim(match_value),
            Doc::indent(Doc::concat(branch_parts)),
            Doc::Hardline,
            Doc::text(")"),
        ]);
        self.render_doc(&doc, indent)
    }

    /// Format a list of expressions as `(item1 item2 ...)`.
    /// Breaks to multi-line if any item has a trailing comment or
    /// the first-line width exceeds `max_line_length`.
    fn display_list(&mut self, exprs: &'a [PreSymbolicExpression], indent: &Indent) -> String {
        struct Item {
            text: String,
            has_comment: bool,
        }
        let mut items: Vec<Item> = Vec::new();
        let mut iter = exprs.iter().peekable();
        while let Some(item) = iter.next() {
            let trailing = get_trailing_comment(item, &mut iter);
            self.check_and_cache_ignored(item, iter.peek().copied());
            let mut text = self.format_source_exprs(slice::from_ref(item), indent);
            let has_comment = trailing.is_some();
            if let Some(comment) = trailing {
                let count = comment
                    .span()
                    .start_column
                    .saturating_sub(item.span().end_column + 1);
                push_spaces(&mut text, count);
                text.push_str(&self.display_pse(comment, indent));
            }
            items.push(Item { text, has_comment });
        }

        // Decide whether to break: comments force break, otherwise check width
        let has_any_comment = items.iter().any(|it| it.has_comment);
        let first_line_width = indent.len()
            + 1 // opening paren
            + items.iter().map(|it| first_line_len(&it.text)).sum::<usize>()
            + items.len().saturating_sub(1); // spaces between items
        let should_break = has_any_comment || first_line_width + 1 > self.settings.max_line_length;

        let doc = if should_break {
            let mut inner_parts: Vec<Doc> = Vec::new();
            for (i, item) in items.into_iter().enumerate() {
                if i > 0 {
                    inner_parts.push(Doc::Hardline);
                }
                inner_parts.push(Doc::verbatim(item.text));
            }
            Doc::concat(vec![
                Doc::text("("),
                Doc::indent(Doc::concat(vec![Doc::Hardline, Doc::concat(inner_parts)])),
                Doc::Hardline,
                Doc::text(")"),
            ])
        } else {
            let mut inner_parts: Vec<Doc> = Vec::new();
            for (i, item) in items.into_iter().enumerate() {
                if i > 0 {
                    inner_parts.push(Doc::text(" "));
                }
                inner_parts.push(Doc::verbatim(item.text));
            }
            Doc::concat(vec![
                Doc::text("("),
                Doc::concat(inner_parts),
                Doc::text(")"),
            ])
        };
        self.render_doc(&doc, indent)
    }

    fn format_restrict_assets(
        &mut self,
        exprs: &'a [PreSymbolicExpression],
        indent: &Indent,
    ) -> String {
        let nested = indent.indented(&self.indentation_str);
        let owner = self.format_source_exprs(slice::from_ref(&exprs[1]), indent);

        let mut parts: Vec<Doc> = vec![Doc::text("(restrict-assets? "), Doc::verbatim(owner)];

        // allowances
        if let Some(allowances_list) = exprs.get(2) {
            if let Some(allowances) = allowances_list.match_list() {
                if allowances.len() == 1 {
                    parts.push(Doc::text(" "));
                    parts.push(Doc::verbatim(
                        self.format_source_exprs(slice::from_ref(allowances_list), indent),
                    ));
                } else {
                    let double_indent = nested.indented(&self.indentation_str);
                    let mut allowance_parts: Vec<Doc> = Vec::new();
                    let mut iter = allowances.iter().peekable();
                    while let Some(allowance) = iter.next() {
                        let trailing = get_trailing_comment(allowance, &mut iter);
                        self.check_and_cache_ignored(allowance, iter.peek().copied());
                        allowance_parts.push(Doc::Hardline);
                        allowance_parts.push(Doc::verbatim(
                            self.format_source_exprs(slice::from_ref(allowance), &double_indent),
                        ));
                        if let Some(comment) = trailing {
                            allowance_parts.push(Doc::text(" "));
                            allowance_parts.push(Doc::verbatim(self.display_pse(comment, indent)));
                        }
                    }
                    parts.push(Doc::text(" ("));
                    parts.push(Doc::indent(Doc::concat(vec![
                        Doc::indent(Doc::concat(allowance_parts)),
                        Doc::Hardline,
                        Doc::text(")"),
                    ])));
                }
            }
        }

        // body expressions
        let mut body_parts: Vec<Doc> = Vec::new();
        let mut prev_end_line = None;
        let body_exprs = exprs.get(3..).unwrap_or_default();
        for (i, e) in body_exprs.iter().enumerate() {
            if has_blank_line(prev_end_line, e.span().start_line) {
                body_parts.push(Doc::BlankLine);
            } else {
                body_parts.push(Doc::Hardline);
            }
            self.check_and_cache_ignored(e, body_exprs.get(i + 1));
            body_parts.push(Doc::verbatim(
                self.format_source_exprs(slice::from_ref(e), &nested),
            ));
            prev_end_line = Some(e.span().end_line);
        }
        parts.push(Doc::indent(Doc::concat(body_parts)));
        parts.push(Doc::Hardline);
        parts.push(Doc::text(")"));

        let doc = Doc::concat(parts);
        self.render_doc(&doc, indent)
    }

    /// Check if an expression represents a signed integer (for list type size detection)
    /// Only signed integers are valid for list type signatures: (list 10 <type>)
    /// Unsigned integers like u10 would be list elements, not type signatures
    fn is_integer_expr(&mut self, expr: &PreSymbolicExpression) -> bool {
        match &expr.pre_expr {
            PreSymbolicExpressionType::AtomValue(ref value) => {
                matches!(value, clarity::vm::types::Value::Int(_))
            }
            _ => false,
        }
    }

    /// Detect if this is a list type signature: (list <integer> <type>)
    fn is_list_type_signature(&mut self, exprs: &[PreSymbolicExpression]) -> bool {
        // the 1st item is a different type than the 2nd
        exprs.len() >= 3
            && exprs[0].match_atom() == Some(&clarity::vm::ClarityName::from_literal("list"))
            && self.is_integer_expr(&exprs[1])
            && !self.is_integer_expr(&exprs[2])
    }

    /// Estimate the length of a list if formatted on a single line
    fn estimate_list_length(
        &mut self,
        exprs: &'a [PreSymbolicExpression],
        start_index: usize,
        prefix_len: usize,
    ) -> usize {
        let mut estimated_len = prefix_len;
        let empty = Indent::empty();
        for item in &exprs[start_index..] {
            let display = self.display_pse(item, &empty);
            estimated_len += display.len() + 1; // display + space
        }
        estimated_len + 1 // closing paren
    }

    /// Estimate the width of key-value sugar `{ k1: v1, k2: v2 }` on a single line.
    fn estimate_kv_width(&mut self, exprs: &'a [PreSymbolicExpression], indent: &Indent) -> usize {
        let empty = Indent::empty();
        // "{ " + entries + " }"
        let mut width = indent.len() + 4;
        for (i, expr) in exprs.iter().enumerate() {
            if is_comment(expr) {
                // Comments always force multiline
                return usize::MAX;
            }
            let display = self.display_pse(expr, &empty);
            if i % 2 == 0 {
                // key
                width += display.len() + 2; // "key: "
            } else {
                // value
                width += display.len() + 2; // "value, "
            }
        }
        width
    }

    /// Format the size value for a list type signature, keeping it on the same line as "list"
    fn format_list(&mut self, exprs: &'a [PreSymbolicExpression], indent: &Indent) -> String {
        let nested = indent.indented(&self.indentation_str);
        let mut start_index = 0;

        let is_list_cons = self.display_pse(&exprs[0], indent) == "list";
        if is_list_cons {
            start_index = 1;
        }

        let is_list_type_sig = self.is_list_type_signature(exprs);

        // Determine if list should be multiline based on if it fits on one line
        let is_multiline = if is_list_cons && exprs.len() > start_index {
            let estimated_len = self.estimate_list_length(exprs, start_index, 6); // "(list "
            indent.len() + estimated_len > self.settings.max_line_length
        } else {
            let estimated_len = self.estimate_list_length(exprs, start_index, 1); // opening paren
            indent.len() + estimated_len > self.settings.max_line_length
        };

        let spacing = if is_multiline { &nested } else { indent };

        // Collect formatted items with optional trailing comments
        struct ListItem {
            text: String,
            comment: Option<String>,
        }
        let mut items: Vec<ListItem> = Vec::new();
        let mut type_sig_size: Option<String> = None;

        let mut iter = exprs[start_index..].iter().peekable();
        let mut is_first_item = true;
        while let Some(item) = iter.next() {
            let trailing = get_trailing_comment(item, &mut iter);
            self.check_and_cache_ignored(item, iter.peek().copied());

            // Special handling for first item in type signatures
            if is_multiline && is_first_item && is_list_type_sig {
                let size_value = self.display_pse(item, indent);
                let size_clean = size_value.trim().replace('\n', " ").replace('\r', "");
                let mut size_text = size_clean;
                if let Some(comment) = trailing {
                    let count = comment
                        .span()
                        .start_column
                        .saturating_sub(item.span().end_column + 1);
                    push_spaces(&mut size_text, count);
                    size_text.push_str(&self.display_pse(comment, indent));
                }
                type_sig_size = Some(size_text);
                is_first_item = false;
                continue;
            }

            let text = self.format_source_exprs(slice::from_ref(item), spacing);
            let comment = trailing.map(|c| {
                let count = c
                    .span()
                    .start_column
                    .saturating_sub(item.span().end_column + 1);
                let mut s = String::new();
                push_spaces(&mut s, count);
                s.push_str(&self.display_pse(c, indent));
                s
            });
            items.push(ListItem { text, comment });
            is_first_item = false;
        }

        if !is_multiline {
            // Single line
            let mut parts: Vec<Doc> = Vec::new();
            if is_list_cons {
                parts.push(Doc::text("(list"));
                if !items.is_empty() {
                    parts.push(Doc::text(" "));
                }
            } else {
                parts.push(Doc::text("("));
            }
            for (i, item) in items.iter().enumerate() {
                if i > 0 {
                    parts.push(Doc::text(" "));
                }
                parts.push(Doc::verbatim(&item.text));
                if let Some(ref c) = item.comment {
                    parts.push(Doc::verbatim(c));
                }
            }
            parts.push(Doc::text(")"));
            let result = self.render_doc(&Doc::concat(parts), indent);
            let trimmed = t(&result);
            if trimmed.len() == result.len() {
                return result;
            }
            return trimmed.to_string();
        }

        // Multiline
        let mut parts: Vec<Doc> = Vec::new();
        if is_list_cons {
            parts.push(Doc::text("(list"));
        } else {
            parts.push(Doc::text("("));
        }

        if is_list_type_sig {
            // Type sig: (list <size>\n  <type>\n)
            if let Some(size_text) = type_sig_size {
                parts.push(Doc::text(" "));
                parts.push(Doc::verbatim(size_text));
            }
            let mut body: Vec<Doc> = Vec::new();
            for item in &items {
                body.push(Doc::Hardline);
                body.push(Doc::verbatim(&item.text));
                if let Some(ref c) = item.comment {
                    body.push(Doc::verbatim(c));
                }
            }
            parts.push(Doc::indent(Doc::concat(body)));
        } else {
            // Greedy wrapping with lookahead (matches old maybe_wrap_item behavior)
            let mut body: Vec<Doc> = vec![Doc::Hardline];
            let mut col = nested.len(); // column after first hardline + indent

            for (i, item) in items.iter().enumerate() {
                let item_doc = if let Some(ref c) = item.comment {
                    Doc::concat(vec![Doc::verbatim(&item.text), Doc::verbatim(c)])
                } else {
                    Doc::verbatim(&item.text)
                };

                let is_multiline_item = item.text.contains('\n');

                if i == 0 {
                    body.push(item_doc);
                    col = last_line_len(&item.text, col);
                    if let Some(ref c) = item.comment {
                        col += c.len();
                    }
                } else if is_multiline_item {
                    // Multiline items always get their own line
                    body.push(Doc::Hardline);
                    body.push(item_doc);
                    col = last_line_len(&item.text, nested.len());
                    if let Some(ref c) = item.comment {
                        col += c.len();
                    }
                } else {
                    // Lookahead: check if this + next item fit on current line
                    // Use full text length for next item (matches old maybe_wrap_item behavior)
                    let item_len = item.text.len();
                    let next_len = items.get(i + 1).map(|n| n.text.len());

                    let would_exceed = if let Some(nl) = next_len {
                        col + 1 + item_len + 1 + nl > self.settings.max_line_length
                    } else {
                        col + 1 + item_len > self.settings.max_line_length
                    };

                    if would_exceed {
                        body.push(Doc::Hardline);
                        col = nested.len();
                    } else {
                        body.push(Doc::text(" "));
                        col += 1;
                    }
                    body.push(item_doc);
                    col += item_len;
                    if let Some(ref c) = item.comment {
                        col += c.len();
                    }
                }
            }
            parts.push(Doc::indent(Doc::concat(body)));
        }

        parts.push(Doc::Hardline);
        parts.push(Doc::text(")"));
        let result = self.render_doc(&Doc::concat(parts), indent);
        let trimmed = t(&result);
        if trimmed.len() == result.len() {
            result
        } else {
            trimmed.to_string()
        }
    }

    // used for { n1: 1 } syntax
    fn format_key_value_sugar(
        &mut self,
        exprs: &'a [PreSymbolicExpression],
        indent: &Indent,
    ) -> String {
        let nested = indent.indented(&self.indentation_str);
        let double_indent = nested.indented(&self.indentation_str);
        let over_2_kvs = without_comments_len(exprs) > 2;

        let estimated_width = self.estimate_kv_width(exprs, indent);
        if over_2_kvs || estimated_width > self.settings.max_line_length {
            let mut entry_parts: Vec<Doc> = Vec::new();
            let mut iter = exprs.iter().peekable();
            while let Some(key) = iter.next() {
                if is_comment(key) {
                    entry_parts.push(Doc::Hardline);
                    entry_parts.push(Doc::verbatim(self.display_pse(key, indent)));
                    continue;
                }
                self.check_and_cache_ignored(key, iter.peek().copied());
                let key_str = self.format_source_exprs(slice::from_ref(key), &nested);
                if let Some(value) = iter.next() {
                    entry_parts.push(Doc::Hardline);
                    if is_comment(value) {
                        entry_parts.push(Doc::verbatim(format!("{key_str}:")));
                        // Comment between key and value
                        entry_parts.push(Doc::indent(Doc::concat(vec![
                            Doc::Hardline,
                            Doc::verbatim(self.display_pse(value, &nested)),
                        ])));
                        if let Some(actual_value) = iter.next() {
                            let trailing = get_trailing_comment(actual_value, &mut iter);
                            let value_str = self
                                .format_source_exprs(slice::from_ref(actual_value), &double_indent);
                            entry_parts.push(Doc::indent(Doc::concat(vec![
                                Doc::Hardline,
                                Doc::verbatim(format!("{value_str},")),
                            ])));
                            if let Some(comment) = trailing {
                                entry_parts.push(Doc::text(" "));
                                entry_parts.push(Doc::verbatim(self.display_pse(comment, &nested)));
                            }
                        }
                    } else {
                        let trailing = get_trailing_comment(value, &mut iter);
                        self.check_and_cache_ignored(value, iter.peek().copied());
                        let value_str = self.format_source_exprs(slice::from_ref(value), &nested);
                        entry_parts.push(Doc::verbatim(format!("{key_str}: {value_str},")));
                        if let Some(comment) = trailing {
                            entry_parts.push(Doc::text(" "));
                            entry_parts.push(Doc::verbatim(self.display_pse(comment, indent)));
                        }
                    }
                }
            }

            let doc = Doc::concat(vec![
                Doc::text("{"),
                Doc::indent(Doc::concat(entry_parts)),
                Doc::Hardline,
                Doc::text("}"),
            ]);
            self.render_doc(&doc, indent)
        } else {
            let fkey = self.display_pse(&exprs[0], indent);
            let fvalue = self.format_source_exprs(slice::from_ref(&exprs[1]), indent);
            let doc = Doc::concat(vec![
                Doc::text("{ "),
                Doc::verbatim(fkey),
                Doc::text(": "),
                Doc::verbatim(fvalue),
                Doc::text(" }"),
            ]);
            self.render_doc(&doc, indent)
        }
    }

    // used for (tuple (n1  1)) syntax
    // Note: Converted to a { a: 1 } style map
    fn format_key_value(&mut self, exprs: &'a [PreSymbolicExpression], indent: &Indent) -> String {
        let nested = indent.indented(&self.indentation_str);
        let multiline = exprs.len() > 1;

        if multiline {
            let mut entry_parts: Vec<Doc> = Vec::new();
            let mut iter = exprs.iter().peekable();
            while let Some(arg) = iter.next() {
                let trailing = get_trailing_comment(arg, &mut iter);
                self.check_and_cache_ignored(arg, iter.peek().copied());
                let (key, value) = arg
                    .match_list()
                    .and_then(|list| list.split_first())
                    .unwrap();
                let fkey = self.display_pse(key, &nested);
                let fvalue = self.format_source_exprs(value, &nested);
                entry_parts.push(Doc::Hardline);
                entry_parts.push(Doc::verbatim(format!("{fkey}: {fvalue},")));
                if let Some(comment) = trailing {
                    entry_parts.push(Doc::text(" "));
                    entry_parts.push(Doc::verbatim(self.display_pse(comment, &nested)));
                }
            }
            let doc = Doc::concat(vec![
                Doc::text("{"),
                Doc::indent(Doc::concat(entry_parts)),
                Doc::Hardline,
                Doc::text("}"),
            ]);
            self.render_doc(&doc, indent)
        } else {
            let (key, value) = exprs[0]
                .match_list()
                .and_then(|list| list.split_first())
                .unwrap();
            let fkey = self.display_pse(key, indent);
            let fvalue = self.format_source_exprs(value, indent);
            let doc = Doc::concat(vec![
                Doc::text("{ "),
                Doc::verbatim(fkey),
                Doc::text(": "),
                Doc::verbatim(fvalue),
                Doc::text(" }"),
            ]);
            self.render_doc(&doc, indent)
        }
    }

    // This prints leaves of the PSE tree
    fn display_pse(&mut self, pse: &'a PreSymbolicExpression, indent: &Indent) -> String {
        let key = (
            pse as *const PreSymbolicExpression as usize,
            indent.to_string(),
        );

        if let Some(result) = self.cache.get(&key) {
            return result.clone();
        }
        let result = match pse.pre_expr {
            PreSymbolicExpressionType::Atom(ref value) => t(value.as_str()).to_string(),
            PreSymbolicExpressionType::AtomValue(ref value) => match value {
                clarity::vm::types::Value::Principal(c) => {
                    format!("'{c}")
                }
                clarity::vm::types::Value::Sequence(clarity::vm::types::SequenceData::String(
                    ref string_data,
                )) => {
                    match string_data {
                        clarity::vm::types::CharType::ASCII(ascii_data) => {
                            let content = String::from_utf8_lossy(&ascii_data.data);
                            format!("\"{content}\"")
                        }
                        clarity::vm::types::CharType::UTF8(_) => {
                            // utf8 strings use the original source to preserve formatting
                            // note: we could just apply this to both utf8
                            // and ascii but format! is much faster than
                            // using extract_expr_source
                            self.source
                                .map(|source| extract_expr_source(pse, source))
                                .filter(|extracted| !extracted.is_empty())
                                .unwrap_or_else(|| value.to_string())
                        }
                    }
                }
                clarity::vm::types::Value::Sequence(_) => value.to_string(),
                _ => value.to_string(),
            },
            PreSymbolicExpressionType::List(ref items) => self.display_list(items, indent),
            PreSymbolicExpressionType::Tuple(ref items) => {
                self.format_key_value_sugar(items, indent)
            }
            PreSymbolicExpressionType::SugaredContractIdentifier(ref name) => {
                format!(".{name}")
            }
            PreSymbolicExpressionType::SugaredFieldIdentifier(ref contract, ref field) => {
                format!(".{contract}.{field}")
            }
            PreSymbolicExpressionType::FieldIdentifier(ref trait_id) => {
                format!("'{trait_id}")
            }
            PreSymbolicExpressionType::TraitReference(ref name) => {
                format!("<{name}>")
            }
            PreSymbolicExpressionType::Comment(ref text) => {
                if text.is_empty() {
                    ";;".to_string()
                } else {
                    comment_piece(text, pse)
                }
            }
            PreSymbolicExpressionType::Placeholder(ref placeholder) => {
                placeholder.to_string() // Placeholder is for if parsing fails
            }
        };
        self.cache.insert(key, result.clone());

        result
    }

    // Top level define-<function> should have a line break above and after (except on first line)
    // Functions always on multiple lines, even if short
    fn function(&mut self, exprs: &'a [PreSymbolicExpression]) -> String {
        let empty = Indent::empty();
        let func_type = self.display_pse(exprs.first().unwrap(), &empty);
        let indentation = self.indentation_str.clone();
        let one_indent = empty.indented(&indentation);
        let args_indent = one_indent.indented(&indentation);

        let mut parts: Vec<Doc> = vec![Doc::text(format!("({func_type} ("))];

        // function name and arguments
        if let Some(def) = exprs.get(1).and_then(|f| f.match_list()) {
            if let Some((name, args)) = def.split_first() {
                parts.push(Doc::verbatim(self.display_pse(name, &empty)));

                if args.len() == 1 {
                    parts.push(Doc::text(" "));
                    parts.push(Doc::verbatim(
                        self.format_source_exprs(slice::from_ref(&args[0]), &empty),
                    ));
                    parts.push(Doc::text(")"));
                } else {
                    let mut arg_parts: Vec<Doc> = Vec::new();
                    let mut iter = args.iter().peekable();
                    let mut prev_end_line = 0u32;
                    while let Some(arg) = iter.next() {
                        let trailing = get_trailing_comment(arg, &mut iter);
                        self.check_and_cache_ignored(arg, iter.peek().copied());
                        let need_newline = prev_end_line == 0
                            || arg.match_list().is_some()
                            || arg.span().start_line > prev_end_line;
                        if need_newline {
                            arg_parts.push(Doc::Hardline);
                        }
                        arg_parts.push(Doc::verbatim(
                            self.format_source_exprs(slice::from_ref(arg), &args_indent),
                        ));
                        if let Some(comment) = trailing {
                            arg_parts.push(Doc::text(" "));
                            arg_parts.push(Doc::verbatim(self.display_pse(comment, &empty)));
                            prev_end_line = comment.span().end_line;
                        } else {
                            prev_end_line = arg.span().end_line;
                        }
                    }
                    // Args at double indent
                    parts.push(Doc::indent(Doc::indent(Doc::concat(arg_parts))));
                    if !args.is_empty() {
                        // Close args paren at single indent
                        parts.push(Doc::indent(Doc::concat(vec![
                            Doc::Hardline,
                            Doc::text(")"),
                        ])));
                    } else {
                        parts.push(Doc::text(")"));
                    }
                }
            }
        }

        // function body expressions
        let mut body_parts: Vec<Doc> = Vec::new();
        let body_exprs = exprs.get(2..).unwrap_or_default();
        for (i, expr) in body_exprs.iter().enumerate() {
            self.check_and_cache_ignored(expr, body_exprs.get(i + 1));
            body_parts.push(Doc::Hardline);
            body_parts.push(Doc::verbatim(
                self.format_source_exprs(slice::from_ref(expr), &one_indent),
            ));
        }
        parts.push(Doc::indent(Doc::concat(body_parts)));
        parts.push(Doc::Hardline);
        parts.push(Doc::text(")"));
        parts.push(Doc::Hardline);

        self.render_doc(&Doc::concat(parts), &empty)
    }

    /// Format a generic function call as `(fn-name arg1 arg2 ...)`.
    /// Items are placed on the current line if they fit, wrapped to the next
    /// line (with increased indentation) otherwise.
    fn format_inner_content(
        &mut self,
        list: &'a [PreSymbolicExpression],
        indent: &Indent,
    ) -> String {
        let mut result = String::new();
        let mut current_line_width = indent.len();
        let mut first_on_line = true;
        let mut broken_up = false;
        let base_indent = indent.indented(&self.indentation_str);

        // Simple wrappers like (ok expr) are always kept on one line
        if list.len() == 2 && list[0].match_atom().is_some() {
            let atom_name = list[0].match_atom().unwrap();
            let is_special_format =
                NativeFunctions::lookup_by_name(atom_name).is_some_and(|native| {
                    matches!(
                        native,
                        NativeFunctions::Let
                            | NativeFunctions::Begin
                            | NativeFunctions::Match
                            | NativeFunctions::TupleCons
                            | NativeFunctions::If
                    )
                });

            if !is_special_format {
                let fn_name = self.format_source_exprs(slice::from_ref(&list[0]), indent);
                let arg = self.format_source_exprs(slice::from_ref(&list[1]), indent);
                return format!("({} {})", fn_name.trim(), arg.trim());
            }
        }

        for (i, expr) in list.iter().enumerate() {
            let indented = if first_on_line { &base_indent } else { indent };
            self.check_and_cache_ignored(expr, list.get(i + 1));
            let formatted = self.format_source_exprs(slice::from_ref(expr), indented);
            let trimmed = t(&formatted);
            let expr_width = trimmed.len();

            if !first_on_line {
                let is_map_opening = trimmed.starts_with('{');
                let prev = &list[i - 1];
                let on_different_line_in_source = (is_comment(expr)
                    && prev.span().start_line != expr.span().start_line)
                    || (is_comment(prev) && prev.span().start_line != expr.span().start_line);

                if on_different_line_in_source
                    || (!is_map_opening
                        && (current_line_width + expr_width + 1 > self.settings.max_line_length))
                {
                    result.push('\n');
                    result.push_str(&base_indent);
                    current_line_width = base_indent.len() + self.indentation_str.len();
                    broken_up = true;
                } else {
                    result.push(' ');
                    current_line_width += 1;
                }
            }

            if broken_up {
                let formatted = self.format_source_exprs(slice::from_ref(expr), &base_indent);
                let trimmed = t(&formatted);
                result.push_str(trimmed);
            } else {
                result.push_str(trimmed);
            }

            current_line_width += expr_width;
            first_on_line = false;
            broken_up = false;
        }

        let break_lines = if !result.contains('\n') {
            false
        } else {
            let last_line = result
                .rfind('\n')
                .map(|pos| &result[pos + 1..])
                .unwrap_or(&result);
            let trimmed = last_line.trim();
            !(trimmed == ")" || trimmed == "}")
        };
        let mut out = String::with_capacity(result.len() + indent.len() + 4);
        out.push('(');
        out.push_str(&result);
        if break_lines {
            out.push('\n');
            out.push_str(indent);
        }
        out.push(')');
        out
    }
}

fn is_format_ignore(pse: &PreSymbolicExpression) -> bool {
    matches!(&pse.pre_expr, PreSymbolicExpressionType::Comment(text) if text.contains(FORMAT_IGNORE_SYNTAX))
}

fn is_comment(pse: &PreSymbolicExpression) -> bool {
    matches!(pse.pre_expr, PreSymbolicExpressionType::Comment(_))
}

fn without_comments_len(exprs: &[PreSymbolicExpression]) -> usize {
    exprs.iter().filter(|expr| !is_comment(expr)).count()
}
/// Length of the first line of a (possibly multi-line) string.
fn first_line_len(s: &str) -> usize {
    s.find('\n').unwrap_or(s.len())
}

/// After appending `s` to output at column `col`, return the new column.
/// If `s` contains newlines, returns length of last line; otherwise `col + s.len()`.
fn last_line_len(s: &str, col: usize) -> usize {
    if let Some(pos) = s.rfind('\n') {
        s.len() - pos - 1
    } else {
        col + s.len()
    }
}

fn is_define_fn(atom_name: &str) -> bool {
    DefineFunctions::lookup_by_name(atom_name).is_some()
}

fn is_same_line(expr1: &PreSymbolicExpression, expr2: &PreSymbolicExpression) -> bool {
    expr1.span().start_line == expr2.span().start_line
}

// convenience function to return a possible comment PSE from a peekable iterator
fn get_trailing_comment<'a, I>(
    expr: &'a PreSymbolicExpression,
    iter: &mut Peekable<I>,
) -> Option<&'a PreSymbolicExpression>
where
    I: Iterator<Item = &'a PreSymbolicExpression>,
{
    // cloned() here because of the second mutable borrow on iter.next()
    match iter.peek().cloned() {
        Some(next) => {
            if is_comment(next) && is_same_line(expr, next) {
                iter.next();
                Some(next)
            } else {
                None
            }
        }
        _ => None,
    }
}

fn contains_comments(exprs: &[PreSymbolicExpression]) -> bool {
    exprs.iter().any(is_comment)
}

/// Push `count` spaces directly into `acc`, avoiding a temporary String allocation.
fn push_spaces(acc: &mut String, count: u32) {
    for _ in 0..count {
        acc.push(' ');
    }
}

fn comment_piece(text: &str, pse: &PreSymbolicExpression) -> String {
    let (comment_part, rest) = text
        .find(|c| c != ';')
        .map_or((text, ""), |idx| (&text[..idx], &text[idx..]));
    let comment_length = text.len() as u32;
    let space_count = pse.span().end_column - comment_length - pse.span().start_column - 1; // 1 to account for span starting at 1 instead of 0
    if space_count > 0 {
        let mut result =
            String::with_capacity(2 + comment_part.len() + space_count as usize + rest.len());
        result.push_str(";;");
        result.push_str(comment_part);
        push_spaces(&mut result, space_count);
        result.push_str(rest);
        result
    } else {
        // remove the spaces if the comment has its own
        let spaces = if rest.starts_with(' ') { "" } else { " " };
        format!(";;{comment_part}{spaces}{rest}")
    }
}

/// Returns true if there's at least one blank line between the previous expression's
/// end line and the current expression's start line.
fn has_blank_line(prev_end_line: Option<u32>, curr_start_line: u32) -> bool {
    prev_end_line.is_some_and(|prev_end| curr_start_line > prev_end + 1)
}

#[cfg(test)]
mod tests_formatter {
    #[allow(unused_imports)]
    use std::assert_eq;

    use clarinet_defaults::DEFAULT_EPOCH;
    use clarity::vm::ast::stack_depth_checker::StackDepthLimits;
    use indoc::indoc;

    use super::{ClarityFormatter, Settings};
    use crate::formatter::{Aggregator, Indentation};
    #[macro_export]
    macro_rules! assert_eq {
        ($($arg:tt)*) => {
            pretty_assertions::assert_eq!($($arg)*)
        }
    }

    fn format_with_default(source: &str) -> String {
        let formatter = ClarityFormatter::new(Settings::default());
        formatter.format_section(source, None).unwrap()
    }

    fn format_with(source: &str, settings: Settings) -> String {
        let formatter = ClarityFormatter::new(settings);
        formatter.format_section(source, None).unwrap()
    }

    #[test]
    fn test_simplest_formatter() {
        let result = format_with_default(&String::from("(  ok    true )"));
        assert_eq!(result, "(ok true)");
    }

    #[test]
    fn test_fungible_token() {
        let src = "(define-fungible-token hello)\n";
        let result = format_with_default(&String::from(src));
        assert_eq!(result, src);

        let src = "(define-fungible-token hello u100)\n";
        let result = format_with_default(&String::from(src));
        assert_eq!(result, src);
    }

    #[test]
    fn test_manual_tuple() {
        let result = format_with_default(&String::from("(tuple (n1 1))"));
        assert_eq!(result, "{ n1: 1 }");
        let result = format_with_default(&String::from("(tuple (n1 1) (n2 2))"));
        assert_eq!(result, "{\n  n1: 1,\n  n2: 2,\n}");
    }

    #[test]
    fn test_function_formatter() {
        let result = format_with_default(&String::from("(define-private (my-func) (ok true))"));
        assert_eq!(result, "(define-private (my-func)\n  (ok true)\n)\n");
    }

    #[test]
    fn intact_comment_spacing() {
        let src = indoc!(
            r#"
          ;; (define-read-only (has-access)
          ;;   (begin
          ;;     (ok true)
          ;;   )
          ;; )"#
        );
        let result = format_with_default(&String::from(src));
        assert_eq!(src, result);

        let src = "(ok true) ;;     spaced\n";
        let result = format_with_default(&String::from(src));
        assert_eq!(src, result);
    }
    #[test]
    fn test_multi_function() {
        let src = "(define-public (my-func) (ok true))\n(define-public (my-func2) (ok true))";
        let result = format_with_default(&String::from(src));
        let expected = indoc!(
            r#"
            (define-public (my-func)
              (ok true)
            )
            (define-public (my-func2)
              (ok true)
            )
            "#
        );
        assert_eq!(expected, result);
    }
    #[test]
    fn test_function_single_arg() {
        let src = "(define-public (my-func (amount uint)) (ok true))";
        let result = format_with_default(&String::from(src));
        assert_eq!(
            result,
            "(define-public (my-func (amount uint))\n  (ok true)\n)\n"
        );
        let src = "(define-public (my-func (amount uint)) (ok true))";
        let result = format_with_default(&String::from(src));
        assert_eq!(
            result,
            "(define-public (my-func (amount uint))\n  (ok true)\n)\n"
        );
    }
    #[test]
    fn test_function_args_multiline() {
        let src = "(define-public (my-func (amount uint) (sender principal)) (ok true))";
        let result = format_with_default(&String::from(src));
        assert_eq!(
            result,
            "(define-public (my-func\n    (amount uint)\n    (sender principal)\n  )\n  (ok true)\n)\n"
        );
    }
    #[test]
    fn test_preserve_newlines_inner_function() {
        let src = indoc!(
            r#"
            (define-public (increment)
              (begin
                (try! (stx-transfer? (var-get cost) tx-sender (var-get contract-owner)))

                (ok (var-set count (+ (var-get count) u1)))
              )
            )
            "#
        );
        let result = format_with_default(&String::from(src));
        assert_eq!(result, src);
    }
    #[test]
    fn test_pre_comments_included() {
        let src = ";; this is a pre comment\n;; multi\n(ok true)";
        let result = format_with_default(&String::from(src));
        assert_eq!(src, result);
    }

    #[test]
    fn test_inline_comments_included() {
        let src = "(ok true) ;; this is an inline comment\n";
        let result = format_with_default(&String::from(src));
        assert_eq!(src, result);
    }

    #[test]
    fn test_booleans() {
        let src = "(or true false)";
        let result = format_with_default(&String::from(src));
        assert_eq!(src, result);
        let src = "(or true (is-eq 1 2) (is-eq 1 1))";
        let result = format_with_default(&String::from(src));
        let expected = "(or\n  true\n  (is-eq 1 2)\n  (is-eq 1 1)\n)";
        assert_eq!(expected, result);
    }

    #[test]
    fn test_booleans_with_comments() {
        let src = indoc!(
            r#"
            (or
              true
              ;; pre comment
              (is-eq 1 2) ;; comment
              (is-eq 1 1) ;; b
            )"#
        );
        let result = format_with_default(&String::from(src));
        assert_eq!(src, result);

        let src = indoc!(
            r#"
            (asserts!
              (or
                (is-eq merkle-root txid) ;; true, if the transaction is the only transaction
                (try! (verify-merkle-proof reversed-txid (reverse-buff32 merkle-root) proof))
              )
              (err ERR-INVALID-MERKLE-PROOF)
            )"#
        );
        let result = format_with_default(&String::from(src));
        assert_eq!(src, result);
    }

    #[test]
    fn long_line_unwrapping() {
        let src = "(try! (unwrap! (complete-deposit-wrapper (get txid deposit) (get vout-index deposit) (get amount deposit) (get recipient deposit) (get burn-hash deposit) (get burn-height deposit) (get sweep-txid deposit)) (err (+ ERR_DEPOSIT_INDEX_PREFIX (+ u10 index)))))";
        let result = format_with_default(&String::from(src));
        let expected = indoc!(
            r#"
            (try! (unwrap!
              (complete-deposit-wrapper (get txid deposit) (get vout-index deposit)
                (get amount deposit) (get recipient deposit) (get burn-hash deposit)
                (get burn-height deposit) (get sweep-txid deposit)
              )
              (err (+ ERR_DEPOSIT_INDEX_PREFIX (+ u10 index)))
            ))"#
        );
        assert_eq!(expected, result);

        // non-max-length sanity case
        let src = "(try! (unwrap! (something) (err SOME_ERR)))";
        let result = format_with_default(&String::from(src));
        assert_eq!(src, result);
    }

    #[test]
    fn test_map() {
        let src = "(define-map a uint {n1: (buff 20)})";
        let result = format_with_default(&String::from(src));
        assert_eq!(result, "(define-map a\n  uint\n  { n1: (buff 20) }\n)\n");
        let src = "(define-map something { name: (buff 48), a: uint } uint)\n";
        let result = format_with_default(&String::from(src));
        let expected = indoc!(
            r#"
            (define-map something
              {
                name: (buff 48),
                a: uint,
              }
              uint
            )
            "#
        );
        assert_eq!(result, expected);
    }

    #[test]
    fn test_let() {
        let src = "(let ((a 1) (b 2)) (+ a b))";
        let result = format_with_default(&String::from(src));
        let expected = indoc!(
            r#"
            (let (
                (a 1)
                (b 2)
              )
              (+ a b)
            )"#
        );
        assert_eq!(expected, result);
    }
    #[test]
    fn test_single_let() {
        let src = indoc!(
            r#"
            (let ((current-count (var-get count)))
              (asserts! (> current-count u0) ERR_COUNT_MUST_BE_POSITIVE)
              (ok (var-set count (- current-count u1)))
            )"#
        );
        let result = format_with_default(&String::from(src));
        assert_eq!(src, result);
    }

    #[test]
    fn test_option_match() {
        let src = "(match opt value (ok (handle-new-value value)) (ok 1))";
        let result = format_with_default(&String::from(src));
        // "(match opt\n
        let expected = indoc!(
            r#"
            (match opt
              value (ok (handle-new-value value))
              (ok 1)
            )"#
        );
        assert_eq!(result, expected);
    }

    #[test]
    fn test_response_match() {
        let src = "(match x value (ok (+ to-add value)) err-value (err err-value))";
        let result = format_with_default(&String::from(src));
        let expected = indoc!(
            r#"
            (match x
              value (ok (+ to-add value))
              err-value (err err-value)
            )"#
        );
        assert_eq!(result, expected);
    }

    #[test]
    fn test_comment_spacing() {
        let src = indoc!(
            r#"
            ;;comment
            ;;    comment
            ;;;comment"#
        );
        let result = format_with_default(&String::from(src));
        let expected = indoc!(
            r#"
            ;; comment
            ;;    comment
            ;;; comment"#
        );
        assert_eq!(expected, result);
    }
    #[test]
    fn test_commented_match() {
        let src = indoc!(
            r#"
            (match x
              ;; comment
              value
              ;; comment
              (ok (+ to-add value))
              (ok true)
            )"#
        );
        let result = format_with_default(&String::from(src));
        assert_eq!(src, result);
    }
    #[test]
    fn test_key_value_sugar() {
        let src = "{name: (buff 48)}";
        let result = format_with_default(&String::from(src));
        assert_eq!(result, "{ name: (buff 48) }");
        let src = "{ name: (buff 48), a: uint }";
        let result = format_with_default(&String::from(src));
        assert_eq!(result, "{\n  name: (buff 48),\n  a: uint,\n}");
    }

    #[test]
    fn map_in_map() {
        let src = "(ok { a: b, ctx: { a: b, c: d }})";
        let result = format_with_default(src);
        let expected = indoc!(
            r#"
            (ok {
              a: b,
              ctx: {
                a: b,
                c: d,
              },
            })"#
        );
        assert_eq!(expected, result);
        let src = indoc!(
            r#"
            (ok {
              varslice: (unwrap! (slice? txbuff slice-start target-index) (err ERR-OUT-OF-BOUNDS)),
              ctx: {
                txbuff: tx,
                index: (+ u1 ptr),
              },
            })"#
        );
        let result = format_with_default(src);
        assert_eq!(src, result);
    }

    #[test]
    fn old_tuple() {
        let src = indoc!(
            r#"
            (tuple
              (a uint)
              (b uint) ;; comment
              (c bool)
            )"#
        );
        let result = format_with_default(src);
        let expected = indoc!(
            r#"
            {
              a: uint,
              b: uint, ;; comment
              c: bool,
            }"#
        );
        assert_eq!(result, expected);
    }

    #[test]
    fn top_level_exprs() {
        let src = indoc!(
            r#"
            (let ((x (+ u1 u1)))
              (map-insert ns x true)
            )
            (define-public (get-value)
              (ok (map-get? ns u2))
            )
            "#
        );
        let result = format_with_default(src);
        assert_eq!(result, src);

        let src = indoc!(
            r#"
            (print {
              notification: "format-me",
              payload: { message: "Hello, World!" },
            })
            (var-set test-var 1)
            (var-set test-var 2)"#
        );
        let result = format_with_default(src);
        assert_eq!(result, src);
    }
    #[test]
    fn test_indentation_levels() {
        let src = "(begin (let ((a 1) (b 2)) (ok true)))";
        let result = format_with_default(&String::from(src));
        let expected = indoc!(
            r#"
            (begin
              (let (
                  (a 1)
                  (b 2)
                )
                (ok true)
              )
            )"#
        );
        assert_eq!(result, expected);
    }
    #[test]
    fn test_let_comments() {
        let src = indoc!(
            r#"
            (begin
              (let (
                  (a 1) ;; something
                  (b 2) ;; comment
                )
                (ok true)
              )
            )"#
        );
        let result = format_with_default(&String::from(src));
        assert_eq!(src, result);
    }

    #[test]
    fn test_block_comments() {
        let src = ";;\n;; abc\n;;";
        let result = format_with_default(src);
        assert_eq!(src, result)
    }

    #[test]
    fn test_key_value_sugar_comment_midrecord() {
        let src = indoc!(
            r#"
            {
              name: (buff 48),
              ;;; comment
              owner: send-to, ;; trailing
            }"#
        );
        let result = format_with_default(&String::from(src));
        assert_eq!(src, result);
    }

    #[test]
    fn test_basic_slice() {
        let src = "(slice? (1 2 3 4 5) u5 u9)";
        let result = format_with_default(&String::from(src));
        assert_eq!(src, result);
    }
    #[test]
    fn test_constant() {
        let src = "(define-constant minter 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM.minter)\n";
        let result = format_with_default(&String::from(src));
        assert_eq!(result, src);

        let src = "(define-constant a u1) ;;; comment\n";
        let result = format_with_default(&String::from(src));
        assert_eq!(result, src);
    }

    #[test]
    fn test_begin_never_one_line() {
        let src = "(begin (ok true))";
        let result = format_with_default(&String::from(src));
        assert_eq!(result, "(begin\n  (ok true)\n)");
    }

    #[test]
    fn test_begin() {
        let src = "(begin (+ 1 1) ;; a\n (ok true))";
        let result = format_with_default(&String::from(src));
        assert_eq!(result, "(begin\n  (+ 1 1) ;; a\n  (ok true)\n)");
    }

    #[test]
    fn test_custom_tab_setting() {
        let src = "(begin (ok true))";
        let result = format_with(&String::from(src), Settings::new(Indentation::Space(4), 80));
        assert_eq!(result, "(begin\n    (ok true)\n)");
    }

    #[test]
    fn test_if() {
        let src = "(if (<= amount max-supply) (list ) (something amount))";
        let result = format_with_default(&String::from(src));
        let expected = "(if (<= amount max-supply)\n  (list)\n  (something amount)\n)";
        assert_eq!(result, expected);
    }
    #[test]
    fn test_ignore_formatting() {
        let src = ";; @format-ignore\n(    begin ( ok true))";
        let result = format_with(&String::from(src), Settings::new(Indentation::Space(4), 80));
        assert_eq!(src, result);

        let src = ";; @format-ignore\n(list\n  u64\n  u64 u64\n)";
        let result = format_with(&String::from(src), Settings::new(Indentation::Space(4), 80));
        assert_eq!(src, result);
    }

    #[test]
    fn test_index_of() {
        let src = "(index-of? (contract-call? .pool borroweable) asset)";
        let result = format_with_default(&String::from(src));
        assert_eq!(src, result);
    }
    #[test]
    fn test_traits() {
        let src = "(use-trait token-a-trait 'SPAXYA5XS51713FDTQ8H94EJ4V579CXMTRNBZKSF.token-a.token-trait)\n";
        let result = format_with(&String::from(src), Settings::new(Indentation::Space(4), 80));
        assert_eq!(src, result);

        let src = "(impl-trait 'SPAXYA5XS51713FDTQ8H94EJ4V579CXMTRNBZKSF.token-a.token-trait)\n";
        let result = format_with(&String::from(src), Settings::new(Indentation::Space(4), 80));
        assert_eq!(src, result);
    }
    #[test]
    fn test_detailed_traits() {
        let src = indoc!(
            r#"
            (define-public (parse-and-verify-vaa
                (core-contract <core-trait>)
                (vaa-bytes (buff 8192))
              )
              (begin
                (try! (check-active-wormhole-core-contract core-contract))
                (contract-call? core-contract parse-and-verify-vaa vaa-bytes)
              )
            )
            "#
        );
        let result = format_with_default(&String::from(src));
        assert_eq!(src, result);
    }
    #[test]
    fn test_as_contract() {
        let src = "(as-contract (contract-call? .tokens mint! u19))";
        let result = format_with(&String::from(src), Settings::new(Indentation::Space(4), 80));
        assert_eq!(src, result);
    }

    #[test]
    fn too_many_newlines() {
        let src = indoc!(
            r#"
            (ok (at-block
              (unwrap! (get-stacks-block-info? id-header-hash block) ERR_BLOCK_NOT_FOUND)
              (var-get count)
            ))"#
        );
        let result = format_with_default(&String::from(src));
        assert_eq!(src, result);
    }

    // this looks redundant, but a regression kept happening with ill-spaced
    // inner expressions. Likely this is a product of poorly handled nesting
    // logic
    #[test]
    fn spacing_for_inner_expr() {
        let src = "(something (- (/ b o) (/ (- balance-sender a) o)))";
        let result = format_with_default(src);
        assert_eq!(src, result)
    }
    #[test]
    fn closing_if_parens() {
        let src = "(something (if (true) (list) (list 1 2 3)))";
        let result = format_with_default(src);
        let expected = indoc!(
            r#"
            (something (if (true)
              (list)
              (list 1 2 3)
            ))"#
        );
        assert_eq!(expected, result);
    }
    #[test]
    fn ok_map() {
        let src = "(ok { a: b, c: d })";
        let result = format_with_default(src);
        let expected = indoc!(
            r#"
            (ok {
              a: b,
              c: d,
            })"#
        );
        assert_eq!(expected, result);
    }

    #[test]
    fn if_let_if() {
        let src = indoc!(
            r#"
            (if (true)
              (let ((a (if (true)
                  (list)
                  (list)
                )))
                (list)
              )
              (list)
            )"#
        );
        let result = format_with_default(src);
        assert_eq!(src, result);
    }

    #[test]
    fn weird_nesting() {
        let src = indoc!(
            r#"
            (merge name-props {
              something: u1,
              ;; comment
              renewal-height:
                ;; If still within lifetime, extend from current renewal height; otherwise, use new renewal height
                (if (< burn-block-height
                    (unwrap-panic (get-renewal-height (unwrap-panic (get-id-from-bns name namespace))))
                  )
                  (+
                    (unwrap-panic (get-renewal-height (unwrap-panic (get-id-from-bns name namespace))))
                    lifetime
                  )
                  new-renewal-height
                ),
            })"#
        );
        let result = format_with_default(src);
        assert_eq!(src, result);
    }

    #[test]
    fn weird_nesting_single_value() {
        let src = indoc!(
            r#"
            (begin
              (map-set name-properties {
                name: name,
                namespace: namespace,
              }
                (merge name-props {
                  renewal-height:
                    ;; If still within lifetime, extend from current renewal height; otherwise, use new renewal height
                    (if (< burn-block-height
                        (unwrap-panic (get-renewal-height (unwrap-panic (get-id-from-bns name namespace))))
                      )
                      (+
                        (unwrap-panic (get-renewal-height (unwrap-panic (get-id-from-bns name namespace))))
                        lifetime
                      )
                      new-renewal-height
                    ),
                })
              )
            )"#
        );
        let result = format_with_default(src);
        assert_eq!(src, result);
    }
    #[test]
    fn define_data_var_test() {
        let src = "(define-data-var my-data-var principal tx-sender)\n";
        let result = format_with_default(src);
        assert_eq!(src, result);
    }

    #[test]
    fn define_multiline_list() {
        let src = r#"  (
    (optional <sip-010>) ;; token
    uint                 ;; amount
    principal            ;; with
    uint                 ;; nonce
  )"#;
        let result = format_with_default(src);
        assert_eq!(src, result);
    }

    #[test]
    fn inner_list_with_maps() {
        let src = indoc!(
            r#"
            (list
              u1
              {
                extension: .ccd001-direct-execute,
                enabled: true,
              }
              ;; {extension: .ccd008-city-activation, enabled: true}
              {
                extension: .ccd009-auth-v2-adapter,
                enabled: true,
              }
            )"#
        );
        let result = format_with_default(src);
        assert_eq!(src, result);
    }

    #[test]
    fn valid_comment_breaking() {
        let src = indoc!(
            r#"
            (var-set voteStart block-height) ;; vote tracking
            (define-data-var yesVotes uint u0)
            "#
        );
        let result = format_with_default(src);
        assert_eq!(src, result);
    }
    #[test]
    fn significant_newline_preserving_inner() {
        let src = indoc!(
            r#"
            ;; comment

            ;; another
            ;; more


            ;; after 2 spaces, now it's 1"#
        );
        let result = format_with_default(src);
        let expected = indoc!(
            r#"
            ;; comment

            ;; another
            ;; more

            ;; after 2 spaces, now it's 1"#
        );
        assert_eq!(expected, result);
    }
    #[test]
    fn significant_newline_preserving() {
        let src = indoc!(
            r#"
            ;; comment

            ;; another
            ;; more


            ;; after 2 spaces, now it's 1"#
        );
        let result = format_with_default(src);
        let expected = indoc!(
            r#"
            ;; comment

            ;; another
            ;; more

            ;; after 2 spaces, now it's 1"#
        );
        assert_eq!(expected, result);
    }
    #[test]
    fn define_trait_test() {
        let src = indoc!(
            r#"
            (define-trait token-trait (
              (transfer?
                (principal principal uint) ;; principal
                ;; pre comment
                (response uint uint)       ;; comment
              )
              (get-balance
                (principal)
                (response uint uint)
              )
            ))
            "#
        );
        let result = format_with_default(src);
        assert_eq!(src, result);
    }
    #[test]
    fn unwrap_wrapped_lines() {
        let src = indoc!(
            r#"
            (new-available-ids (if (is-eq no-to-treasury u0)
              (var-get available-ids)
              (unwrap-panic (as-max-len? (concat (var-get available-ids) ids-to-treasury) u10000))
            ))"#
        );
        let result = format_with_default(src);
        assert_eq!(src, result);
    }

    #[test]
    fn wrapped_list() {
        let src = indoc!(
            r#"{ buckets: (list p-func-b1 p-func-b2 p-func-b3 p-func-b4 p-func-b5 p-func-b6 p-func-b7 p-func-b8 p-func-b9 p-func-b10 p-func-b11 p-func-b12 p-func-b13 p-func-b14 p-func-b15 p-func-b16), something: u1 }"#
        );
        let expected = indoc!(
            r#"
            {
              buckets: (list
                p-func-b1 p-func-b2 p-func-b3 p-func-b4 p-func-b5 p-func-b6
                p-func-b7 p-func-b8 p-func-b9 p-func-b10 p-func-b11 p-func-b12
                p-func-b13 p-func-b14 p-func-b15 p-func-b16
              ),
              something: u1,
            }"#
        );
        let result = format_with_default(src);
        assert_eq!(expected, result);
    }

    #[test]
    fn list_spacing_simple_atoms_should_collapse_to_single_line() {
        // multiline (list ...) with simple atom values should collapses to single line if it fits

        let src = indoc!(
            r#"
            (fold sorted-fold-step
              (list
                u0               u1               u2               u3
                u4               u5               u6               u7
                u8               u9
                u10               u11               u12               u13               u14
                u15               u16               u17               u18
                u19
              )
              {
                sorted: true,
              }
            )"#
        );
        let expected = indoc!(
            r#"
            (fold sorted-fold-step
              (list u0 u1 u2 u3 u4 u5 u6 u7 u8 u9 u10 u11 u12 u13 u14 u15 u16 u17 u18 u19) { sorted: true }
            )"#
        );
        let result = format_with_default(src);
        assert_eq!(expected, result);
    }

    #[test]
    fn format_ast_without_source() {
        let src = "(define-private (noop) (begin (+ 1 2) (ok true)))";
        let ast =
            clarity::vm::ast::parser::v2::parse(src, StackDepthLimits::for_epoch(DEFAULT_EPOCH))
                .unwrap();
        let formatter = ClarityFormatter::new(Settings::default());
        let expected = format_with_default(src);
        let result = formatter.format_ast(&ast);
        assert_eq!(result, expected);
    }

    #[test]
    fn format_ast_without_source_handle_indentation() {
        let src = "  (begin (+ 1 2) (ok true))";
        let ast =
            clarity::vm::ast::parser::v2::parse(src, StackDepthLimits::for_epoch(DEFAULT_EPOCH))
                .unwrap();
        let expected = format_with_default(src);
        let formatter = ClarityFormatter::new(Settings::default());
        let result = formatter.format_ast(&ast);
        assert_eq!(result, expected);
    }

    #[test]
    fn retain_comment_newlines() {
        let src = indoc!(
            r#"
            (senderBalance (unwrap!
              (at-block proposalBlockHash
                ;; /g/.aibtc-faktory/dao_contract_token
                (contract-call? .aibtc-faktory get-balance contract-caller)
              )
              ERR_FETCHING_TOKEN_DATA
            ))"#
        );
        let result = format_with_default(src);
        assert_eq!(src, result);
    }

    #[test]
    fn test_format_ignore_multiple_expressions() {
        let src = ";; @format-ignore\n(+ u1 u1)\n(+ u1 u1)";
        let result = format_with_default(src);

        assert_eq!(src, result);

        let src = ";; @format-ignore\n(+ u1 u1)\n;; @format-ignore\n(+ u1 u1)";
        let result = format_with_default(src);

        assert_eq!(src, result);
    }

    #[test]
    fn test_match_lining() {
        let src = indoc!(
            r#"
        (match prior
          ok-value result
          err-value (err err-value)
        )"#
        );
        let result = format_with_default(src);

        assert_eq!(src, result);
    }

    #[test]
    fn test_empty_trait() {
        let src = "(define-trait my-trait ())\n";
        let result = format_with_default(src);
        assert_eq!(src, result);
    }

    #[test]
    fn test_empty_trait_with_comment() {
        let src = "(define-trait my-trait ()) ;; empty\n";
        let result = format_with_default(src);
        assert_eq!(src, result);
    }

    #[test]
    fn test_empty_trait_with_multiline_comment() {
        let src = indoc!(
            r#"
            (define-trait my-trait
              () ;; empty
            )
            "#
        );
        let result = format_with_default(src);
        assert_eq!(src, result);
    }

    #[test]
    fn test_restrict_assets_single_arg() {
        let src = indoc!(
            r#"
            (restrict-assets? asset-owner ((with-stx u10000))
              (+ u1 u2)
            )"#
        );
        let result = format_with_default(src);
        assert_eq!(src, result);
    }
    #[test]
    fn test_restrict_assets_multi_arg() {
        let src = indoc!(
            r#"
            (restrict-assets? asset-owner (
                (with-stx u10000)
                (with-ft (contract-of token-trait) "stackaroo" u50)
              )
              (try! (contract-call? token-trait transfer u100 tx-sender
                'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM none
              ))
              (try! (stx-transfer? u1000000 tx-sender 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM))
              (print u1)
            )"#
        );
        let result = format_with_default(src);
        assert_eq!(src, result);
    }

    #[test]
    fn test_quote_escaping() {
        let src = "(ok \"'\")";
        let result = format_with_default(src);
        assert_eq!(src, result);

        let src = "(ok u\"hello\")";
        let result = format_with_default(src);
        assert_eq!(src, result);

        let src = "(ok u\"\\u{6e05}\\u{6670}\")";
        let result = format_with_default(src);
        assert_eq!(result, src);
    }

    #[test]
    fn test_list_type_signature() {
        fn assert_list_type_signature(src: &str, expected: bool) {
            let settings = Settings::default();
            let exprs = clarity::vm::ast::parser::v2::parse(
                src,
                StackDepthLimits::for_epoch(DEFAULT_EPOCH),
            )
            .unwrap();
            let mut aggregator = Aggregator::new(&settings, &exprs, Some(src));
            let list_exprs = exprs[0].match_list().unwrap();
            assert_eq!(aggregator.is_list_type_signature(list_exprs), expected);
        }
        assert_list_type_signature("(list 1 2 3)", false);

        assert_list_type_signature("(list 1)", false);

        assert_list_type_signature("(list 12 (buff 24))", true);
    }

    #[test]
    fn test_format_ignore_parent() {
        let src = indoc!(
            r#"
            (define-public (hello)
              (begin
                ;; @format-ignore
                (ok   (concat "world"
                  "something"
                ))
              )
            )
            "#
        );
        let result = format_with_default(src);
        assert_eq!(src, result);

        // test with another expression (if) to make sure it works for both
        let src = indoc!(
            r#"
            (define-public (hello)
              (if (true)
                ;; @format-ignore
                (ok   (concat "world"
                  "something"
                ))
              )
            )
            "#
        );
        let result = format_with_default(src);
        assert_eq!(src, result);
    }

    #[test]
    fn test_comment_args() {
        // Comments must stay on their original lines (span.start_line preserved)
        let src = indoc!(
            r#"
            (define-public (some-fn
                (a uint)
                ;; (b uint)
              )
              (ok true)
            )
            "#
        );
        let result = format_with_default(src);
        assert_eq!(src, result);

        let src = indoc!(
            r#"
            (define-public (some-fn
                (a uint)
                ;; documentation comment for b
                (b uint)
              )
              (ok true)
            )
            "#
        );
        let result = format_with_default(src);
        assert_eq!(src, result);
    }

    #[test]
    fn test_inline_comment_try() {
        let src = indoc!(
            r#"
    (try! (contract-call? 'STV9K21TBFAK4KNRJXF5DFP8N7W46G4V9RJ5XDY2.sbtc-token
      ;; /g/.aibtc-pre-faktory/dao_contract_token_prelaunch
      transfer pre-fee tx-sender .aibtc-pre-faktory none
    ))"#
        );
        let result = format_with_default(src);
        assert_eq!(src, result);
    }
}
