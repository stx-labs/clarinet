pub mod doc;
pub mod helpers;
pub mod ignored;
mod sequence;

use std::collections::HashMap;
use std::iter::Peekable;
use std::ops::Deref;
use std::{fmt, slice};

use clarinet_defaults::DEFAULT_EPOCH;
use clarity::types::StacksEpochId;
use clarity::vm::ast::stack_depth_checker::StackDepthLimits;
use clarity::vm::functions::define::DefineFunctions;
use clarity::vm::functions::NativeFunctions;
use clarity::vm::representations::{PreSymbolicExpression, PreSymbolicExpressionType, Span};
use doc::{Doc, Printer};
use helpers::t;
use ignored::SourceLines;
use sequence::Sequence;

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

/// The indentation string at one nesting level.
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
    source_lines: Option<SourceLines<'a>>,
    indentation_str: String,

    // Keyed by slice pointer, length and indent
    cache: HashMap<(usize, usize, String), String>,
    ignored_exprs: HashMap<Span, String>,
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
            source_lines: source.map(SourceLines::new),
            indentation_str,
            cache: HashMap::new(),
            ignored_exprs: HashMap::new(),
        }
    }

    /// Render a Doc tree to a String using the current settings.
    fn render_doc(&self, doc: &Doc, indent: &Indent) -> String {
        Printer::new(&self.indentation_str, self.settings.max_line_length).render(doc, indent)
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
    fn cache_ignored_expression(&mut self, next_expr: &PreSymbolicExpression) {
        let Some(source_lines) = &self.source_lines else {
            return;
        };
        // Only the expression: callers add its trailing comment themselves
        let source = source_lines.extract_expr_source(next_expr);
        self.ignored_exprs.insert(next_expr.span().clone(), source);
    }

    /// Handle a `@format-ignore` comment: append the comment and the following
    /// expression to `result` as raw source text.
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
        let next_list = iter
            .peek()
            .copied()
            .filter(|next| next.match_list().is_some());
        let (Some(source_lines), Some(next_expr)) = (&self.source_lines, next_list) else {
            // Without source, or with no list to ignore, this is a plain comment
            let comment = match &self.source_lines {
                Some(source_lines) => source_lines.extract_expr_source(comment_expr),
                None => self.display_pse(comment_expr, indent),
            };
            result.push_str(&comment);
            if iter.peek().is_some() {
                result.push('\n');
            }
            return comment_expr.span().end_line;
        };
        iter.next();

        // Keep the directive, the expression and its trailing comment verbatim
        let last = get_trailing_comment(next_expr, iter).unwrap_or(next_expr);
        result.push_str(&source_lines.extract_source_range(&Span {
            start_line: comment_expr.span().start_line,
            start_column: comment_expr.span().start_column,
            ..last.span().clone()
        }));
        if iter.peek().is_some() {
            result.push('\n');
        }

        last.span().end_line
    }

    /// If `expr` is a `@format-ignore` comment and `next_expr` is a list,
    /// cache the next expression so it's returned verbatim by `format_source_exprs`.
    fn check_and_cache_ignored(
        &mut self,
        expr: &PreSymbolicExpression,
        next_expr: Option<&PreSymbolicExpression>,
    ) {
        if let Some(next) =
            next_expr.filter(|next| is_format_ignore(expr) && next.match_list().is_some())
        {
            self.cache_ignored_expression(next);
        }
    }

    fn format_source_exprs(
        &mut self,
        expressions: &'a [PreSymbolicExpression],
        indent: &Indent,
    ) -> String {
        // if this expression was marked as ignored, return the cached source
        if let [expr] = expressions {
            if let Some(source) = self.ignored_exprs.get(expr.span()) {
                return source.clone();
            }
        }

        let key = (
            expressions.as_ptr() as usize,
            expressions.len(),
            indent.to_string(),
        );

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
                        self.format_list_call(expr, atom_name, list, indent),
                        is_define_fn(atom_name),
                    )
                } else {
                    // list without a leading atom
                    (self.display_pse(expr, indent), false)
                }
            } else {
                (self.display_pse(expr, indent), false)
            };

            // Top-level lists each start on their own line
            if (trailing_comment.is_some() || expr.match_list().is_some())
                && indent.is_empty()
                && result.ends_with(')')
            {
                result.push('\n');
            }

            if let Some(comment) = trailing_comment {
                // A define ends with a newline, and the comment goes before it
                result.push_str(formatted.strip_suffix('\n').unwrap_or(&formatted));
                result.push(' ');
                result.push_str(&self.display_pse(comment, indent));
                result.push('\n');
            } else if is_define {
                // Define-level expressions always get a trailing newline
                result.push_str(&formatted);
                if !formatted.ends_with('\n') {
                    result.push('\n');
                }
            } else if expr.match_list().is_some() {
                result.push_str(&formatted);
                if let Some(next) = iter.peek() {
                    if !is_same_line(expr, next) {
                        result.push('\n');
                    } else {
                        result.push(' ');
                    }
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
        expr: &'a PreSymbolicExpression,
        atom_name: &str,
        list: &'a [PreSymbolicExpression],
        indent: &Indent,
    ) -> String {
        if let Some(native) = NativeFunctions::lookup_by_name(atom_name) {
            match native {
                NativeFunctions::Let => self.format_let(list, indent),
                NativeFunctions::Begin => self.format_begin(list, indent),
                NativeFunctions::Match => self.format_match(list, indent),
                NativeFunctions::TupleCons => {
                    if let Some(formatted) = self.format_key_value(&list[1..], indent) {
                        formatted
                    } else {
                        self.preserve_expr(expr, indent)
                    }
                }
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
                DefineFunctions::Trait => self.define_trait(expr, list, indent),
            }
        } else {
            self.format_inner_content(list, indent)
        }
    }

    fn define_trait(
        &mut self,
        expr: &'a PreSymbolicExpression,
        exprs: &'a [PreSymbolicExpression],
        indent: &Indent,
    ) -> String {
        let sequence = Sequence::from(exprs);
        let [keyword, name, methods] = sequence.items.as_slice() else {
            return self.preserve_expr(expr, indent);
        };
        let Some(method_list) = methods.expression.match_list() else {
            return self.preserve_expr(expr, indent);
        };
        let nested = indent.indented(&self.indentation_str);
        let empty_multiline = method_list.is_empty()
            && name.expression.span().end_line != methods.expression.span().start_line;

        let multiline_header =
            empty_multiline || !name.comments.is_empty() || !methods.comments.is_empty();
        let header_indent = if multiline_header { &nested } else { indent };
        let header = Doc::concat(vec![
            self.trait_item_prefix(
                name.comments,
                Some(keyword.expression),
                Doc::text(" "),
                header_indent,
            ),
            Doc::verbatim(self.display_pse(name.expression, header_indent)),
            self.trait_item_prefix(
                methods.comments,
                Some(name.expression),
                if empty_multiline {
                    Doc::Hardline
                } else {
                    Doc::text(" ")
                },
                header_indent,
            ),
            self.trait_list_doc(
                method_list,
                header_indent,
                Doc::Hardline,
                |this, _, method, nested| this.trait_method_doc(method, nested),
            ),
        ]);
        let mut parts = vec![
            Doc::text("(define-trait"),
            if multiline_header {
                Doc::indent(header)
            } else {
                header
            },
            Doc::indent(self.comment_gap_doc(
                sequence.end_comments,
                Some(methods.expression),
                &nested,
            )),
        ];
        if multiline_header || !sequence.end_comments.is_empty() {
            parts.push(Doc::Hardline);
        }
        parts.push(Doc::text(")"));
        self.render_doc(&Doc::concat(parts), indent)
    }

    /// Emit a gap without its final line break, so the caller controls the
    /// indentation of the next child or closing delimiter.
    fn comment_gap_doc(
        &mut self,
        comments: &'a [PreSymbolicExpression],
        mut previous: Option<&'a PreSymbolicExpression>,
        indent: &Indent,
    ) -> Doc {
        let mut parts = Vec::new();
        for comment in comments {
            if let Some(previous) =
                previous.filter(|previous| previous.span().end_line == comment.span().start_line)
            {
                parts.push(Doc::verbatim(
                    self.spaced_comment(previous, comment, indent),
                ));
            } else {
                parts.push(Doc::Hardline);
                parts.push(Doc::verbatim(self.display_pse(comment, indent)));
            }
            previous = Some(comment);
        }
        Doc::concat(parts)
    }

    fn trait_item_prefix(
        &mut self,
        comments: &'a [PreSymbolicExpression],
        previous: Option<&'a PreSymbolicExpression>,
        separator: Doc,
        indent: &Indent,
    ) -> Doc {
        if comments.is_empty() {
            return separator;
        }
        Doc::concat(vec![
            self.comment_gap_doc(comments, previous, indent),
            Doc::Hardline,
        ])
    }

    /// Lay out a trait list with one item per line, keeping every comment.
    fn trait_list_doc(
        &mut self,
        expressions: &'a [PreSymbolicExpression],
        indent: &Indent,
        first_separator: Doc,
        mut item_doc: impl FnMut(&mut Self, usize, &'a PreSymbolicExpression, &Indent) -> Doc,
    ) -> Doc {
        if expressions.is_empty() {
            return Doc::text("()");
        }
        let nested = indent.indented(&self.indentation_str);
        let sequence = Sequence::from(expressions);
        let mut first_separator = Some(first_separator);
        let mut body = Vec::new();
        let mut previous = None;
        for (index, item) in sequence.items.iter().enumerate() {
            let separator = first_separator.take().unwrap_or(Doc::Hardline);
            body.push(self.trait_item_prefix(item.comments, previous, separator, &nested));
            body.push(item_doc(self, index, item.expression, &nested));
            previous = Some(item.expression);
        }
        body.push(self.comment_gap_doc(sequence.end_comments, previous, &nested));
        block(Doc::text("("), body, ")")
    }

    fn trait_method_doc(&mut self, method: &'a PreSymbolicExpression, indent: &Indent) -> Doc {
        let Some(expressions) = method.match_list() else {
            return Doc::verbatim(self.preserve_syntax(method, indent));
        };
        self.trait_list_doc(
            expressions,
            indent,
            Doc::text(""),
            |this, index, item, nested| {
                // Only the name, parameters, and return type have defined formatting,
                // and none of them is a tuple. Anything else must not dispatch nested
                // lists as function calls.
                let defined =
                    index < 3 && !matches!(item.pre_expr, PreSymbolicExpressionType::Tuple(_));
                Doc::verbatim(if defined {
                    this.display_pse(item, nested)
                } else {
                    this.preserve_syntax(item, nested)
                })
            },
        )
    }

    fn preserve_expr(&mut self, expr: &'a PreSymbolicExpression, indent: &Indent) -> String {
        if let Some(source_lines) = &self.source_lines {
            source_lines.extract_expr_source(expr)
        } else {
            self.preserve_syntax(expr, indent)
        }
    }

    fn format_constant(&mut self, exprs: &'a [PreSymbolicExpression], indent: &Indent) -> String {
        let func_type = self.display_pse(exprs.first().unwrap(), &Indent::empty());
        if contains_comments(&exprs[1..]) {
            let nested = indent.indented(&self.indentation_str);
            let body = self.lines_doc(&exprs[1..], &nested, false);
            return self.render_doc(
                &block(Doc::text(format!("({func_type}")), body, ")"),
                indent,
            );
        }
        let mut parts: Vec<Doc> = vec![Doc::text(format!("({func_type} "))];
        for (i, expr) in exprs[1..].iter().enumerate() {
            if i > 0 {
                parts.push(Doc::text(" "));
            }
            parts.push(Doc::verbatim(
                self.format_source_exprs(slice::from_ref(expr), indent),
            ));
        }
        parts.push(Doc::text(")"));
        self.render_doc(&Doc::concat(parts), indent)
    }

    fn format_map(&mut self, exprs: &'a [PreSymbolicExpression], indent: &Indent) -> String {
        let func_type = self.display_pse(exprs.first().unwrap(), &Indent::empty());
        let nested = indent.indented(&self.indentation_str);
        let name = self.format_source_exprs(slice::from_ref(&exprs[1]), indent);

        let body = self.lines_doc(&exprs[2..], &nested, false);
        let open = Doc::concat(vec![
            Doc::text(format!("({func_type} ")),
            Doc::verbatim(name),
        ]);
        self.render_doc(&block(open, body, ")"), indent)
    }

    // *begin* never on one line
    fn format_begin(&mut self, exprs: &'a [PreSymbolicExpression], indent: &Indent) -> String {
        let nested = indent.indented(&self.indentation_str);
        let body = self.lines_doc(&exprs[1..], &nested, true);
        self.render_doc(&block(Doc::text("(begin"), body, ")"), indent)
    }

    // formats (and ..) and (or ...)
    // if given more than BOOLEAN_BREAK_LIMIT expressions it will break it onto new lines
    fn format_booleans(&mut self, exprs: &'a [PreSymbolicExpression], indent: &Indent) -> String {
        let func_type = self.display_pse(exprs.first().unwrap(), indent);
        let nested = indent.indented(&self.indentation_str);
        let break_up = without_comments_len(&exprs[1..]) > BOOLEAN_BREAK_LIMIT
            || contains_comments(&exprs[1..])
            || !self.booleans_fit_on_one_line(&func_type, &exprs[1..], indent);

        let doc = if break_up {
            let body = self.lines_doc(&exprs[1..], &nested, true);
            block(Doc::text(format!("({func_type}")), body, ")")
        } else {
            // No comments here: they force `break_up`
            let mut parts = vec![Doc::text(format!("({func_type}"))];
            for expr in &exprs[1..] {
                parts.push(Doc::text(" "));
                parts.push(Doc::verbatim(
                    self.format_source_exprs(slice::from_ref(expr), indent),
                ));
            }
            parts.push(Doc::text(")"));
            Doc::concat(parts)
        };
        self.render_doc(&doc, indent)
    }

    /// Whether `(and a b)` / `(or a b)` fits on one line with no multi-line argument.
    fn booleans_fit_on_one_line(
        &mut self,
        func_type: &str,
        args: &'a [PreSymbolicExpression],
        indent: &Indent,
    ) -> bool {
        let mut flat_width = indent.len() + 1 + func_type.len() + 1; // "(or" + ")"
        for arg in args {
            let formatted = self.format_source_exprs(slice::from_ref(arg), indent);
            if formatted.contains('\n') {
                return false;
            }
            flat_width += formatted.len() + 1; // " arg"
            if flat_width > self.settings.max_line_length {
                return false;
            }
        }
        true
    }

    fn format_if(&mut self, exprs: &'a [PreSymbolicExpression], indent: &Indent) -> String {
        let opening = exprs.first().unwrap();
        let func_type = self.display_pse(opening, indent);
        let nested = indent.indented(&self.indentation_str);

        let mut body = self.lines_doc(&exprs[1..], &nested, true);
        // The condition stays on the `(if` line
        if let Some(first_break) = body.first_mut() {
            *first_break = Doc::text(" ");
        }
        self.render_doc(
            &block(Doc::text(format!("({func_type}")), body, ")"),
            indent,
        )
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
                let binding_parts = self.lines_doc(args, &double_indent, false);
                // Double-indent for bindings, close paren at single-indent
                parts.push(Doc::indent(Doc::concat(vec![
                    Doc::indent(Doc::concat(binding_parts)),
                    Doc::Hardline,
                    Doc::text(")"),
                ])));
            }
        }

        parts.push(self.body_doc(exprs.get(2..).unwrap_or_default(), &nested));
        parts.push(Doc::Hardline);
        parts.push(Doc::text(")"));

        let doc = Doc::concat(parts);
        self.render_doc(&doc, indent)
    }

    /// Body expressions one per line at `indent`, keeping at most one blank
    /// line between them.
    fn body_doc(&mut self, body_exprs: &'a [PreSymbolicExpression], indent: &Indent) -> Doc {
        let mut body_parts: Vec<Doc> = Vec::new();
        let mut prev_end_line = None;
        for (i, e) in body_exprs.iter().enumerate() {
            body_parts.push(line_break(prev_end_line, e));
            self.check_and_cache_ignored(e, body_exprs.get(i + 1));
            body_parts.push(Doc::verbatim(
                self.format_source_exprs(slice::from_ref(e), indent),
            ));
            prev_end_line = Some(e.span().end_line);
        }
        Doc::indent(Doc::concat(body_parts))
    }

    /// Each expression on its own line at `indent`, followed by its trailing
    /// comment. With `keep_blank_lines`, keeps at most one blank line from the
    /// source between expressions.
    fn lines_doc(
        &mut self,
        exprs: &'a [PreSymbolicExpression],
        indent: &Indent,
        keep_blank_lines: bool,
    ) -> Vec<Doc> {
        let mut parts: Vec<Doc> = Vec::new();
        let mut prev_end_line = None;
        let mut iter = exprs.iter().peekable();
        while let Some(expr) = iter.next() {
            let trailing = get_trailing_comment(expr, &mut iter);
            self.check_and_cache_ignored(expr, iter.peek().copied());
            parts.push(line_break(prev_end_line, expr));
            parts.push(Doc::verbatim(
                self.format_source_exprs(slice::from_ref(expr), indent),
            ));
            self.push_trailing_comment(&mut parts, trailing, indent);
            if keep_blank_lines {
                prev_end_line = Some(expr.span().end_line);
            }
        }
        parts
    }

    /// Appends ` <comment>` if there is a trailing comment.
    fn push_trailing_comment(
        &mut self,
        parts: &mut Vec<Doc>,
        trailing: Option<&'a PreSymbolicExpression>,
        indent: &Indent,
    ) {
        if let Some(comment) = trailing {
            parts.push(Doc::text(" "));
            parts.push(Doc::verbatim(self.display_pse(comment, indent)));
        }
    }

    /// `comment` preceded by the spaces that separate it from `item` in the source.
    fn spaced_comment(
        &mut self,
        item: &PreSymbolicExpression,
        comment: &'a PreSymbolicExpression,
        indent: &Indent,
    ) -> String {
        let mut text = String::new();
        let count = comment
            .span()
            .start_column
            .saturating_sub(item.span().end_column + 1);
        push_spaces(&mut text, count);
        text.push_str(&self.display_pse(comment, indent));
        text
    }

    // * match *
    // always multiple lines
    fn format_match(&mut self, exprs: &'a [PreSymbolicExpression], indent: &Indent) -> String {
        let nested = indent.indented(&self.indentation_str);

        // value to match on
        let match_value = self.format_source_exprs(slice::from_ref(&exprs[1]), indent);

        // With comments, each branch part goes on its own line
        if contains_comments(exprs) {
            let branch_parts = self.lines_doc(&exprs[2..], &nested, false);
            let open = Doc::concat(vec![Doc::text("(match "), Doc::verbatim(match_value)]);
            return self.render_doc(&block(open, branch_parts, ")"), indent);
        }

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
                    self.push_trailing_comment(&mut branch_parts, expr_trailing, indent);
                }
            }
            self.push_trailing_comment(&mut branch_parts, trailing, indent);
        }

        let open = Doc::concat(vec![Doc::text("(match "), Doc::verbatim(match_value)]);
        self.render_doc(&block(open, branch_parts, ")"), indent)
    }

    /// Format a list of expressions as `(item1 item2 ...)`.
    /// Breaks to multi-line if it contains a comment or any line would
    /// exceed `max_line_length`.
    fn display_list(&mut self, exprs: &'a [PreSymbolicExpression], indent: &Indent) -> String {
        let mut items: Vec<String> = Vec::new();
        let mut iter = exprs.iter().peekable();
        while let Some(item) = iter.next() {
            let trailing = get_trailing_comment(item, &mut iter);
            self.check_and_cache_ignored(item, iter.peek().copied());
            let mut text = self.format_source_exprs(slice::from_ref(item), indent);
            if let Some(comment) = trailing {
                text.push_str(&self.spaced_comment(item, comment, indent));
            }
            items.push(text);
        }

        // A comment runs to the end of its line, so it always forces a break
        let should_break = contains_comments(exprs)
            || !fits_inline(
                &items,
                indent.len() + 1, // opening paren
                self.settings.max_line_length,
            );

        let doc = if should_break {
            let mut body: Vec<Doc> = Vec::new();
            for item in items {
                body.push(Doc::Hardline);
                body.push(Doc::verbatim(item));
            }
            block(Doc::text("("), body, ")")
        } else {
            let mut parts = vec![Doc::text("(")];
            for (i, item) in items.into_iter().enumerate() {
                if i > 0 {
                    parts.push(Doc::text(" "));
                }
                parts.push(Doc::verbatim(item));
            }
            parts.push(Doc::text(")"));
            Doc::concat(parts)
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
                    let allowance_parts = self.lines_doc(allowances, &double_indent, false);
                    parts.push(Doc::text(" ("));
                    parts.push(Doc::indent(Doc::concat(vec![
                        Doc::indent(Doc::concat(allowance_parts)),
                        Doc::Hardline,
                        Doc::text(")"),
                    ])));
                }
            }
        }

        parts.push(self.body_doc(exprs.get(3..).unwrap_or_default(), &nested));
        parts.push(Doc::Hardline);
        parts.push(Doc::text(")"));

        let doc = Doc::concat(parts);
        self.render_doc(&doc, indent)
    }

    /// Check if an expression represents a signed integer (for list type size detection)
    /// Only signed integers are valid for list type signatures: (list 10 <type>)
    /// Unsigned integers like u10 would be list elements, not type signatures
    fn is_integer_expr(&self, expr: &PreSymbolicExpression) -> bool {
        match &expr.pre_expr {
            PreSymbolicExpressionType::AtomValue(ref value) => {
                matches!(value, clarity::vm::types::Value::Int(_))
            }
            _ => false,
        }
    }

    /// Detect if this is a list type signature: (list <integer> <type>)
    fn is_list_type_signature(&self, exprs: &[PreSymbolicExpression]) -> bool {
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

    /// `{ key: value }` on one line, or `None` if a comment or a multi-line
    /// value forces multiline, or the line is too long. Tuple sugar and
    /// `(tuple ...)` both use this, so converting one to the other keeps the layout.
    fn inline_entry(
        &mut self,
        key: &'a PreSymbolicExpression,
        value: &'a [PreSymbolicExpression],
        indent: &Indent,
    ) -> Option<String> {
        if is_comment(key) || contains_comments(value) {
            return None;
        }
        let nested = indent.indented(&self.indentation_str);
        // Formatted as the multiline layout would, so that reuses the cached result
        let fvalue = self.format_source_exprs(value, &nested);
        let fkey = self.display_pse(key, indent);
        // "{ " + "key: " + "value, " + " }"
        let width = indent.len() + fkey.len() + fvalue.len() + 8;
        if fvalue.contains('\n') || width > self.settings.max_line_length {
            return None;
        }
        let doc = Doc::concat(vec![
            Doc::text("{ "),
            Doc::verbatim(fkey),
            Doc::text(": "),
            Doc::verbatim(fvalue),
            Doc::text(" }"),
        ]);
        Some(self.render_doc(&doc, indent))
    }

    /// Format `(list ...)` expressions with greedy line-wrapping.
    fn format_list(&mut self, exprs: &'a [PreSymbolicExpression], indent: &Indent) -> String {
        let nested = indent.indented(&self.indentation_str);
        let is_list_cons = self.display_pse(&exprs[0], indent) == "list";
        let start_index = usize::from(is_list_cons);

        let is_list_type_sig = self.is_list_type_signature(exprs);

        // A comment runs to the end of its line, so it forces multiline
        let is_multiline = contains_comments(&exprs[start_index..]) || {
            let prefix_len = if is_list_cons { 6 } else { 1 }; // "(list " or "("
            let estimated_len = self.estimate_list_length(exprs, start_index, prefix_len);
            indent.len() + estimated_len > self.settings.max_line_length
        };

        let spacing = if is_multiline { &nested } else { indent };

        struct ListItem {
            text: String,
            trailing_comment: Option<String>,
            is_comment: bool,
        }
        let mut items: Vec<ListItem> = Vec::new();
        let mut type_sig_size: Option<String> = None;

        let mut iter = exprs[start_index..].iter().peekable();
        while let Some(item) = iter.next() {
            let trailing = get_trailing_comment(item, &mut iter);
            self.check_and_cache_ignored(item, iter.peek().copied());

            // The size of a multiline type signature stays on the `(list` line
            let is_first_item = type_sig_size.is_none() && items.is_empty();
            if is_multiline && is_first_item && is_list_type_sig {
                let size_value = self.display_pse(item, indent);
                let mut size_text = size_value.trim().replace('\n', " ").replace('\r', "");
                if let Some(comment) = trailing {
                    size_text.push_str(&self.spaced_comment(item, comment, indent));
                }
                type_sig_size = Some(size_text);
                continue;
            }

            let text = self.format_source_exprs(slice::from_ref(item), spacing);
            let trailing_comment = trailing.map(|c| self.spaced_comment(item, c, indent));
            items.push(ListItem {
                text,
                trailing_comment,
                is_comment: is_comment(item),
            });
        }

        if !is_multiline {
            // Single line, so there are no comments
            let mut parts = vec![Doc::text(if is_list_cons { "(list" } else { "(" })];
            for (i, item) in items.into_iter().enumerate() {
                if i > 0 || is_list_cons {
                    parts.push(Doc::text(" "));
                }
                parts.push(Doc::verbatim(item.text));
            }
            parts.push(Doc::text(")"));
            return self.render_doc(&Doc::concat(parts), indent);
        }

        // Multiline
        let mut open = vec![Doc::text(if is_list_cons { "(list" } else { "(" })];
        let mut body: Vec<Doc> = Vec::new();

        if is_list_type_sig {
            // Type sig: (list <size>\n  <type>\n)
            if let Some(size_text) = type_sig_size {
                open.push(Doc::text(" "));
                open.push(Doc::verbatim(size_text));
            }
            for item in items {
                body.push(Doc::Hardline);
                body.push(Doc::verbatim(item.text));
                if let Some(c) = item.trailing_comment {
                    body.push(Doc::verbatim(c));
                }
            }
        } else {
            // Greedy wrapping with one item of lookahead
            body.push(Doc::Hardline);
            let mut col = nested.len(); // column after first hardline + indent
            let mut after_comment = false;

            let mut items = items.into_iter().enumerate().peekable();
            while let Some((i, item)) = items.next() {
                if i > 0 {
                    // Wrap unless this item and the next both fit on the current line
                    let next_len = items.peek().map_or(0, |(_, n)| n.text.len() + 1);
                    let would_exceed =
                        col + 1 + item.text.len() + next_len > self.settings.max_line_length;
                    // Multiline items and standalone comments always get their own line
                    if after_comment || item.is_comment || item.text.contains('\n') || would_exceed
                    {
                        body.push(Doc::Hardline);
                        col = nested.len();
                    } else {
                        body.push(Doc::text(" "));
                        col += 1;
                    }
                }

                col = last_line_len(&item.text, col);
                after_comment = item.is_comment || item.trailing_comment.is_some();
                body.push(Doc::verbatim(item.text));
                if let Some(c) = item.trailing_comment {
                    col += c.len();
                    body.push(Doc::verbatim(c));
                }
            }
        }

        self.render_doc(&block(Doc::concat(open), body, ")"), indent)
    }

    // used for { n1: 1 } syntax
    fn format_key_value_sugar(
        &mut self,
        exprs: &'a [PreSymbolicExpression],
        indent: &Indent,
    ) -> String {
        let inline = match exprs {
            [key, value] => self.inline_entry(key, slice::from_ref(value), indent),
            _ => None,
        };
        if let Some(inline) = inline {
            inline
        } else {
            let nested = indent.indented(&self.indentation_str);
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
                let mut comments = Vec::new();
                while let Some(comment) = iter.next_if(|next| is_comment(next)) {
                    comments.push(comment);
                }
                if let Some(value) = iter.next() {
                    let trailing = get_trailing_comment(value, &mut iter);
                    self.check_and_cache_ignored(value, iter.peek().copied());
                    entry_parts.extend(self.entry_doc(key_str, comments, value, &nested));
                    self.push_trailing_comment(&mut entry_parts, trailing, &nested);
                }
            }

            self.render_doc(&block(Doc::text("{"), entry_parts, "}"), indent)
        }
    }

    /// A `key: value,` line of a multiline tuple. Comments between the key and
    /// the value go on their own lines, with the value below them.
    fn entry_doc(
        &mut self,
        key: String,
        comments: impl IntoIterator<Item = &'a PreSymbolicExpression>,
        value: &'a PreSymbolicExpression,
        nested: &Indent,
    ) -> Vec<Doc> {
        let mut comment_parts = Vec::new();
        let mut comments = comments.into_iter().peekable();
        while let Some(comment) = comments.next() {
            self.check_and_cache_ignored(comment, comments.peek().copied().or(Some(value)));
            comment_parts.push(Doc::Hardline);
            comment_parts.push(Doc::verbatim(self.display_pse(comment, nested)));
        }
        if comment_parts.is_empty() {
            let fvalue = self.format_source_exprs(slice::from_ref(value), nested);
            return vec![Doc::Hardline, Doc::verbatim(format!("{key}: {fvalue},"))];
        }
        let double_indent = nested.indented(&self.indentation_str);
        let fvalue = self.format_source_exprs(slice::from_ref(value), &double_indent);
        comment_parts.push(Doc::Hardline);
        comment_parts.push(Doc::verbatim(format!("{fvalue},")));
        vec![
            Doc::Hardline,
            Doc::verbatim(format!("{key}:")),
            Doc::indent(Doc::concat(comment_parts)),
        ]
    }

    /// Preserve unrecognized syntax without interpreting nested lists as function
    /// calls or dropping extra arguments.
    fn preserve_syntax(&mut self, expr: &'a PreSymbolicExpression, indent: &Indent) -> String {
        let (items, open, close, tuple) = match &expr.pre_expr {
            PreSymbolicExpressionType::List(items) => (items, "(", ')', false),
            PreSymbolicExpressionType::Tuple(items) => (items, "{", '}', true),
            _ => return self.display_pse(expr, indent),
        };
        let mut result = String::from(open);
        let mut is_key = true;
        for (i, item) in items.iter().enumerate() {
            if i > 0 && !is_comment(&items[i - 1]) {
                result.push(' ');
            }
            result.push_str(&self.preserve_syntax(item, indent));
            if is_comment(item) {
                result.push('\n');
            } else if tuple {
                result.push(if is_key { ':' } else { ',' });
                is_key = !is_key;
            }
        }
        result.push(close);
        result
    }

    /// `(tuple (n1 1))` converted to `{ n1: 1 }`, or `None` if an entry isn't a
    /// key and a single value.
    fn format_key_value(
        &mut self,
        exprs: &'a [PreSymbolicExpression],
        indent: &Indent,
    ) -> Option<String> {
        if !exprs
            .iter()
            .all(|entry| is_comment(entry) || split_tuple_entry(entry).is_some())
        {
            return None;
        }
        let inline = match exprs {
            [entry] => match entry.match_list().and_then(|list| list.split_first()) {
                Some((key, value)) => self.inline_entry(key, value, indent),
                None => None,
            },
            _ => None,
        };
        if inline.is_some() {
            return inline;
        }
        let nested = indent.indented(&self.indentation_str);
        let mut entry_parts: Vec<Doc> = Vec::new();
        let mut iter = exprs.iter().peekable();
        while let Some(arg) = iter.next() {
            if is_comment(arg) {
                entry_parts.push(Doc::Hardline);
                entry_parts.push(Doc::verbatim(self.display_pse(arg, &nested)));
                continue;
            }
            let trailing = get_trailing_comment(arg, &mut iter);
            self.check_and_cache_ignored(arg, iter.peek().copied());
            let TupleEntry {
                key,
                before,
                value,
                after,
            } = split_tuple_entry(arg)?;
            let fkey = self.display_pse(key, &nested);
            entry_parts.extend(self.entry_doc(fkey, before, value, &nested));
            for comment in after.iter().chain(trailing) {
                if comment.span().start_line == value.span().end_line {
                    entry_parts.push(Doc::text(" "));
                } else {
                    entry_parts.push(Doc::Hardline);
                }
                entry_parts.push(Doc::verbatim(self.display_pse(comment, &nested)));
            }
        }
        Some(self.render_doc(&block(Doc::text("{"), entry_parts, "}"), indent))
    }

    // This prints leaves of the PSE tree
    fn display_pse(&mut self, pse: &'a PreSymbolicExpression, indent: &Indent) -> String {
        match pse.pre_expr {
            PreSymbolicExpressionType::Atom(ref value) => t(value).to_string(),
            PreSymbolicExpressionType::AtomValue(ref value) => match value {
                clarity::vm::types::Value::Principal(c) => {
                    format!("'{c}")
                }
                clarity::vm::types::Value::Sequence(clarity::vm::types::SequenceData::String(
                    ref string_data,
                )) => {
                    match string_data {
                        clarity::vm::types::CharType::ASCII(ascii_data) => {
                            // ASCIIData's Display emits escapes Clarity rejects, such as \'.
                            let mut literal = String::with_capacity(ascii_data.data.len() + 2);
                            literal.push('"');
                            for &byte in &ascii_data.data {
                                match byte {
                                    b'\\' => literal.push_str("\\\\"),
                                    b'"' => literal.push_str("\\\""),
                                    b'\n' => literal.push_str("\\n"),
                                    b'\r' => literal.push_str("\\r"),
                                    b'\t' => literal.push_str("\\t"),
                                    _ => literal.push(char::from(byte)),
                                }
                            }
                            literal.push('"');
                            literal
                        }
                        clarity::vm::types::CharType::UTF8(_) => {
                            // utf8 strings use the original source to preserve formatting
                            self.source_lines
                                .as_ref()
                                .map(|source_lines| source_lines.extract_expr_source(pse))
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
        }
    }

    // Top level define-<function> should have a line break above and after (except on first line)
    // Functions always on multiple lines, even if short
    fn function(&mut self, exprs: &'a [PreSymbolicExpression]) -> String {
        let empty = Indent::empty();
        let func_type = self.display_pse(exprs.first().unwrap(), &empty);
        let one_indent = empty.indented(&self.indentation_str);
        let args_indent = one_indent.indented(&self.indentation_str);

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
                        self.push_trailing_comment(&mut arg_parts, trailing, &empty);
                        prev_end_line = trailing.unwrap_or(arg).span().end_line;
                    }
                    // Args at double indent
                    parts.push(Doc::indent(Doc::indent(Doc::concat(arg_parts))));
                    if args.is_empty() {
                        parts.push(Doc::text(")"));
                    } else {
                        // Close args paren at single indent
                        parts.push(Doc::indent(Doc::concat(vec![
                            Doc::Hardline,
                            Doc::text(")"),
                        ])));
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
        // Simple wrappers like (ok expr) are always kept on one line
        if list.len() == 2 && list[0].match_atom().is_some() && !is_comment(&list[1]) {
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

        let base_indent = indent.indented(&self.indentation_str);
        let mut result = String::new();
        let mut current_line_width = indent.len();
        let mut prev_multiline = false;
        let mut prev_is_map = false;
        for (i, expr) in list.iter().enumerate() {
            let indented = if i == 0 { &base_indent } else { indent };
            self.check_and_cache_ignored(expr, list.get(i + 1));
            let mut formatted = self.format_source_exprs(slice::from_ref(expr), indented);
            let trimmed = t(&formatted);

            if i > 0 {
                let is_map_opening = trimmed.starts_with('{');
                let prev = &list[i - 1];
                let on_different_line_in_source = (is_comment(expr)
                    && prev.span().start_line != expr.span().start_line)
                    || (is_comment(prev) && prev.span().start_line != expr.span().start_line);

                // `} {` keeps a map next to the multi-line map before it, as in
                // `(map-set m { .. } { .. })`; anything else after a multi-line
                // argument starts a new line
                if on_different_line_in_source
                    || (prev_multiline && !(is_map_opening && prev_is_map))
                    || (!is_map_opening
                        && (current_line_width + trimmed.len() + 1 > self.settings.max_line_length))
                {
                    result.push('\n');
                    result.push_str(&base_indent);
                    current_line_width = base_indent.len();
                    formatted = self.format_source_exprs(slice::from_ref(expr), &base_indent);
                } else {
                    result.push(' ');
                    current_line_width += 1;
                }
            }

            let text = t(&formatted);
            result.push_str(text);
            current_line_width = last_line_len(text, current_line_width);
            prev_multiline = text.contains('\n');
            prev_is_map = text.starts_with('{');
        }

        let break_lines = if list.last().is_some_and(is_comment) {
            true
        } else if !result.contains('\n') {
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

/// `open`, then `body` one level deeper, then `close` on its own line.
fn block(open: Doc, body: Vec<Doc>, close: &str) -> Doc {
    Doc::concat(vec![
        open,
        Doc::indent(Doc::concat(body)),
        Doc::Hardline,
        Doc::text(close),
    ])
}

/// The line break before `expr`: a blank line if the source has one after
/// `prev_end_line`, otherwise a single newline.
fn line_break(prev_end_line: Option<u32>, expr: &PreSymbolicExpression) -> Doc {
    if has_blank_line(prev_end_line, expr.span().start_line) {
        Doc::BlankLine
    } else {
        Doc::Hardline
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
/// Whether `items`, joined by single spaces from column `start`, keep the
/// lines they share (and the closing paren) within `max_width`. Inner lines of
/// a multi-line item don't depend on this layout, so they aren't counted.
fn fits_inline(items: &[String], start: usize, max_width: usize) -> bool {
    let mut col = start;
    for (i, item) in items.iter().enumerate() {
        let separator = usize::from(i > 0);
        if col + separator + first_line_len(item) > max_width {
            return false;
        }
        col = last_line_len(item, col + separator);
    }
    col < max_width
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
            if is_comment(next) && expr.span().end_line == next.span().start_line {
                iter.next();
                Some(next)
            } else {
                None
            }
        }
        _ => None,
    }
}

/// A `(key ;; c value ;; c)` tuple entry, split around its value.
struct TupleEntry<'a> {
    key: &'a PreSymbolicExpression,
    before: &'a [PreSymbolicExpression],
    value: &'a PreSymbolicExpression,
    after: &'a [PreSymbolicExpression],
}

/// `None` unless `entry` is a key atom followed by one value and any comments.
fn split_tuple_entry(entry: &PreSymbolicExpression) -> Option<TupleEntry<'_>> {
    let (key, rest) = entry.match_list()?.split_first()?;
    key.match_atom()?;
    let pos = rest.iter().position(|expr| !is_comment(expr))?;
    let (before, after) = (&rest[..pos], &rest[pos + 1..]);
    after.iter().all(is_comment).then_some(TupleEntry {
        key,
        before,
        value: &rest[pos],
        after,
    })
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
    use clarity::types::StacksEpochId;
    use clarity::vm::ast::stack_depth_checker::StackDepthLimits;
    use clarity::vm::representations::{PreSymbolicExpression, PreSymbolicExpressionType};
    use clarity::vm::types::Value;
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

    fn format_with_epoch(source: &str, epoch: StacksEpochId) -> String {
        let formatter = ClarityFormatter::new(Settings::default());
        formatter.format_section(source, Some(epoch)).unwrap()
    }

    fn format_with(source: &str, settings: Settings) -> String {
        let formatter = ClarityFormatter::new(settings);
        formatter.format_section(source, None).unwrap()
    }

    #[test]
    fn test_ascii_string_escapes_round_trip() {
        fn values(expressions: &[PreSymbolicExpression]) -> Vec<Value> {
            expressions
                .iter()
                .flat_map(|expression| match &expression.pre_expr {
                    PreSymbolicExpressionType::AtomValue(value) => vec![value.clone()],
                    PreSymbolicExpressionType::List(items) => values(items),
                    _ => vec![],
                })
                .collect()
        }

        let formatter = ClarityFormatter::new(Settings::default());
        let parse = |source: &str| {
            clarity::vm::ast::parser::v2::parse(source, StackDepthLimits::for_epoch(DEFAULT_EPOCH))
        };
        for literal in [
            r#""a\"b""#,
            r#""a\\b""#,
            r#""a\tb""#,
            r#""a\nb""#,
            r#""a\rb""#,
            r#""""#,
            r#""it's printable!""#,
            r#""\\'\\x0c""#,
            "\"form\u{c}feed\"",
            r#"(list "a\"b" "\\" "" "\t\n\r")"#,
        ] {
            let source = format!("(define-constant s {literal})");
            let original = parse(&source).expect("valid input");
            let expected_values = values(&original);
            assert!(!expected_values.is_empty());

            for (mode, formatted) in [
                ("file", formatter.format_file(&source, None)),
                ("section", formatter.format_section(&source, None).unwrap()),
                ("ast", formatter.format_ast(&original)),
            ] {
                let reparsed = parse(&formatted).unwrap_or_else(|error| {
                    panic!("{mode} formatting {source:?} produced invalid source {formatted:?}: {error}")
                });
                assert_eq!(values(&reparsed), expected_values, "{mode}: {source}");
                let reformatted = match mode {
                    "file" => formatter.format_file(&formatted, None),
                    "section" => formatter.format_section(&formatted, None).unwrap(),
                    _ => formatter.format_ast(&reparsed),
                };
                assert_eq!(reformatted, formatted, "{mode}: {source}");
            }
        }
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
            let aggregator = Aggregator::new(&settings, &exprs, Some(src));
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
    fn test_nested_concat() {
        let src = "(concat \"hello\" (concat \"world\" \"!\"))";
        let result = format_with_default(src);
        assert_eq!(src, result);
    }

    #[test]
    fn test_variadic_concat() {
        let src = "(concat \"hello\" \"world\" \"!\")";
        let result = format_with_epoch(src, StacksEpochId::Epoch40);
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

    /// Formats `src` as a file and checks the output parses and is stable.
    fn format_valid_and_stable(src: &str) -> String {
        let formatter = ClarityFormatter::new(Settings::default());
        let result = formatter.format_file(src, None);
        clarity::vm::ast::parser::v2::parse(&result, StackDepthLimits::for_epoch(DEFAULT_EPOCH))
            .unwrap_or_else(|e| panic!("invalid output for {src:?}:\n{result}\n{e}"));
        assert_eq!(
            formatter.format_file(&result, None),
            result,
            "not idempotent"
        );
        result
    }

    #[test]
    fn test_standalone_comment_breaks_list() {
        let src = "(define-trait t ((f (uint\n ;; field\n uint) (response uint uint))))";
        let expected = indoc!(
            r#"
            (define-trait t (
              (f
                (
                  uint
                  ;; field
                  uint
                )
                (response uint uint)
              )
            ))
            "#
        );
        assert_eq!(format_valid_and_stable(src), expected);
    }

    #[test]
    fn test_comment_in_list_cons() {
        for src in [
            "(define-constant l (list u1\n ;; c\n u2))",
            "(define-constant l (list u1 ;; c\n u2))",
            "(define-constant l (list 10 ;; size\n uint))",
        ] {
            let result = format_valid_and_stable(src);
            assert!(
                result.contains(";; c\n") || result.contains(";; size\n"),
                "{result}"
            );
        }
    }

    #[test]
    fn test_multiline_expression_keeps_trailing_comment() {
        let field = "a".repeat(74);
        for (src, trailing) in [
            (
                format!("(list {{{field}: u1}} ;; trailing\n {{{field}: u2}})"),
                "} ;; trailing\n",
            ),
            (
                format!("(let ((x {{{field}: u1}}) ;; trailing\n (y {{{field}: u2}})) x)"),
                "}) ;; trailing\n",
            ),
            (
                format!("{{x: {{{field}: u1}}, ;; trailing\n y: {{{field}: u2}}}}"),
                "}, ;; trailing\n",
            ),
            (
                "(define-private (f)\n  (ok u1)) ;; trailing\n(define-private (g) (ok u2))"
                    .to_string(),
                "\n) ;; trailing\n",
            ),
        ] {
            let result = format_valid_and_stable(&src);
            assert!(result.contains(trailing), "{result}");
        }
    }

    #[test]
    fn test_tuple_constructor_matches_sugar_layout() {
        for (constructor, sugar) in [
            (
                "(define-constant X (tuple (a { a: u1, b: u2 })))",
                "(define-constant X { a: { a: u1, b: u2 } })",
            ),
            (
                "(define-constant X (tuple (a (begin u1))))",
                "(define-constant X { a: (begin u1) })",
            ),
            (
                "(define-constant X (tuple (a u1)))",
                "(define-constant X { a: u1 })",
            ),
        ] {
            assert_eq!(
                format_valid_and_stable(constructor),
                format_valid_and_stable(sugar)
            );
        }
    }

    #[test]
    fn test_format_ignore_trailing_comment() {
        let src = "(begin ;; @format-ignore\n (foo   1) ;; trailing\n (ok true))";
        let result = format_valid_and_stable(src);
        assert_eq!(result.matches(";; trailing").count(), 1, "{result}");

        let src = ";; @format-ignore\n(foo   1) ;; trailing\n(bar)";
        let result = format_valid_and_stable(src);
        assert_eq!(result.matches(";; trailing").count(), 1, "{result}");
    }

    #[test]
    fn test_format_ignore_before_non_list() {
        let src = "(define-constant x (list\n ;; @format-ignore\n u1\n u2))";
        let result = format_valid_and_stable(src);
        assert!(!result.contains("@format-ignoreu1"), "{result}");

        let src = ";; @format-ignore\n;; @format-ignore\n(foo   1)";
        let result = format_valid_and_stable(src);
        assert_eq!(result, format!("{src}\n"));
    }

    #[test]
    fn test_format_ignore_without_source() {
        let src =
            ";; @format-ignore\n(define-read-only (foo) (ok u1))\n(define-read-only (bar) (ok u2))";
        let ast =
            clarity::vm::ast::parser::v2::parse(src, StackDepthLimits::for_epoch(DEFAULT_EPOCH))
                .unwrap();
        let formatter = ClarityFormatter::new(Settings::default());
        let result = formatter.format_ast(&ast);
        let reparsed = clarity::vm::ast::parser::v2::parse(
            &result,
            StackDepthLimits::for_epoch(DEFAULT_EPOCH),
        )
        .unwrap();
        assert_eq!(reparsed.len(), ast.len(), "{result}");
    }

    #[test]
    fn test_lists_without_leading_atom_are_separated() {
        let src = "(foo)\n((bar))\n((baz))";
        let result = format_valid_and_stable(src);
        assert_eq!(result, "(foo)\n((bar))\n((baz))\n");
    }

    #[test]
    fn test_begin_in_tuple_is_multiline() {
        let result = format_valid_and_stable("(define-constant X { a: (begin 1) })");
        assert_eq!(
            result,
            indoc!(
                r#"
                (define-constant X {
                  a: (begin
                    1
                  ),
                })
                "#
            )
        );
    }

    #[test]
    fn test_map_after_multiline_arg() {
        let src = "(map-set name-properties { name: name, namespace: namespace } (merge (unwrap-panic (map-get? name-properties { name: name, namespace: namespace })) {stx-burn: (try! (compute-name-price name (get price-function namespace-props)))}))";
        let result = format_valid_and_stable(src);
        assert!(!result.contains("})) {"), "{result}");
    }
}
