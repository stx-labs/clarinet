pub mod helpers;
pub mod ignored;

use std::cell::RefCell;
use std::collections::HashMap;
use std::iter::Peekable;
use std::{fmt, slice};

use clarity::vm::functions::define::DefineFunctions;
use clarity::vm::functions::NativeFunctions;
use clarity::vm::representations::{PreSymbolicExpression, PreSymbolicExpressionType};
use helpers::t;
use ignored::ignored_exprs;

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

pub struct ClarityFormatter {
    settings: Settings,
}
impl ClarityFormatter {
    pub fn new(settings: Settings) -> Self {
        Self { settings }
    }
    /// formatting for files to ensure a newline at the end
    pub fn format_file(&self, source: &str) -> String {
        let trimmed_source = source.trim_start_matches(['\n', '\r']);
        let pse = clarity::vm::ast::parser::v2::parse(trimmed_source).unwrap();
        let agg = Aggregator::new(&self.settings, &pse, Some(trimmed_source));
        let result = agg.generate();

        // make sure the file ends with a newline
        format!("{}\n", result.trim_end_matches(['\n', '\r']))
    }
    /// formatting an AST without a source file
    pub fn format_ast(&self, pse: &[PreSymbolicExpression]) -> String {
        let agg = Aggregator::new(&self.settings, pse, None);
        agg.generate()
    }
    /// Alias `format_file` to `format`
    pub fn format(&self, source: &str) -> String {
        self.format_file(source)
    }
    /// for range formatting within editors
    pub fn format_section(&self, source: &str) -> Result<String, String> {
        let pse = clarity::vm::ast::parser::v2::parse(source).map_err(|e| e.to_string())?;

        // range formatting specifies to the aggregator that we're
        // starting mid-source and thus should pre-populate
        // `previous_indentation` for format_source_exprs
        let indentation_level = source.chars().take_while(|c| c.is_whitespace()).count();
        let leading_spaces = &source[..indentation_level];
        let agg = Aggregator::new(&self.settings, &pse, Some(source));

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

    cache: RefCell<HashMap<(usize, String), String>>,
}

impl<'a> Aggregator<'a> {
    pub fn new(
        settings: &'a Settings,
        pse: &'a [PreSymbolicExpression],
        source: Option<&'a str>,
    ) -> Self {
        Aggregator {
            settings,
            pse,
            source,
            cache: RefCell::new(HashMap::new()),
        }
    }
    pub fn generate(&self) -> String {
        self.cache.borrow_mut().clear();
        // this handles if we're formatting a section of code rather than the whole file
        let indentation_level = match self.source {
            Some(source) => source.chars().take_while(|c| c.is_whitespace()).count(),
            None => {
                (self
                    .pse
                    .first()
                    .map_or(0, |expr| expr.span().start_column.saturating_sub(1)))
                    as usize
            }
        };
        let previous_indentation = match self.source {
            Some(source) => &source[..indentation_level],
            None => &" ".repeat(indentation_level),
        };

        let formatted = self.format_source_exprs(self.pse, previous_indentation);
        // If we're formatting an AST without a source and there's indentation,
        // we need to ensure it's applied to the beginning of the output
        if self.source.is_none() && indentation_level > 0 {
            // Only add indentation if we're formatting a top-level expression
            if self.pse.len() == 1 && formatted.starts_with('(') {
                return format!("{previous_indentation}{formatted}");
            }
        }

        formatted
    }

    fn format_source_exprs(
        &self,
        expressions: &[PreSymbolicExpression],
        previous_indentation: &str,
    ) -> String {
        // Create a key based on the slice pointer and length for the whole array
        let key = (
            expressions.as_ptr() as usize,
            previous_indentation.to_string(),
        );

        // Check if we have a cached result
        let cached_result = {
            let cache_ref = self.cache.borrow();
            cache_ref.get(&key).cloned()
        };
        if let Some(result) = cached_result {
            return result;
        }
        // Track the end line of the previous expression
        let mut prev_end_line = 0;

        // use peekable to handle trailing comments nicely
        let mut iter = expressions.iter().peekable();
        let mut result = "".to_owned(); // Accumulate results here

        while let Some(expr) = iter.next() {
            let trailing_comment = get_trailing_comment(expr, &mut iter);
            let cur = self.display_pse(expr, previous_indentation);
            if cur.contains(FORMAT_IGNORE_SYNTAX) {
                result.push_str(&cur);
                if let Some(next) = iter.peek() {
                    if next.match_list().is_some() {
                        let next_expr = iter.next().unwrap();
                        result.push('\n');
                        result.push_str(&ignored_exprs(
                            std::slice::from_ref(next_expr),
                            self.source.unwrap_or_default(),
                        ));
                        result.push('\n');
                        prev_end_line = next_expr.span().end_line;
                    }
                }
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
            if let Some(list) = expr.match_list() {
                if let Some(atom_name) = list.split_first().and_then(|(f, _)| f.match_atom()) {
                    let formatted = if let Some(native) = NativeFunctions::lookup_by_name(atom_name)
                    {
                        match native {
                            NativeFunctions::Let => self.format_let(list, previous_indentation),
                            NativeFunctions::Begin => self.format_begin(list, previous_indentation),
                            NativeFunctions::Match =>
                              if contains_comments(list) {
                                self.match_with_comments(list, previous_indentation)
                              } else {
                                self.format_match(list, previous_indentation)
                              },
                            NativeFunctions::TupleCons => {
                                // if the kv map is defined with (tuple (c 1)) then we strip the
                                // ClarityName("tuple") out first and convert it to key/value syntax
                                self.format_key_value(&list[1..], previous_indentation)
                            }
                            NativeFunctions::If => self.format_if(list, previous_indentation),
                            NativeFunctions::And | NativeFunctions::Or => {
                                self.format_booleans(list, previous_indentation)
                            }

                            NativeFunctions::ListCons => self.format_list(list, previous_indentation),

                            // everything else that's not special cased
                            NativeFunctions::Add
                            | NativeFunctions::Subtract
                            | NativeFunctions::Multiply
                            | NativeFunctions::Divide
                            | NativeFunctions::CmpGeq
                            | NativeFunctions::CmpLeq
                            | NativeFunctions::CmpLess
                            | NativeFunctions::CmpGreater
                            | NativeFunctions::ToInt
                            | NativeFunctions::ToUInt
                            | NativeFunctions::Modulo
                            | NativeFunctions::Power
                            | NativeFunctions::Sqrti
                            | NativeFunctions::Log2
                            | NativeFunctions::BitwiseXor
                            | NativeFunctions::Not
                            | NativeFunctions::Equals
                            | NativeFunctions::Map
                            | NativeFunctions::Fold
                            | NativeFunctions::Append
                            | NativeFunctions::Concat
                            | NativeFunctions::AsMaxLen
                            | NativeFunctions::Len
                            | NativeFunctions::ElementAt
                            | NativeFunctions::ElementAtAlias
                            | NativeFunctions::IndexOf
                            | NativeFunctions::IndexOfAlias
                            | NativeFunctions::BuffToIntLe
                            | NativeFunctions::BuffToUIntLe
                            | NativeFunctions::BuffToIntBe
                            | NativeFunctions::BuffToUIntBe
                            | NativeFunctions::IsStandard
                            | NativeFunctions::PrincipalDestruct
                            | NativeFunctions::PrincipalConstruct
                            | NativeFunctions::StringToInt
                            | NativeFunctions::StringToUInt
                            | NativeFunctions::IntToAscii
                            | NativeFunctions::IntToUtf8
                            | NativeFunctions::FetchVar
                            | NativeFunctions::FetchEntry // map-get?
                            | NativeFunctions::SetEntry // map-set?
                            | NativeFunctions::SetVar
                            | NativeFunctions::InsertEntry
                            | NativeFunctions::DeleteEntry
                            | NativeFunctions::TupleGet
                            | NativeFunctions::TupleMerge
                            | NativeFunctions::Hash160
                            | NativeFunctions::Sha256
                            | NativeFunctions::Sha512
                            | NativeFunctions::Sha512Trunc256
                            | NativeFunctions::Keccak256
                            | NativeFunctions::Secp256k1Recover
                            | NativeFunctions::Secp256k1Verify
                            | NativeFunctions::Print
                            | NativeFunctions::ContractCall
                            | NativeFunctions::AsContract
                            | NativeFunctions::ContractOf
                            | NativeFunctions::PrincipalOf
                            | NativeFunctions::AtBlock
                            | NativeFunctions::GetBlockInfo
                            | NativeFunctions::GetBurnBlockInfo
                            | NativeFunctions::ConsError
                            | NativeFunctions::ConsOkay
                            | NativeFunctions::ConsSome
                            | NativeFunctions::DefaultTo
                            | NativeFunctions::Asserts
                            | NativeFunctions::UnwrapRet
                            | NativeFunctions::UnwrapErrRet
                            | NativeFunctions::Unwrap
                            | NativeFunctions::UnwrapErr
                            | NativeFunctions::TryRet
                            | NativeFunctions::IsOkay
                            | NativeFunctions::IsNone
                            | NativeFunctions::IsErr
                            | NativeFunctions::IsSome
                            | NativeFunctions::Filter
                            | NativeFunctions::GetTokenBalance
                            | NativeFunctions::GetAssetOwner
                            | NativeFunctions::TransferToken
                            | NativeFunctions::TransferAsset
                            | NativeFunctions::MintAsset
                            | NativeFunctions::MintToken
                            | NativeFunctions::GetTokenSupply
                            | NativeFunctions::BurnToken
                            | NativeFunctions::BurnAsset
                            | NativeFunctions::GetStxBalance
                            | NativeFunctions::StxTransfer
                            | NativeFunctions::StxTransferMemo
                            | NativeFunctions::StxBurn
                            | NativeFunctions::StxGetAccount
                            | NativeFunctions::BitwiseAnd
                            | NativeFunctions::BitwiseOr
                            | NativeFunctions::BitwiseNot
                            | NativeFunctions::BitwiseLShift
                            | NativeFunctions::BitwiseRShift
                            | NativeFunctions::BitwiseXor2
                            | NativeFunctions::Slice
                            | NativeFunctions::ToConsensusBuff
                            | NativeFunctions::FromConsensusBuff
                            | NativeFunctions::ReplaceAt
                            | NativeFunctions::GetStacksBlockInfo
                            | NativeFunctions::GetTenureInfo
                            // @todo: RestrictAssets will likely need formatting rework
                            | NativeFunctions::RestrictAssets
                            | NativeFunctions::AsContractSafe
                            | NativeFunctions::AllowanceAll
                            | NativeFunctions::AllowanceWithStacking
                            | NativeFunctions::AllowanceWithStx
                            | NativeFunctions::AllowanceWithFt
                            | NativeFunctions::AllowanceWithNft
                            | NativeFunctions::Secp256r1Verify
                            | NativeFunctions::ContractHash
                            | NativeFunctions::ToAscii => {
                                let inner_content =
                                    self.to_inner_content(list, previous_indentation);

                                format!(
                                    "{}{}",
                                    inner_content,
                                    if let Some(comment) = trailing_comment {
                                        format!(
                                            " {}\n",
                                            &self.display_pse(comment, previous_indentation)
                                        )
                                    } else if let Some(next) = iter.peek() {
                                        if list[0].span().end_line != next.span().end_line {
                                            "\n".to_string()
                                        } else {
                                            " ".to_string()
                                        }
                                    } else {
                                        "".to_string()
                                    }
                                )
                            }
                        }
                    } else if let Some(define) = DefineFunctions::lookup_by_name(atom_name) {
                        let formatted = match define {
                            DefineFunctions::PublicFunction
                            | DefineFunctions::ReadOnlyFunction
                            | DefineFunctions::PrivateFunction => self.function(list),
                            DefineFunctions::Constant
                            | DefineFunctions::PersistedVariable
                            | DefineFunctions::FungibleToken
                            | DefineFunctions::ImplTrait
                            | DefineFunctions::UseTrait
                            | DefineFunctions::NonFungibleToken => {
                                self.constant(list, previous_indentation)
                            }
                            DefineFunctions::Map => self.format_map(list, previous_indentation),
                            DefineFunctions::Trait => self.define_trait(list, previous_indentation),
                        };
                        let result = &formatted.to_string();
                        if let Some(comment) = trailing_comment {
                            let mut result_with_comment = result.to_string();
                            result_with_comment.push(' ');
                            result_with_comment
                                .push_str(&self.display_pse(comment, previous_indentation));
                            format!("{result_with_comment}\n")
                        } else if result.ends_with('\n') {
                            result.to_string()
                        } else {
                            format!("{result}\n")
                        }
                    } else {
                        self.to_inner_content(list, previous_indentation)
                    };
                    // if it's a top level expression, newline it
                    if previous_indentation.is_empty() && (result.ends_with(")")) {
                        result.push('\n');
                    }
                    result.push_str(&formatted);
                    prev_end_line = expr.span().end_line;
                    continue;
                }
            }
            let current = self.display_pse(expr, previous_indentation);
            let mut between = " ";
            if let Some(next) = iter.peek() {
                if !is_same_line(expr, next) || is_comment(expr) {
                    between = "\n";
                }
            } else {
                // no next expression to space out
                between = "";
            }

            prev_end_line = expr.span().end_line;

            result.push_str(&format!("{current}{between}"));
        }
        // Cache the result
        self.cache.borrow_mut().insert(key, result.clone());
        result
    }

    // (define-trait trait-name (
    //   (func1-name
    //     (arg1-type arg2-type ...)
    //     (return-type)
    //   )
    //   (func2-name (arg1-type arg2-type ...) (return-type))
    // )
    fn define_trait(&self, exprs: &[PreSymbolicExpression], previous_indentation: &str) -> String {
        let mut acc = "(define-trait ".to_string();
        let indentation = &self.settings.indentation.to_string();
        let space = format!("{indentation}{previous_indentation}");

        // name
        acc.push_str(&self.format_source_exprs(slice::from_ref(&exprs[1]), previous_indentation));
        acc.push_str(" (");
        acc.push('\n');
        acc.push_str(&space);

        // methods
        let mut iter = exprs[2].match_list().unwrap().iter().peekable();
        while let Some(expr) = iter.next() {
            let trailing = get_trailing_comment(expr, &mut iter);

            if let Some(method_list) = expr.match_list() {
                acc.push('(');

                // method name
                if let Some(method_name) = method_list.first() {
                    acc.push_str(&self.display_pse(method_name, previous_indentation));
                }

                let double_indent = format!("{space}{indentation}");

                let mut items_iter = method_list.iter().skip(1).peekable();

                while let Some(arg) = items_iter.next() {
                    if let Some(element_list) = arg.match_list() {
                        // Found either args or return type
                        acc.push('\n');
                        acc.push_str(&double_indent);
                        acc.push_str(&self.display_list(element_list, &double_indent));

                        if let Some(next_item) = items_iter.peek() {
                            if is_comment(next_item) {
                                let count =
                                    next_item.span().start_column - arg.span().end_column - 1;
                                let spaces = " ".repeat(count as usize);
                                acc.push_str(&spaces);
                                acc.push_str(&self.display_pse(next_item, previous_indentation));
                                items_iter.next();
                            }
                        }
                    } else if is_comment(arg) {
                        // standalone comments
                        acc.push('\n');
                        acc.push_str(&double_indent);
                        acc.push_str(&self.display_pse(arg, previous_indentation));
                    }
                }

                if let Some(comment) = trailing {
                    if let Some(last_item) = method_list.last() {
                        let count = comment.span().start_column - last_item.span().end_column - 1;
                        let spaces = " ".repeat(count as usize);
                        acc.push_str(&spaces);
                        acc.push_str(&self.display_pse(comment, previous_indentation));
                    }
                }

                acc.push('\n');
                acc.push_str(&space);
                acc.push(')');

                if iter.peek().is_some() {
                    acc.push('\n');
                    acc.push_str(&space);
                }
            }
        }

        acc.push('\n');
        acc.push_str("))");
        acc
    }

    fn constant(&self, exprs: &[PreSymbolicExpression], previous_indentation: &str) -> String {
        let func_type = self.display_pse(exprs.first().unwrap(), "");
        let mut acc = format!("({func_type} ");
        let mut iter = exprs[1..].iter().peekable();
        while let Some(expr) = iter.next() {
            let trailing = get_trailing_comment(expr, &mut iter);
            acc.push_str(&self.format_source_exprs(slice::from_ref(expr), previous_indentation));
            if iter.peek().is_some() {
                acc.push(' ');
            }
            if let Some(comment) = trailing {
                acc.push(' ');
                acc.push_str(&self.display_pse(comment, previous_indentation));
            }
        }
        acc.push(')');
        acc
    }

    fn format_map(&self, exprs: &[PreSymbolicExpression], previous_indentation: &str) -> String {
        let func_type = self.display_pse(exprs.first().unwrap(), "");
        let mut acc = format!("({func_type} ");
        let indentation = &self.settings.indentation.to_string();
        let space = format!("{indentation}{previous_indentation}");
        acc.push_str(&self.format_source_exprs(slice::from_ref(&exprs[1]), previous_indentation));
        let mut iter = exprs[2..].iter().peekable();
        while let Some(expr) = iter.next() {
            let trailing = get_trailing_comment(expr, &mut iter);

            acc.push('\n');
            acc.push_str(&space);
            acc.push_str(&self.format_source_exprs(slice::from_ref(expr), &space));
            if let Some(comment) = trailing {
                acc.push(' ');
                acc.push_str(&self.display_pse(comment, previous_indentation));
            }
        }
        acc.push('\n');
        acc.push_str(previous_indentation);
        acc.push(')');
        acc
    }

    // *begin* never on one line
    fn format_begin(&self, exprs: &[PreSymbolicExpression], previous_indentation: &str) -> String {
        let mut acc = "(begin".to_string();
        let indentation = &self.settings.indentation.to_string();
        let space = format!("{previous_indentation}{indentation}");

        let mut iter = exprs.get(1..).unwrap_or_default().iter().peekable();
        let mut prev_end_line = None;

        while let Some(expr) = iter.next() {
            let trailing = get_trailing_comment(expr, &mut iter);

            // Add extra newlines based on original blank lines (limit to 1 consecutive blank lines)
            push_blank_lines(&mut acc, prev_end_line, expr.span().start_line);

            // begin body
            acc.push_str(&format!(
                "\n{}{}",
                space,
                self.format_source_exprs(slice::from_ref(expr), &space)
            ));
            if let Some(comment) = trailing {
                acc.push(' ');
                acc.push_str(&self.display_pse(comment, previous_indentation));
            }

            prev_end_line = Some(expr.span().end_line);
        }
        acc.push_str(&format!("\n{previous_indentation})"));
        acc
    }

    // formats (and ..) and (or ...)
    // if given more than BOOLEAN_BREAK_LIMIT expressions it will break it onto new lines
    fn format_booleans(
        &self,
        exprs: &[PreSymbolicExpression],
        previous_indentation: &str,
    ) -> String {
        let func_type = self.display_pse(exprs.first().unwrap(), previous_indentation);
        let mut acc = format!("({func_type}");
        let indentation = &self.settings.indentation.to_string();
        let space = format!("{previous_indentation}{indentation}");
        let break_up =
            without_comments_len(&exprs[1..]) > BOOLEAN_BREAK_LIMIT || differing_lines(exprs);
        let mut iter = exprs.get(1..).unwrap_or_default().iter().peekable();
        let mut prev_end_line = None;

        if break_up {
            while let Some(expr) = iter.next() {
                let trailing = get_trailing_comment(expr, &mut iter);

                // Add extra newlines based on original blank lines (limit to 1 consecutive blank lines)
                push_blank_lines(&mut acc, prev_end_line, expr.span().start_line);

                acc.push_str(&format!(
                    "\n{}{}",
                    space,
                    self.format_source_exprs(slice::from_ref(expr), &space)
                ));
                if let Some(comment) = trailing {
                    acc.push(' ');
                    acc.push_str(&self.display_pse(comment, previous_indentation));
                }

                prev_end_line = Some(expr.span().end_line);
            }
        } else {
            while let Some(expr) = iter.next() {
                let trailing = get_trailing_comment(expr, &mut iter);
                acc.push(' ');
                acc.push_str(
                    &self.format_source_exprs(slice::from_ref(expr), previous_indentation),
                );
                if let Some(comment) = trailing {
                    acc.push(' ');
                    acc.push_str(&self.display_pse(comment, previous_indentation));
                    acc.push('\n');
                    acc.push_str(&space)
                }
            }
        }
        if break_up {
            acc.push_str(&format!("\n{previous_indentation}"));
        }
        acc.push(')');
        acc
    }

    fn format_if(&self, exprs: &[PreSymbolicExpression], previous_indentation: &str) -> String {
        let opening = exprs.first().unwrap();
        let func_type = self.display_pse(opening, previous_indentation);
        let indentation = &self.settings.indentation.to_string();
        let space = format!("{indentation}{previous_indentation}");

        let mut acc = format!("({func_type} ");
        let mut iter = exprs[1..].iter().peekable();
        let mut index = 0;
        let mut prev_end_line = None;

        while let Some(expr) = iter.next() {
            let trailing = get_trailing_comment(expr, &mut iter);

            // Add extra newlines based on original blank lines (limit to 1 consecutive blank lines)
            push_blank_lines(&mut acc, prev_end_line, expr.span().start_line);

            if index > 0 {
                acc.push('\n');
                acc.push_str(&space);
            }
            acc.push_str(&self.format_source_exprs(slice::from_ref(expr), &space));
            if let Some(comment) = trailing {
                acc.push(' ');
                acc.push_str(&self.display_pse(comment, previous_indentation));
            }

            index += 1;
            prev_end_line = Some(expr.span().end_line);
        }
        acc.push('\n');
        acc.push_str(previous_indentation);
        acc.push(')');

        acc
    }

    fn format_let(&self, exprs: &[PreSymbolicExpression], previous_indentation: &str) -> String {
        let mut acc = "(let (".to_string();
        let indentation = &self.settings.indentation.to_string();
        let space = format!("{previous_indentation}{indentation}");

        if let Some(args) = exprs[1].match_list() {
            if args.len() == 1 {
                acc.push_str(&self.format_source_exprs(slice::from_ref(&args[0]), &space));
                acc.push(')');
            } else {
                let mut iter = args.iter().peekable();
                while let Some(arg) = iter.next() {
                    let trailing = get_trailing_comment(arg, &mut iter);
                    let double_indent = format!("{space}{indentation}");
                    acc.push_str(&format!(
                        "\n{}{}",
                        double_indent,
                        self.format_source_exprs(slice::from_ref(arg), &double_indent)
                    ));
                    if let Some(comment) = trailing {
                        acc.push(' ');
                        acc.push_str(&self.display_pse(comment, previous_indentation));
                    }
                }
                // close the args paren
                acc.push_str(&format!("\n{previous_indentation}{indentation})"));
            }
        }
        // start the let body
        let mut prev_end_line = None;
        for e in exprs.get(2..).unwrap_or_default() {
            // Add extra newlines based on original blank lines (limit to 1 consecutive blank lines)
            push_blank_lines(&mut acc, prev_end_line, e.span().start_line);

            acc.push_str(&format!(
                "\n{}{}",
                space,
                self.format_source_exprs(slice::from_ref(e), &space)
            ));

            prev_end_line = Some(e.span().end_line);
        }
        acc.push_str(&format!("\n{previous_indentation})"));
        acc
    }

    // * match *
    // always multiple lines
    fn format_match(&self, exprs: &[PreSymbolicExpression], previous_indentation: &str) -> String {
        let mut acc = "(match ".to_string();
        let indentation = &self.settings.indentation.to_string();
        let space = format!("{previous_indentation}{indentation}");

        // value to match on
        acc.push_str(&self.format_source_exprs(slice::from_ref(&exprs[1]), previous_indentation));
        acc.push('\n');

        let mut iter = exprs[2..].iter().peekable();
        while let Some(branch) = iter.next() {
            let trailing = get_trailing_comment(branch, &mut iter);
            let is_binding = branch.match_list().is_none() && iter.peek().is_some();
            acc.push_str(&space);
            acc.push_str(&self.format_source_exprs(slice::from_ref(branch), &space));

            // If this is a binding pattern, add the next expression on the same line
            if is_binding {
                if let Some(expr_part) = iter.next() {
                    let expr_trailing = get_trailing_comment(expr_part, &mut iter);
                    acc.push(' ');
                    acc.push_str(&self.format_source_exprs(slice::from_ref(expr_part), &space));
                    if let Some(comment) = expr_trailing {
                        acc.push(' ');
                        acc.push_str(&self.display_pse(comment, previous_indentation));
                    }
                }
            }
            if let Some(comment) = trailing {
                acc.push(' ');
                acc.push_str(&self.display_pse(comment, previous_indentation));
            }

            if iter.peek().is_some() {
                acc.push('\n');
            }
        }
        acc.push_str(&format!("\n{previous_indentation})"));
        acc
    }

    /// Special case for match with comments in line.
    /// aligns all bindings and values
    fn match_with_comments(
        &self,
        exprs: &[PreSymbolicExpression],
        previous_indentation: &str,
    ) -> String {
        let mut acc = "(match ".to_string();
        let indentation = &self.settings.indentation.to_string();
        let space = format!("{previous_indentation}{indentation}");

        // value to match on
        acc.push_str(&self.format_source_exprs(slice::from_ref(&exprs[1]), previous_indentation));
        // branches evenly spaced

        let mut iter = exprs[2..].iter().peekable();
        while let Some(branch) = iter.next() {
            let trailing = get_trailing_comment(branch, &mut iter);
            acc.push_str(&format!(
                "\n{}{}",
                space,
                self.format_source_exprs(slice::from_ref(branch), &space)
            ));
            if let Some(comment) = trailing {
                acc.push(' ');
                acc.push_str(&self.display_pse(comment, previous_indentation));
            }
        }
        acc.push_str(&format!("\n{previous_indentation})"));
        acc
    }

    fn needs_break(
        &self,
        current_column: usize,
        acc: &str,
        next_expr: &PreSymbolicExpression,
    ) -> bool {
        let length = chars_since_last_newline(acc);
        let next_expr_len = self.display_pse(next_expr, "").len();
        (current_column + length + next_expr_len) > self.settings.max_line_length
    }

    // strictly used for display_pse. Sort of a dumbed down version of format_list
    fn display_list(&self, exprs: &[PreSymbolicExpression], previous_indentation: &str) -> String {
        let indentation = &self.settings.indentation.to_string();
        let space = format!("{previous_indentation}{indentation}");
        let mut acc = "(".to_string();

        if differing_lines(exprs) {
            acc.push('\n');
        }
        let mut iter = exprs[0..].iter().peekable();
        while let Some(item) = iter.next() {
            let trailing = get_trailing_comment(item, &mut iter);
            if differing_lines(exprs) {
                acc.push_str(&space)
            }
            let value = self.format_source_exprs(slice::from_ref(item), previous_indentation);
            let start_line = item.span().start_line;
            acc.push_str(&value.to_string());
            if let Some(comment) = trailing {
                let count = comment
                    .span()
                    .start_column
                    .saturating_sub(item.span().end_column + 1);
                let spaces = " ".repeat(count as usize);
                acc.push_str(&spaces);
                acc.push_str(&self.display_pse(comment, previous_indentation));
            }
            if let Some(next) = iter.peek() {
                if start_line != next.span().start_line {
                    acc.push('\n')
                } else {
                    acc.push(' ')
                }
            }
        }
        if differing_lines(exprs) {
            acc.push('\n');
            acc.push_str(previous_indentation);
        }
        acc.push(')');
        acc
    }
    fn format_list(&self, exprs: &[PreSymbolicExpression], previous_indentation: &str) -> String {
        let indentation = &self.settings.indentation.to_string();
        let space = format!("{previous_indentation}{indentation}");
        let mut start_index = 0;
        let mut acc = "(".to_string();
        let is_multiline = differing_lines(exprs);
        let mut first_break = true; // hack to account for accumulation before the list
                                    // used for breaking lines over max length
        if self.display_pse(&exprs[0], previous_indentation) == "list" {
            start_index = 1;
            acc.push_str("list");
            if !is_multiline && exprs.len() > 1 {
                acc.push(' ');
            }
        }

        if is_multiline {
            acc.push('\n');
        }
        let mut iter = exprs[start_index..].iter().peekable();
        while let Some(item) = iter.next() {
            let trailing = get_trailing_comment(item, &mut iter);
            if is_multiline {
                acc.push_str(&space)
            }
            let spacing = if is_multiline {
                &space
            } else {
                previous_indentation
            };
            let value = self.format_source_exprs(slice::from_ref(item), spacing);
            let start_line = item.span().start_line;
            acc.push_str(&value.to_string());

            if let Some(comment) = trailing {
                let count = comment
                    .span()
                    .start_column
                    .saturating_sub(item.span().end_column + 1);
                let spaces = " ".repeat(count as usize);
                acc.push_str(&spaces);
                acc.push_str(&self.display_pse(comment, previous_indentation));
            }
            let padding = if first_break {
                // ideally we'd be able to inspect the previously formatted accumulator value
                // TODO this is slightly wrong. We probably need a 2-pass formatter to handle these breaks better
                previous_indentation.len()
            } else {
                0
            };
            if let Some(next) = iter.peek() {
                if start_line != next.span().start_line {
                    acc.push('\n')
                } else if self.needs_break(padding, &acc, next) {
                    first_break = false;
                    acc.push('\n');
                    acc.push_str(&space);
                } else {
                    acc.push(' ')
                }
            }
        }
        if is_multiline {
            acc.push('\n');
            acc.push_str(previous_indentation);
        }
        acc.push(')');
        t(&acc).to_string()
    }

    // used for { n1: 1 } syntax
    fn format_key_value_sugar(
        &self,
        exprs: &[PreSymbolicExpression],
        previous_indentation: &str,
    ) -> String {
        let indentation = &self.settings.indentation.to_string();
        let space = format!("{previous_indentation}{indentation}");
        let over_2_kvs = without_comments_len(exprs) > 2;
        let mut acc = "{".to_string();

        // differing_lines breaks determinism but is a good way to break up
        // complex values in maps
        if over_2_kvs || differing_lines(exprs) {
            acc.push('\n');
            let mut iter = exprs.iter().peekable();
            while let Some(key) = iter.next() {
                if is_comment(key) {
                    acc.push_str(&space);
                    acc.push_str(&self.display_pse(key, previous_indentation));
                    acc.push('\n');
                    continue;
                }
                let key_str = self.format_source_exprs(slice::from_ref(key), &space);
                acc.push_str(&format!("{space}{key_str}:"));
                if let Some(value) = iter.next() {
                    if is_comment(value) {
                        acc.push('\n');
                        acc.push_str(&format!("{space}{indentation}"));
                        acc.push_str(&self.display_pse(value, &space));
                        acc.push('\n');
                        // Try to get the actual value after the comment
                        if let Some(actual_value) = iter.next() {
                            // comment implies next indent level which we don't
                            // want if this is a normal value
                            let indent = if is_comment(value) {
                                &format!("{space}{indentation}")
                            } else {
                                &space
                            };
                            let trailing = get_trailing_comment(actual_value, &mut iter);
                            let value_str =
                                self.format_source_exprs(slice::from_ref(actual_value), indent);
                            acc.push_str(indent);
                            acc.push_str(&value_str);
                            acc.push(',');

                            // Add trailing comment if present
                            if let Some(comment) = trailing {
                                acc.push(' ');
                                acc.push_str(&self.display_pse(comment, &space));
                            }
                        }
                    } else {
                        let trailing = get_trailing_comment(value, &mut iter);
                        let indent = if is_comment(value) {
                            &format!("{space}{indentation}")
                        } else {
                            &space
                        };
                        // Pass the current indentation level to nested formatting
                        let value_str = self.format_source_exprs(slice::from_ref(value), indent);
                        acc.push_str(&format!(" {value_str}"));
                        acc.push(',');

                        if let Some(comment) = trailing {
                            acc.push(' ');
                            acc.push_str(&self.display_pse(comment, previous_indentation));
                        }
                    }
                    acc.push('\n');
                }
            }
            acc.push_str(previous_indentation);
        } else {
            // for cases where we keep it on the same line with 1 k/v pair
            let fkey = self.display_pse(&exprs[0], previous_indentation);
            acc.push_str(&format!(
                " {fkey}: {} ",
                self.format_source_exprs(slice::from_ref(&exprs[1]), previous_indentation)
            ));
        }

        acc.push('}');
        acc
    }

    // used for (tuple (n1  1)) syntax
    // Note: Converted to a { a: 1 } style map
    // TODO: This should be rolled into format_key_value_sugar, but the PSE
    // structure is different so it would take some finagling
    fn format_key_value(
        &self,
        exprs: &[PreSymbolicExpression],
        previous_indentation: &str,
    ) -> String {
        let indentation = &self.settings.indentation.to_string();
        let space = format!("{previous_indentation}{indentation}");

        let mut acc = previous_indentation.to_string();
        acc.push('{');

        // for cases where we keep it on the same line with 1 k/v pair
        let multiline = exprs.len() > 1;
        if multiline {
            acc.push('\n');
            let mut iter = exprs.iter().peekable();
            while let Some(arg) = iter.next() {
                let trailing = get_trailing_comment(arg, &mut iter);
                let (key, value) = arg
                    .match_list()
                    .and_then(|list| list.split_first())
                    .unwrap();
                let fkey = self.display_pse(key, previous_indentation);

                acc.push_str(&format!(
                    "{space}{fkey}: {},",
                    self.format_source_exprs(value, previous_indentation)
                ));
                if let Some(comment) = trailing {
                    acc.push(' ');
                    acc.push_str(&self.display_pse(comment, previous_indentation));
                }
                acc.push('\n');
            }
            acc.push_str(previous_indentation);
        } else {
            // for cases where we keep it on the same line with 1 k/v pair
            let (key, value) = exprs[0]
                .match_list()
                .and_then(|list| list.split_first())
                .unwrap();
            let fkey = self.display_pse(key, previous_indentation);
            acc.push_str(&format!(
                " {fkey}: {} ",
                self.format_source_exprs(value, previous_indentation)
            ));
        }

        acc.push('}');
        acc
    }

    // This prints leaves of the PSE tree
    fn display_pse(&self, pse: &PreSymbolicExpression, previous_indentation: &str) -> String {
        let key = (
            pse as *const PreSymbolicExpression as usize,
            previous_indentation.to_string(),
        );

        let cached_result = {
            let cache_ref = self.cache.borrow();
            cache_ref.get(&key).cloned()
        };
        if let Some(result) = cached_result {
            return result;
        }
        let result = match pse.pre_expr {
            PreSymbolicExpressionType::Atom(ref value) => t(value.as_str()).to_string(),
            PreSymbolicExpressionType::AtomValue(ref value) => match value {
                clarity::vm::types::Value::Principal(c) => {
                    format!("'{c}")
                }
                // Fill in these explicitly
                _ => value.to_string(),
            },
            PreSymbolicExpressionType::List(ref items) => {
                self.display_list(items, previous_indentation)
            }
            PreSymbolicExpressionType::Tuple(ref items) => {
                self.format_key_value_sugar(items, previous_indentation)
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
        self.cache.borrow_mut().insert(key, result.clone());

        result
    }

    // * functions

    // Top level define-<function> should have a line break above and after (except on first line)
    // options always on new lines
    // Functions Always on multiple lines, even if short
    fn function(&self, exprs: &[PreSymbolicExpression]) -> String {
        let func_type = self.display_pse(exprs.first().unwrap(), "");
        let indentation = &self.settings.indentation.to_string();
        let args_indent = format!("{indentation}{indentation}");

        let mut acc = format!("({func_type} (");

        // function name and arguments
        if let Some(def) = exprs.get(1).and_then(|f| f.match_list()) {
            if let Some((name, args)) = def.split_first() {
                acc.push_str(&self.display_pse(name, ""));

                // Keep everything on one line if there's only one argument
                if args.len() == 1 {
                    acc.push(' ');
                    acc.push_str(&self.format_source_exprs(slice::from_ref(&args[0]), ""));
                    acc.push(')');
                } else {
                    let mut iter = args.iter().peekable();
                    while let Some(arg) = iter.next() {
                        let trailing = get_trailing_comment(arg, &mut iter);
                        if arg.match_list().is_some() {
                            // expr args
                            acc.push_str(&format!(
                                "\n{}{}",
                                args_indent,
                                self.format_source_exprs(slice::from_ref(arg), &args_indent)
                            ))
                        } else {
                            // atom args
                            acc.push_str(
                                &self.format_source_exprs(slice::from_ref(arg), &args_indent),
                            )
                        }
                        if let Some(comment) = trailing {
                            acc.push(' ');
                            acc.push_str(&self.display_pse(comment, ""));
                        }
                    }
                    if args.is_empty() {
                        acc.push(')');
                    } else {
                        acc.push_str(&format!("\n{indentation})"))
                    }
                }
            }
        }

        // function body expressions
        for expr in exprs.get(2..).unwrap_or_default() {
            acc.push_str(&format!(
                "\n{}{}",
                indentation,
                self.format_source_exprs(
                    slice::from_ref(expr),
                    &self.settings.indentation.to_string(),
                )
            ))
        }
        acc.push_str("\n)\n");
        acc
    }

    // This code handles the line width wrapping and happens near the bottom of the
    // traversal
    // TODO: Fix this horrible abomination
    fn to_inner_content(
        &self,
        list: &[PreSymbolicExpression],
        previous_indentation: &str,
    ) -> String {
        let mut result = String::new();
        let mut current_line_width = previous_indentation.len();
        let mut first_on_line = true;
        let mut broken_up = false;
        let indentation = self.settings.indentation.to_string();
        let base_indent = format!("{previous_indentation}{indentation}");

        // Check if this is a simple wrapper expression
        let is_simple_wrapper = list.len() == 2 && list[0].match_atom().is_some();

        // Special handling for simple wrappers to avoid unnecessary line breaks
        if is_simple_wrapper {
            let atom_name = list[0].match_atom().unwrap();
            let is_special_format = if let Some(native) = NativeFunctions::lookup_by_name(atom_name)
            {
                matches!(
                    native,
                    NativeFunctions::Let
                        | NativeFunctions::Begin
                        | NativeFunctions::Match
                        | NativeFunctions::TupleCons
                        | NativeFunctions::If
                )
            } else {
                false
            };

            if !is_special_format {
                // For simple wrappers like (ok ...), format compactly
                let fn_name =
                    self.format_source_exprs(slice::from_ref(&list[0]), previous_indentation);
                let arg = self.format_source_exprs(slice::from_ref(&list[1]), previous_indentation);

                return format!("({} {})", fn_name.trim(), arg.trim());
            }
        }
        // TODO: this should ignore comment length
        for (i, expr) in list.iter().enumerate() {
            let indented = if first_on_line {
                &base_indent
            } else {
                previous_indentation
            };
            let formatted = self.format_source_exprs(slice::from_ref(expr), indented);
            let trimmed = t(&formatted);

            let expr_width = trimmed.len();

            if !first_on_line {
                // Don't break before an opening brace of a map
                let is_map_opening = trimmed.starts_with("{");

                // Check if the current expression is on a different line than the previous one
                // in the original source code
                let on_different_line_in_source =
                    // i - 1 index is fine here because we're withing !first_on_line
                    is_comment(expr) && list[i - 1].span().start_line != expr.span().start_line;

                // Add line break if comment was on different lines in source
                // or if the line would be too long
                if on_different_line_in_source
                    || (!is_map_opening
                        && (current_line_width + expr_width + 1 > self.settings.max_line_length))
                {
                    result.push('\n');
                    result.push_str(&base_indent);
                    current_line_width = base_indent.len() + indentation.len();
                    broken_up = true;
                } else {
                    result.push(' ');
                    current_line_width += 1;
                }
            }

            if broken_up {
                // reformat with increased indent in the case we broke up the code on max width
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
            // Find the last line without collecting all lines into a vector
            let last_line = result
                .rfind('\n')
                .map(|pos| &result[pos + 1..])
                .unwrap_or(&result);
            let trimmed = last_line.trim();
            !(trimmed == ")" || trimmed == "}")
        };
        let newlined = format!("\n{previous_indentation})");
        format!("({}{}", result, if break_lines { &newlined } else { ")" })
    }
}

fn is_comment(pse: &PreSymbolicExpression) -> bool {
    matches!(pse.pre_expr, PreSymbolicExpressionType::Comment(_))
}

fn without_comments_len(exprs: &[PreSymbolicExpression]) -> usize {
    exprs.iter().filter(|expr| !is_comment(expr)).count()
}
// if the exprs are already broken onto different lines, return true
fn differing_lines(exprs: &[PreSymbolicExpression]) -> bool {
    !exprs
        .windows(2)
        .all(|window| window[0].span().start_line == window[1].span().start_line)
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

fn comment_piece(text: &str, pse: &PreSymbolicExpression) -> String {
    let (comment_part, rest) = text
        .find(|c| c != ';')
        .map_or((text, ""), |idx| (&text[..idx], &text[idx..]));
    let comment_length = text.len() as u32;
    let space_count = pse.span().end_column - comment_length - pse.span().start_column - 1; // 1 to account for span starting at 1 instead of 0
    let spaces = if space_count > 0 {
        " ".repeat(space_count as usize)
    } else {
        // remove the spaces if the comment has its own
        if rest.starts_with(' ') { "" } else { " " }.to_string()
    };
    format!(";;{comment_part}{spaces}{rest}")
}

fn chars_since_last_newline(acc: &str) -> usize {
    if let Some(last_newline_pos) = acc.rfind('\n') {
        acc.len() - last_newline_pos - 1
    } else {
        acc.len()
    }
}

// Helper to insert at most one blank line if there are blank lines between two expressions
fn push_blank_lines(acc: &mut String, prev_end_line: Option<u32>, curr_start_line: u32) {
    if let Some(prev_end) = prev_end_line {
        if curr_start_line > prev_end {
            let blank_lines = curr_start_line.saturating_sub(prev_end + 1);
            let extra_newlines = std::cmp::min(blank_lines, 1);
            for _ in 0..extra_newlines {
                acc.push('\n');
            }
        }
    }
}

#[cfg(test)]
mod tests_formatter {
    #[allow(unused_imports)]
    use std::assert_eq;

    use indoc::indoc;

    use super::{ClarityFormatter, Settings};
    use crate::formatter::Indentation;
    #[macro_export]
    macro_rules! assert_eq {
        ($($arg:tt)*) => {
            pretty_assertions::assert_eq!($($arg)*)
        }
    }

    fn format_with_default(source: &str) -> String {
        let formatter = ClarityFormatter::new(Settings::default());
        formatter.format_section(source).unwrap()
    }

    fn format_with(source: &str, settings: Settings) -> String {
        let formatter = ClarityFormatter::new(settings);
        formatter.format_section(source).unwrap()
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
        let src = ";; @format-ignore\n(    begin ( ok true))\n";
        let result = format_with(&String::from(src), Settings::new(Indentation::Space(4), 80));
        assert_eq!(src, result);

        let src = ";; @format-ignore\n(list\n  u64\n  u64 u64\n)\n";
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
              buckets: (list p-func-b1 p-func-b2 p-func-b3 p-func-b4 p-func-b5 p-func-b6 p-func-b7
                p-func-b8 p-func-b9 p-func-b10 p-func-b11 p-func-b12 p-func-b13 p-func-b14
                p-func-b15 p-func-b16),
              something: u1,
            }"#
        );
        let result = format_with_default(src);
        assert_eq!(expected, result);
    }

    #[test]
    fn format_ast_without_source() {
        let src = "(define-private (noop) (begin (+ 1 2) (ok true)))";
        let ast = clarity::vm::ast::parser::v2::parse(src).unwrap();
        let formatter = ClarityFormatter::new(Settings::default());
        let expected = format_with_default(src);
        let result = formatter.format_ast(&ast);
        assert_eq!(result, expected);
    }

    #[test]
    fn format_ast_without_source_handle_indentation() {
        let src = "  (begin (+ 1 2) (ok true))";
        let ast = clarity::vm::ast::parser::v2::parse(src).unwrap();
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

        let src = ";; @format-ignore\n(+ u1 u1)\n;; @format-ignore\n(+ u1 u1)\n";
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
}
