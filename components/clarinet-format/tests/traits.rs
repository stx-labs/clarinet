mod common;

use clarinet_format::formatter::{Indentation, Settings};
use clarity::vm::representations::{PreSymbolicExpression, PreSymbolicExpressionType};
use common::{check_entry_points, parse_source};
use indoc::indoc;

#[derive(Debug, Default, PartialEq)]
struct Contents {
    syntax: Vec<String>,
    comments: Vec<String>,
}

impl Contents {
    fn collect(&mut self, expressions: &[PreSymbolicExpression]) {
        for expression in expressions {
            match &expression.pre_expr {
                PreSymbolicExpressionType::Comment(text) => self.comments.push(text.clone()),
                PreSymbolicExpressionType::List(children) => {
                    self.syntax.push("(".into());
                    self.collect(children);
                    self.syntax.push(")".into());
                }
                PreSymbolicExpressionType::Tuple(children) => {
                    self.syntax.push("{".into());
                    self.collect(children);
                    self.syntax.push("}".into());
                }
                leaf => self.syntax.push(format!("{leaf:?}")),
            }
        }
    }
}

fn assert_preserved(source: &str, settings: Settings, expected: Option<&str>) {
    let mut original = Contents::default();
    original.collect(&parse_source(source));
    check_entry_points(source, settings, |mode, formatted, reparsed| {
        if let Some(expected) = expected {
            assert_eq!(formatted.trim_end(), expected, "{mode}: {source}");
        }
        let mut actual = Contents::default();
        actual.collect(reparsed);
        assert_eq!(actual, original, "{mode}: {source}\nformatted: {formatted}");
    });
}

#[test]
fn comments_do_not_change_trait_header_positions() {
    assert_preserved(
        "(define-trait t ;; name\n ((f () (response uint uint))))",
        Settings::default(),
        Some(indoc! {"
            (define-trait t ;; name
              (
                (f
                  ()
                  (response uint uint)
                )
              )
            )"}),
    );
    assert_preserved(
        "(define-trait ;; definition\n t ())",
        Settings::default(),
        Some(indoc! {"
            (define-trait ;; definition
              t ()
            )"}),
    );
}

#[test]
fn comments_before_the_trait_close_are_preserved() {
    for (source, expected) in [
        (
            "(define-trait t () ;; empty\n)",
            "(define-trait t () ;; empty\n)",
        ),
        (
            "(define-trait t ((f () (response uint uint))) ;; methods\n)",
            indoc! {"
                (define-trait t (
                  (f
                    ()
                    (response uint uint)
                  )
                ) ;; methods
                )"},
        ),
        (
            indoc! {"
                (define-trait t ()
                  ;; first
                  ;; second
                )"},
            indoc! {"
                (define-trait t ()
                  ;; first
                  ;; second
                )"},
        ),
    ] {
        assert_preserved(source, Settings::default(), Some(expected));
    }
}

#[test]
fn comments_before_method_names_preserve_the_names() {
    assert_preserved(
        "(define-trait t ((;; name\n f () (response uint uint))))",
        Settings::default(),
        Some(indoc! {"
            (define-trait t (
              (
                ;; name
                f
                ()
                (response uint uint)
              )
            ))"}),
    );
}

#[test]
fn trait_method_comments_are_preserved() {
    let expected = indoc! {"
        (define-trait t (
          ;; before
          ;; also before
          (first
            (uint)
            (response uint uint)
          ) ;; trailing
          ;; between
          (second
            ()
            (response bool uint)
          )
          ;; after
        ))"};
    assert_preserved(expected, Settings::default(), Some(expected));
    let comments_only = "(define-trait t (\n  ;; only comment\n))";
    assert_preserved(comments_only, Settings::default(), Some(comments_only));
}

#[test]
fn multiline_trait_headers_indent_wrapped_signatures() {
    assert_preserved(
        "(define-trait t ;; methods\n ((transfer (uint principal principal (optional (buff 34)) (optional principal) (optional principal)) (response bool uint))))",
        Settings::default(),
        Some(indoc! {"
            (define-trait t ;; methods
              (
                (transfer
                  (
                    uint
                    principal
                    principal
                    (optional (buff 34))
                    (optional principal)
                    (optional principal)
                  )
                  (response bool uint)
                )
              )
            )"}),
    );
}

#[test]
fn malformed_trait_definitions_keep_their_source() {
    // Without source to copy, the AST entry point falls back to plain list syntax.
    for (source, from_ast) in [
        ("(define-trait)", "(define-trait)"),
        ("(define-trait  t)", "(define-trait t)"),
        ("(define-trait t  uint)", "(define-trait t uint)"),
        ("(define-trait t ()  extra)", "(define-trait t () extra)"),
        (
            "(define-trait t ;; name\n uint ;; tail\n)",
            "(define-trait t ;; name\nuint ;; tail\n)",
        ),
    ] {
        check_entry_points(source, Settings::default(), |mode, formatted, _| {
            let expected = if mode == "ast" { from_ast } else { source };
            assert_eq!(formatted.trim_end(), expected, "{mode}: {source}");
        });
    }
}

#[test]
fn unexpected_trait_components_are_kept() {
    for (source, expected) in [
        (
            "(define-trait t (unexpected))",
            indoc! {"
                (define-trait t (
                  unexpected
                ))"},
        ),
        (
            "(define-trait t (() (f uint (response uint uint) extra)))",
            indoc! {"
                (define-trait t (
                  ()
                  (f
                    uint
                    (response uint uint)
                    extra
                  )
                ))"},
        ),
        (
            "(define-trait t ((;; only comment\n)))",
            indoc! {"
                (define-trait t (
                  (
                    ;; only comment
                  )
                ))"},
        ),
    ] {
        assert_preserved(source, Settings::default(), Some(expected));
    }
}

#[test]
fn unexpected_tuple_in_trait_signature_preserves_malformed_nested_calls() {
    assert_preserved(
        "(define-trait t ((f () (response uint uint) {x: (let)})))",
        Settings::default(),
        None,
    );
}

#[test]
fn tuple_as_trait_method_preserves_malformed_nested_calls() {
    assert_preserved("(define-trait t ({x: (let)}))", Settings::default(), None);
}

#[test]
fn tuple_as_trait_parameters_preserves_malformed_nested_calls() {
    assert_preserved(
        "(define-trait t ((f {x: (let)} (response uint uint))))",
        Settings::default(),
        None,
    );
}

#[test]
fn tuple_as_trait_return_type_preserves_malformed_nested_calls() {
    assert_preserved(
        "(define-trait t ((f () {x: (let)})))",
        Settings::default(),
        None,
    );
}

#[test]
fn malformed_trait_with_extra_tuple_preserves_malformed_nested_calls() {
    assert_preserved("(define-trait t () {x: (let)})", Settings::default(), None);
}

#[test]
fn malformed_trait_fallback_preserves_nested_tuple_comments() {
    assert_preserved(
        indoc! {"
            (define-trait t () {
              ;; before key
              x:
                ;; before value
                (let {y: (match)}), ;; after value
              z: {nested: (define-map)}
              ;; after fields
            })"},
        Settings::default(),
        None,
    );
}

#[test]
fn comments_in_trait_gaps_preserve_structure_and_order() {
    // Every subset of the header, method-list, and method-signature gaps. Two
    // comments per gap catch consumers that accidentally handle only the first.
    let tokens = [
        "(",
        "define-trait",
        "t",
        "(",
        "(",
        "f",
        "(",
        "uint",
        ")",
        "(",
        "response",
        "uint",
        "uint",
        ")",
        ")",
        ")",
        ")",
    ];
    let gaps = [2, 3, 4, 5, 6, 9, 14, 15, 16];
    for mask in 0..(1 << gaps.len()) {
        for standalone in [false, true] {
            let mut source = String::new();
            for (i, token) in tokens.iter().enumerate() {
                if i > 0 {
                    source.push(' ');
                }
                if gaps
                    .iter()
                    .enumerate()
                    .any(|(bit, gap)| *gap == i && mask & (1 << bit) != 0)
                {
                    if standalone {
                        source.push('\n');
                    }
                    source.push_str(&format!(";; gap-{i}\n;; more-{i}\n"));
                }
                source.push_str(token);
            }
            for settings in [
                Settings::default(),
                Settings::new(Indentation::Space(4), 20),
                Settings::new(Indentation::Tab, 30),
            ] {
                assert_preserved(&source, settings, None);
            }
        }
    }
}
