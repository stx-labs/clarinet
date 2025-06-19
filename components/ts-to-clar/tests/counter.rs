use indoc::indoc;
use ts_to_clar::transpile;

#[test]
fn test_counter() {
    let src = std::fs::read_to_string("tests/fixtures/contracts/counter.clar.ts").unwrap();
    let clarity_code = transpile("counter.clar.ts", &src).unwrap();

    pretty_assertions::assert_eq!(
        clarity_code,
        indoc! {
            r#"(define-data-var count uint u0)
            (define-private (print-count)
              (print (var-get count))
            )
            (define-read-only (get-count)
              (begin
                (print-count)
                (var-get count)
              )
            )
            (define-public (increment)
              (ok (var-set count (+ (var-get count) u1)))
            )
            (define-public (add (n uint))
              (let ((new-count (+ (var-get count) n)))
                (print new-count)
                (ok new-count)
              )
            )
            "#
        }
    );
}
