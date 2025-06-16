use indoc::indoc;
use ts_to_clar::transpile;

#[test]
fn test_counter() {
    let src = indoc! {
        r#"const count = new DataVar<Uint>(0);

        function printCount() {
            print(count.get());
        }

        function getCount() {
            print(count.get());
            return count.get();
        }

        function increment() {
            return count.set(count.get() + 1);
        }

        export default { readOnly: { getCount }, public: { increment } } satisfies Contract
        "#
    };
    let clarity_code = transpile("counter.clar.ts", src).unwrap();

    pretty_assertions::assert_eq!(
        clarity_code,
        indoc! {
            r#"(define-data-var count uint u0)
            (define-private (print-count)
              (print (var-get count))
            )
            (define-read-only (get-count)
              (begin
                (print (var-get count))
                (var-get count)
              )
            )
            (define-public (increment)
              (var-set count (+ (var-get count) u1))
            )
            "#
        }
    );
}
