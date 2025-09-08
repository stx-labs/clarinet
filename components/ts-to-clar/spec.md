# TypeScript to Clarity transpiler specification

The transpiler allows to write smart contracts for the Stacks blockchain using TypeScript.

TS smart contracts use the `.clar.ts` extension. They are valid TypeScript files that use a subset
of TypeScript and primitive Clarity types along with a Clarity standard library.

A TS smart contracts can not be executed as such, it must be transpiled to Clarity. The reason
behind that is that the standard library and the runtime aren't implemented in TS.

The transpiler expects valid `.clar.ts` that can produce valid Clarity smart contract. The validity
ofv the source code is checked with TypeScript `tsc` and custom linting rules that can be
implemented in ESLint or oxlint - that now support type aware rules with tsgo!

## Primitive types

```ts
export type ClarityValue =
  | Int
  | Uint
  | Bool
  | Principal
  | StringUtf8
  | StringAscii
  | ClBuffer
  | ClarityValue[] // list
  | { [key: string]: ClarityValue } // tuple
  | Optional<ClarityValue>
  | ClOk<ClarityValue, ClarityValue>
  | ClError<ClarityValue, ClarityValue>;
```

See the
[`clarity.d.ts` file](http://github.com/hirosystems/ts-transpiler-case-study/blob/counter/types/clarity.d.ts).

Currently, there are some helpers available globally such as `ok()` and `err()` to produce Clarity
response. They could be moved to the std lib instead of being defined globally (tbd).

## Features

### Top level declarations

#### Constants

Top level constants are declared with the `const` keyword. The type annotations is currently
required.

```ts
const MAX_COUNT: Uint: 1;
const ERR_NEGATIVE_COUNT: ClError<never, Uint> = err(4002);
```

```clar
(define-constant MAX_COUNT u1)
(define-constant ERR_NEGATIVE_COUNT (err u4002))
```

#### DataVars

Clarity data-vars are defined with the `const` keyword along with `new DataVar<T extends Clarity>`
constructor.

```ts
const totalCount = new DataVar<Uint>(0);
```

```clar
(define-data-var totalCount uint u0)
```

#### DataMaps

Clarity data-maps are defined with the `const` keyword along with
`new DataMap<K extends Clarity, V extends Clarity>` constructor.

```ts
const users = new DataMap<Principal, Uint>();
```

```clar
(define-map users principal uint)
```

#### Private Functions

Functions are regular TypeScript function. Functions are private by default.

```ts
function add(a: Uint, b: Uint) {
  return a + b;
}
```

```clar
(define-private (add (a uint) (b uint))
  (+ a b)
)
```

#### Read-Only and Public Functions

Functions are made public (and read-only) by using TS `exports`. The export expects to satisfies a
`Contract` struct that exposes `readOnly` and `public` functions.

```ts
function add(a: Uint, b: Uint) {
  return a + b;
}
export default {
  readOnly: { add },
  public: {},
} satisfies Contract;
```

```clar
(define-read-only (add (a uint) (b uint))
  (+ a b)
)
```

```ts
function add(a: Uint, b: Uint) {
  return ok(a + b);
}

export default {
  readOnly: {},
  public: { add },
} satisfies Contract;
```

```clar
(define-public (add (a uint) (b uint))
  (+ a b)
)
```

#### FTs and NFTs

todo

### Clarity Standard Library

In Clarity, everything is a function, including functions like `define-private` or `if`. Which is
not the case the in TypeScript. The goal if of the transpiler is to determine how Clarity feature
should best be represented in TypeScript. So that `(define-private)` is `function () {}` and `(if)`
is a ternary expression.

But most Clarity functions are simple function calls that access globally available features of the
languages. For exemple, `(print)` let's you emit an arbitrary event.

For such functions, the `clarity-std.d.ts` types are implemented and can be used like so:

````ts
import { print } from "clarity"; // actual lib name tbd (eg: `@stacks/clarity-std`)
function printN(n: Int) {
  print("The number is: ");
  print(n);
``

```clar
(define-private (print-n (n int))
  (begin
    (print "The number is: ")
    (print n)
  )
)
````

Or:

```ts
import { getStacksBlockInfo } from "clarity";

function getStacksBlockTime(height: Uint) {
  return getStacksBlockInfo("time", height);
}
```

```clar
(define-private (get-stacks-block-time (height uint))
  (get-stacks-block-info? time height)
)
```

### Working with DataVars

A data-var is defined like so:

```ts
const count = new DataVar<Uint>(0);
```

The `DataVar` interface has to methods, `get` and `set` that map to `(var-get)` and `(var-set)`.

```ts
const count = new DataVar<Uint>(0);
function increment() {
  ok(count.set(count.get() + 1));
}
```

```clar
(define-data-var count uint u0)
(define-private (increment)
  (ok (var-set count (+ (var-get count) u1)))
)
```

Note that the transpiler has some level of type inference, here the `1` is properly transformed into
`u1`. Using TypeScript `as` is also possible.

### Working with DataMaps

A data-map is defined like so:

```ts
const counts = new DataMap<Principal, Uint>();
```

It has the following methods: `get(<key>)`, `insert(<key>, <value>)`, `set(<key>, <value>)`,
`delete(<key>)`.

```ts
const counts = new DataMap<Principal, Uint>();
function getMyCount() {
  return counts.get(txSender).defaultTo(0);
}
function increment() {
  return ok(counts.set(txSender, getMyCount() + 1));
}
```

```clar
(define-map counts
  principal
  uint
)
(define-read-only (get-my-count)
  (default-to u0 (map-get? counts tx-sender))
)
(define-public (increment)
  (ok (map-set counts tx-sender (+ (get-my-count) u1)))
)
```

### Conditional statements

#### TypeScript ternaries

The Clarity `if` functions can be represented as a TypeScript ternary statement:

```ts
function evenOrOdd(n: Uint) {
  return n % 2 === 0 ? "even" : "odd";
}
```

```clar
(define-private (even-or-odd (n uint))
  (if (is-eq (mod n u2) u0)
    "even"
    "odd"
  )
)
```

#### Early returns

The early return pattern is transpiled to Clarity's `asserts!`.  
Caveat: it currently only works with one statement in the `if`, but will later be handled with
`(begin ...)`

```ts
const count = new DataVar<Uint>(0);
function decrement() {
  if (count.get() == 1) {
    return err(4001);
  }
  return ok(count.set(count.get() - 1));
}
```

```clar
(define-data-var count uint u0)
(define-public (decrement)
  (begin
    (asserts! (not (is-eq (var-get count) u1)) (err 4001))
    (ok (var-set count (- (var-get count) u1)))
  )
)
```
