#### Clarity Types

### Type inferences

- [x] Basic inference for `Int` and `Uint`
- [ ] Basic infernce for `StringAscii` and `StringUtf8`
- [ ] Robust type inference

#### Data stores

- [x] data-var
- [ ] data-map

#### Assets and Tokens

- [ ] FTs
- [ ] NFTs

#### Clarity STD lib

- [ ] Import and use Clarity std lib  
       _Package name tbd_

```ts
import * as std from "clarity-std";
import { print } from "clarity-std";
std.print("hello");
print("hello");
```

#### Global Keywords

- [ ] `burn-block-height`, `stacks-block-height`, ...

- [ ] Store return types of std function to help with inference

#### Local bindings

- [x] Local bindings
  ```ts
  const myCount1: Int = 1,
    myCount2: Uint = 2;
  ```
  ```clar
  (let ((my-count1 1) (my-count2 u2)) ...)
  ```
- [ ] Flatten consecutive bindings  
      _It currently results in nested `let`s_
  ```ts
  const myCount1: Int = 1;
  const myCount2: Uint = 2;
  ```
  ```clar
  (let ((my-count1 1) (my-count2 u2)) ...)
  ```

#### Binary operators

- [x] Math operator `+` `-` `*` `%` `**`
- [x] Comparisons `>` `<` `>=` `<= `
- [ ] Flatten variadic operators

```ts
1 + 2 + 3;
```

```clar
(+ 1 2 3)
```

#### Conditions and execution flow

- [x] Ternary

```ts
n % 2 === 0 ? "even" : "odd";
```

```clar
(is-eq (mod n 2) 0) "even" "odd")
```

- [ ] `if`
- [ ] early return

#### Interactions between contracts

- [ ] `satisfies Trait`

```ts
import NftTrait from "SP3F...sip-010-trait"
export default {} satisfies NftTrait satisfit Contract
```

```clar
(impl-trait 'SP3F...sip-010-trait)
```

#### Advances features

- [ ] Type aliasing
