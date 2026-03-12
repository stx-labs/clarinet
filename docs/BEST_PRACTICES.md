# Clarity Best Practices

A guide to writing secure, efficient, and maintainable Clarity smart contracts using Clarinet.

## Security Best Practices

### 1. Always Validate Inputs

Never trust user input. Validate all function arguments before processing.

```clarity
;; Bad: No input validation
(define-public (transfer (amount uint) (recipient principal))
  (stx-transfer? amount tx-sender recipient))

;; Good: Validate inputs
(define-public (transfer (amount uint) (recipient principal))
  (begin
    (asserts! (> amount u0) (err u1))
    (asserts! (not (is-eq recipient tx-sender)) (err u2))
    (stx-transfer? amount tx-sender recipient)))
```

### 2. Use `asserts!` for Early Returns

Fail fast with clear error codes.

```clarity
(define-public (withdraw (amount uint))
  (let ((balance (get-balance tx-sender)))
    (asserts! (>= balance amount) (err u100)) ;; ERR_INSUFFICIENT_BALANCE
    (asserts! (> amount u0) (err u101))       ;; ERR_ZERO_AMOUNT
    ;; ... rest of logic
  ))
```

### 3. Define Error Constants

Make errors readable and maintainable.

```clarity
;; Define error constants at the top of your contract
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_NOT_FOUND (err u101))
(define-constant ERR_ALREADY_EXISTS (err u102))
(define-constant ERR_INSUFFICIENT_BALANCE (err u103))

(define-public (admin-action)
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (ok true)))
```

### 4. Protect Admin Functions

Always verify authorization for sensitive operations.

```clarity
(define-constant CONTRACT_OWNER tx-sender)

(define-public (set-price (new-price uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (var-set price new-price)
    (ok true)))
```

## Gas Optimization

### 1. Use `map-get?` Over `map-get`

The `map-get?` function is more efficient and returns `none` for missing keys.

```clarity
;; Preferred
(define-read-only (get-user-data (user principal))
  (map-get? users { address: user }))
```

### 2. Minimize Storage Operations

Batch updates when possible.

```clarity
;; Inefficient: Multiple map writes
(map-set data { key: "a" } value-a)
(map-set data { key: "b" } value-b)
(map-set data { key: "c" } value-c)

;; Better: Use a composite key or batch structure
(map-set batch-data 
  { batch-id: u1 } 
  { a: value-a, b: value-b, c: value-c })
```

### 3. Use `default-to` for Safe Defaults

Avoid separate `is-some` checks.

```clarity
;; Verbose
(let ((maybe-balance (map-get? balances { user: tx-sender })))
  (if (is-some maybe-balance)
    (unwrap-panic maybe-balance)
    u0))

;; Clean
(default-to u0 (map-get? balances { user: tx-sender }))
```

## Testing Best Practices

### 1. Test All Paths

Cover success cases, error cases, and edge cases.

```typescript
// test/counter.test.ts
import { describe, it, expect } from "vitest";
import { Cl } from "@stacks/transactions";

describe("counter contract", () => {
  it("increments counter for new user", () => {
    const result = simnet.callPublicFn("counter", "count-up", [], wallet1);
    expect(result.result).toBeOk(Cl.bool(true));
  });

  it("returns zero for user with no count", () => {
    const result = simnet.callReadOnlyFn(
      "counter", 
      "get-count", 
      [Cl.principal(wallet2)],
      wallet1
    );
    expect(result.result).toBeUint(0);
  });
});
```

### 2. Test Authorization

Verify that protected functions reject unauthorized callers.

```typescript
it("rejects non-owner from admin function", () => {
  const result = simnet.callPublicFn(
    "mycontract",
    "admin-action",
    [],
    nonOwnerWallet
  );
  expect(result.result).toBeErr(Cl.uint(100)); // ERR_UNAUTHORIZED
});
```

### 3. Use Descriptive Test Names

Test names should describe the expected behavior.

```typescript
// Bad
it("test1", () => { ... });

// Good
it("should transfer tokens when sender has sufficient balance", () => { ... });
it("should reject transfer when recipient is sender", () => { ... });
```

## Common Patterns

### Ownership Pattern

```clarity
(define-data-var contract-owner principal tx-sender)

(define-public (transfer-ownership (new-owner principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_UNAUTHORIZED)
    (var-set contract-owner new-owner)
    (ok true)))

(define-read-only (get-owner)
  (ok (var-get contract-owner)))
```

### Pausable Pattern

```clarity
(define-data-var paused bool false)

(define-public (pause)
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (var-set paused true)
    (ok true)))

(define-public (unpause)
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (var-set paused false)
    (ok true)))

(define-public (protected-action)
  (begin
    (asserts! (not (var-get paused)) (err u200)) ;; ERR_PAUSED
    ;; ... action logic
    (ok true)))
```

### Counter/Nonce Pattern

```clarity
(define-data-var nonce uint u0)

(define-private (get-and-increment-nonce)
  (let ((current (var-get nonce)))
    (var-set nonce (+ current u1))
    current))
```

## Clarinet Commands Reference

```bash
# Project setup
clarinet new my-project        # Create new project
clarinet contract new counter  # Add new contract
clarinet requirements add ...  # Add dependencies

# Development
clarinet check                 # Validate all contracts
clarinet console               # Launch REPL
clarinet test                  # Run tests

# Deployment
clarinet devnet start          # Start local devnet
clarinet deployments generate  # Generate deployment plan
clarinet deployments apply     # Deploy contracts
```

## Resources

- [Clarity Language Reference](https://docs.stacks.co/clarity/language-overview)
- [Clarinet Documentation](https://docs.stacks.co/clarinet)
- [SIP Standards](https://github.com/stacksgov/sips)
