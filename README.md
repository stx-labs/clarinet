# Clarinet

Clarinet is the fastest way to build, test, and deploy smart contracts on the Stacks blockchain. It
gives you a local devnet, REPL, testing framework, and debugging tools to ship high-quality Clarity
code with confidence.

- 🧑‍💻 **Leverage a powerful CLI** Create new projects, manage your smart contracts and their
  dependencies using clarinet requirements, and interact with your code through the built-in REPL.

- 🧪 **Write unit tests with the SDK** Use the Clarinet SDK to write unit tests in a familiar JS
  environment and validate contract behavior.

- 🛠️ **Run a private blockchain environment** Spin up a local devnet with nodes, miners, and APIs so
  you can test and integrate your code.

- 🔍 **VSCode extension**: Linter, step by step debugger, helps writing smart contracts
  (autocompletion, documentation etc)

---

### Documentation

- [Clarinet CLI](https://docs.hiro.so/stacks/clarinet)
- [Clarinet JS SDK and testing framework](https://docs.hiro.so/stacks/clarinet-js-sdk)

---

### Quickstart

```bash
# Install Clarinet
brew install clarinet
```

> To check out more installation methods, click
> [here](https://docs.hiro.so/stacks/clarinet#installation)

```bash
# Create a new project
clarinet new hello-world
cd hello-world
```

```bash
# Create a new contract
clarinet contract new counter
```

```clarity
;; Add this to the `contracts/counter.clar`

(define-map counters principal uint)

(define-public (count-up)
  (ok (map-set counters tx-sender (+ (get-count tx-sender) u1)))
)

(define-read-only (get-count (who principal))
  (default-to u0 (map-get? counters who))
)
```

```bash
# Then test it out

# Check the contract
clarinet check

# Launch the REPL
clarinet console
```

```bash
# Inside the console
(contract-call? .counter count-up)
(contract-call? .counter get-count tx-sender)
```

### Contributing

Contributions are welcome and appreciated. The following sections provide information on how you can
contribute to Clarinet.

#### Prerequisites

Before contributing to Clarinet, you need the following tools.  
Although it will work with older versions, the team always tries to keep up with the latest versions
of Rust and Node.js (LTS) tooling.

- Rust (>=1.89.0)
- Cargo (>=1.89.0)
- Node (>=v24.4.1)
- NPM (>=11.5.2)

#### Guide

This repo follows the [Conventional Commit](https://www.conventionalcommits.org/en/v1.0.0/#summary)
specification when writing commit messages.

**Note:** These conventions are helpful for any commit message, but all PR end up being merged with
"squash and merge", giving another chance to refine the commit messages.

To start contributing, fork this repo and open a new branch:

1. Fork this repo and clone the fork locally.
1. Create a new branch
   ```bash
   git checkout -b <my-branch>
   ```

##### Contributing to the CLI

1. After making your changes, ensure the following:
   - `cargo build` runs successfully.
   - `cargo tst` runs successfully.
     - `cargo tst` is an alias declared in `./cargo/config`, it runs
       [cargo-nextest](https://crates.io/crates/cargo-nextest)
   - You have formatted your code with `cargo fmt-stacks`
   - All functional tests in the `examples` directory pass.
     ```bash
     for testdir in $(ls examples); do
         pushd examples/${testdir}
             ../../target/debug/clarinet check .
         popd
     done
     ```
1. Submit a pull request against the `main` branch for review.

##### Contributing to the JS SDK

For VSCode users, we recommend opening the following workspace
`./components/clarinet-sdk/clarinet-sdk.code-workspace`. It's set up so that rust-analyzer uses the
Wasm target.

The SDK is divided between the Rust lib compiled to Wasm `./components/clarinet-sdk-wasm` and a TS
wrapper around it: `./components/clarinet-sdk`.

1. Compile the Wasm package with `npm run build:sdk-wasm`
1. Compile the SDK with `npm run build:sdk`
1. Test with `npm test`

Learn more in the [SDK Readme.md](./components/clarinet-sdk/README.md).

### Code of Conduct

Please read our [Code of Conduct](.github/CONTRIBUTING.md) since we expect
project participants to adhere to it.

## Community

Join our community and stay connected with the latest updates and discussions:

- [Join our Discord community chat](https://discord.com/invite/pPwMzMx9k8) to engage with other
  users, ask questions, and participate in discussions.
- [Visit stackslabs.com/](https://stackslabs.com) for updates.
- Follow [Stacks Labs on X.](https://x.com/stackslabs)
