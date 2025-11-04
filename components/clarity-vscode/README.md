# Clarity for Visual Studio Code

Clarity is a **decidable** Smart Contract language that optimizes for predictability and security. It's used on the Stacks blockchain to build decentralized applications on Bitcoin.

This VS Code extension brings essential features to write safe and clean Clarity code: auto-completion, documentation, go-to definition, linting, safety checks, debugger and more.

![screenshot](https://raw.githubusercontent.com/stx-labs/clarinet/refs/heads/main/components/clarity-vscode/docs/images/screenshot.png)

## Clarity for Visual Studio Installation

You can install the latest release of the extension directly from VS Code or from the [marketplace](https://marketplace.visualstudio.com/items?itemName=stackslabs.clarity-lsp).

## Features

### Documentation on hover

Access all of Clarity documentation right in your editor by hovering functions and keywords.

![documentation screenshot](https://raw.githubusercontent.com/stx-labs/clarinet/refs/heads/main/components/clarity-vscode/docs/images/documentation.png)

### Auto Complete Functions

This feature enables you to start typing a function name, and then have the editor automatically suggest auto-completion with the documentation related to the suggestion.  
When you select a function, the extension adds the necessary parentheses around it and puts placeholders in the arguments of the function.

![autocomplete gif](https://raw.githubusercontent.com/stx-labs/clarinet/refs/heads/main/components/clarity-vscode/docs/images/autocomplete.gif)

### Go-to definition

Easily find functions, constants and variables declarations in the same contract or in contract calls.

![go-to definition screenshot](https://raw.githubusercontent.com/stx-labs/clarinet/refs/heads/main/components/clarity-vscode/docs/images/go-to-definition.png)

### Resolve contract-call targeting local contracts

The extension auto-completes local contract calls as well.

![multiple error support](https://raw.githubusercontent.com/stx-labs/clarinet/refs/heads/main/components/clarity-vscode/docs/images/multicontract.gif)

### Check Contract on Save and Display Errors Inline

When a contract is opened or saved, the extension will notify you if errors are found (syntax, unknown keyword, etc), or warnings (such as unsafe code). This helps you to ensure that you write safe and clean code.

![display errors gif](https://raw.githubusercontent.com/stx-labs/clarinet/refs/heads/main/components/clarity-vscode/docs/images/errors.gif)

### Debugger

The debugging feature allows you to run Clarity code, line-by-line, so you can better understand what happens when it runs.

**Note: This feature currently only runs on the desktop and requires a local [installation of Clarinet](https://github.com/stx-labs/clarinet).**

For more information on how debugging works, and how you can debug smart contracts, please see the [How to Debug Your Smart Contracts With Clarinet](https://www.hiro.so/blog/how-to-debug-your-smart-contracts-with-clarinet) blog post.

### Support VS Code for the Web

This extension works in VS Code on Desktop along with support for [vscode.dev](https://vscode.dev/) and [github.dev](https://github.dev/).

### Support for Traits

When a contract implements a trait (such as the NFT of FT trait – SIPs 009 and 010), the extensions will show and errors if the trait implementation is incomplete (for example, if the trait expects a function which is not implemented, or if the function signature does not match the trait definition).

### Handle Requirements

If your Clarity project relies on specific requirements (eg: SIPs 009 or 010) for interacting with contracts on mainnet, the extension will automatically detect and cached the required contracts.

---

## Contributing to this Extension

Stacks Labs welcomes feedback, comments and suggestions to improve this extension over time.

### Run the extension locally

You'll need to have Rust, Node.js and NPM installed.

From the `./components/clarity-vscode`, run `npm install` to install the dependencies and `npm run dev` to start the extension in a Chromium instance.

### Structure

The LSP has two main parts: the client and the server.
These two parts will run in different environments:

- VSCode Web (Web Worker)
- VSCode Desktop (Node.js)

The LSP (`./components/clarity-lsp`), written in Rust, is built with wasm-pack for both these environments and will be loaded accordingly, with `fetch()` in the browser and `fs.readFileSync()` in Node.js.

```
./components/clarity-vscode
├── package.json // The extension manifest.
├── client
│   └── src
│       ├── clientBrowser.ts
│       ├── clientNode.ts
│       └── common.ts
└── server
    └── src
        ├── serverBrowser.ts
        ├── serverNode.ts
        └── common.ts
```
