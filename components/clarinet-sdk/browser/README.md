# Clarinet SDK for the Web

The Clarinet SDK can be used to interact with the simnet from web browsers.

If you want to use the Clarinet SDK in Node.js, try [@stacks/clarinet-sdk](https://www.npmjs.com/package/@stacks/clarinet-sdk).

Find the API references of the SDK in [our documentation](https://docs.stacks.co/reference/clarinet-js-sdk/overview).

You can use this SDK to:

- Interact with a clarinet project as you would with the Clarinet CLI
- Call public, read-only, and private functions from smart contracts
- Get clarity maps or data-var values
- Get contract interfaces (available functions and data)
- Write unit tests for Clarity smart contracts

## Installation

```sh
npm install @stacks/clarinet-sdk-browser
```

### Usage

There are two ways to use the sdk in the browser:

- With an empty clarinet session:

```js
const simnet = await initSimnet();
await simnet.initEmptySession();
simnet.runSnippet("(+ 1 2)");
```

- With a clarinet project (ie: with a Clarinet.toml)
  💡 It requires to use a virtual file system. More documentation and examples soon.

```js
const simnet = await initSimnet();
await simnet.initSession("/project", "Clarinet.toml");
```
