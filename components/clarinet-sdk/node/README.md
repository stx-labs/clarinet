# Clarinet SDK for Node.js

The Clarinet SDK allows to interact with the simnet in Node.js.

If you want to use the Clarinet SDK in web browsers, try [@stacks/clarinet-sdk-browser](https://www.npmjs.com/package/@stacks/clarinet-sdk-browser).

Find the API references of the SDK in [our documentation](https://docs.stacks.co/reference/clarinet-js-sdk/overview).

You can use this SDK to:

- Interact with a clarinet project as you would with the Clarinet CLI
- Call public, read-only, and private functions from smart contracts
- Get clarity maps or data-var values
- Get contract interfaces (available functions and data)
- Write unit tests for Clarity smart contracts

## Installation

```sh
npm install @stacks/clarinet-sdk
```

## Usage

```ts
import { initSimnet } from "@stacks/clarinet-sdk";
import { Cl } from "@stacks/transactions";

async function main() {
  const simnet = await initSimnet();

  const accounts = simnet.getAccounts();
  const address1 = accounts.get("wallet_1");
  if (!address1) throw new Error("invalid wallet name.");

  const call = simnet.callPublicFn("counter", "add", [Cl.uint(1)], address1);
  console.log(Cl.prettyPrint(call.result)); // (ok u1)

  const counter = simnet.getDataVar("counter", "counter");
  console.log(Cl.prettyPrint(counter)); // 2
}

main();
```

By default, the SDK will look for a Clarinet.toml file in the current working directory.
It's also possible to provide the path to the manifest like so:

```ts
const simnet = await initSimnet("./path/to/Clarinet.toml");
```

## Tests

The SDK can be used to write unit tests for Clarinet projects.

You'll need to have Node.js (>= 18) and NPM setup. If you are not sure how to set it up, [Volta](https://volta.sh/) is a nice tool to get started.

In the terminal, run `node --version` to make sure it's available and up to date.

Open your terminal and go to a new or existing Clarinet project:

```sh
cd my-project
ls # you should see Clarinet.toml and package.json in the list
```

Install the dependencies and run the test

```sh
npm install
npm test
```

Visit the [clarity starter project](https://github.com/stx-labs/clarity-starter) to see the testing framework in action.

### Type checking

We recommend to use TypeScript to write the unit tests, but it's also possible to do it with JavaScript. To do so, rename your test files to `.test.js` instead of `.test.ts`. You can also delete the `tsconfig.json` and uninstall typescript with `npm uninstall typescript`.

Note: If you want to write your test in JavaScript but still have a certain level of type safety and autocompletion, VSCode can help you with that. You can create a basic `jsconfig.json` file:

```json
{
  "compilerOptions": {
    "checkJs": true,
    "strict": true
  },
  "include": ["node_modules/@stacks/clarinet-sdk/vitest-helpers/src", "unit-tests"]
}
```
