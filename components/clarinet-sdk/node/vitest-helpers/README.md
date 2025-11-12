# vitest-helpers

This directory contains the setup files used in the `vitest.config.ts` of a Clarinet project.

This mean that these file are not directly part of the clarinet-sdk code and not compiled.
Instead, they are directly loaded by `vitest.config.ts`, following the path exposed in `@stacks/clarinet-sdk/vitest`

### Contributing

This directory contains its own tests that are run in the parent directory (`clarinet-sdk`).

```sh
cd clarinet/components/clarinet-sdk
npm test # will run both sdk and custom matcher unit tests
```
