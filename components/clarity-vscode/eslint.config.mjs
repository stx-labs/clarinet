import typescriptEslint from "@typescript-eslint/eslint-plugin";
import js from "@eslint/js";

export default [
  {
    ignores: [
      "server/src/clarity-lsp-browser/**",
      "server/src/clarity-lsp-node/**",
    ],
  },
  ...[js.configs.recommended, ...typescriptEslint.configs["flat/recommended"]].map(
    (config) => ({
      ...config,
      files: ["**/*.ts", "**/*.tsx"],
    }),
  ),
  {
    files: ["**/*.ts", "**/*.tsx"],
    rules: {
      semi: [2, "always"],
      "comma-dangle": [2, "always-multiline"],
      "@typescript-eslint/ban-ts-comment": 0,
      "@typescript-eslint/no-unused-vars": 0,
      "@typescript-eslint/no-explicit-any": 0,
      "@typescript-eslint/explicit-module-boundary-types": 0,
      "@typescript-eslint/no-non-null-assertion": 0,

      "@typescript-eslint/naming-convention": [
        "error",
        {
          selector: "memberLike",
          modifiers: ["private"],
          format: ["camelCase"],
          leadingUnderscore: "require",
        },
      ],
    },
  },
];
