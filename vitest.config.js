import { defineConfig } from "vitest/config";

export default defineConfig({
  test: {
    globals: true,
    watch: false,
    environment: "node",
    include: ["tests/**/*.js"],
    exclude: [
      "tests/**/helpers/**/*.js",
      "tests/**/helpers/**/*.yaml",
      "tests/**/helpers/**/*.toml",
      "tests/**/helpers/**/*.json5",
      "tests/**/helpers/**/*.jsonl",

      "tests/**/modules/local-module.js",

      "tests/languages/modules/yaml-*.js",

      "tests/**/types-as-comments/**/*.js",
      "tests/**/enums/**/*.js",
      "tests/**/decorators/**/*.js",
      "tests/built-ins/FFI/**/*.js",

      "tests/built-ins/TypedArray/fromBase64.js",
      "tests/built-ins/TypedArray/fromHex.js",
      "tests/built-ins/TypedArray/prototype/toBase64.js",
      "tests/built-ins/TypedArray/prototype/toHex.js",
      "tests/built-ins/TypedArray/prototype/setFromBase64.js",
      "tests/built-ins/TypedArray/prototype/setFromHex.js",

      "tests/built-ins/Iterator/zip.js",
      "tests/built-ins/Iterator/zipKeyed.js",
      "tests/built-ins/Iterator/concat.js",

      "tests/**/emoji-identifiers.js",
    ],
  },
});
