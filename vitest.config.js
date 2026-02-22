import { defineConfig } from "vitest/config";

export default defineConfig({
  test: {
    globals: true,
    watch: false,
    environment: "node",
    include: ["tests/**/*.js"],
    exclude: [
      "tests/**/helpers/**/*.js",
      "tests/**/modules/local-module.js",
      "tests/**/types-as-comments/**/*.js",
      "tests/**/enums/**/*.js",
      "tests/**/emoji-identifiers.js",
    ],
  },
});
