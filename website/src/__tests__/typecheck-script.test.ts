import { expect, test } from "bun:test";
import packageJson from "../../package.json";

// Loading next.config.mjs makes fumadocs-mdx regenerate .source/ without
// waiting for the write, and `next typegen` can exit with a file truncated.
// `fumadocs-mdx` writes .source/ again, to completion, before tsc reads it.
test("type-check regenerates .source/ after next typegen and before tsc", () => {
  const steps = packageJson.scripts.typecheck
    .split("&&")
    .map((step) => step.trim());
  expect(steps).toEqual(["next typegen", "fumadocs-mdx", "tsc --noEmit"]);
});
