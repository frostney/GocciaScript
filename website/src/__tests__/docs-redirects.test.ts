import { describe, expect, test } from "bun:test";
import { existsSync } from "node:fs";
import { fileURLToPath } from "node:url";
import nextConfig from "../../next.config.mjs";

const REPOSITORY_ROOT = fileURLToPath(new URL("../../../", import.meta.url));

describe("docs redirects", () => {
  test("the removed interpreter page redirects permanently to architecture", async () => {
    const redirects = (await nextConfig.redirects?.()) ?? [];
    const interpreter = redirects.find(
      (redirect) => redirect.source === "/docs/interpreter",
    );

    expect(interpreter).toEqual({
      source: "/docs/interpreter",
      destination: "/docs/architecture",
      permanent: true,
    });
    expect(existsSync(`${REPOSITORY_ROOT}docs/interpreter.md`)).toBe(false);
    expect(existsSync(`${REPOSITORY_ROOT}docs/architecture.md`)).toBe(true);
  });
});
