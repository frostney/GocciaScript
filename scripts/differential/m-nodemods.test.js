// Differential suite L — bare-specifier resolution against node_modules.
//
// The fixture packages are committed under ./mods/nodemods/node_modules, so
// bun resolves them natively and goccia resolves them under the
// --allow-node-modules capability the suite's `gocciaFlags` entry carries.
// Every expectation here is one both runtimes must reach the same way; the
// two goccia-specific behaviours live in n-nodemods.goccia.test.js.
import {
  alphaLabel,
  chain,
  deepLabel,
  pinnedName,
  scopedLabel,
  toolName,
  widen,
} from "./mods/nodemods/entry.js";

describe("node_modules bare-specifier resolution", () => {
  test("a bare dependency chain resolves three packages deep", () => {
    // alpha (exports "." condition) -> beta ("main" fallback) -> gamma
    // (string-shorthand "exports"), the shape of a real zod -> tldts ->
    // tldts-core chain, with no alias configured for any link.
    expect(alphaLabel).toBe("alpha");
    expect(chain()).toBe("alpha>beta>gamma");
  });

  test("an exports wildcard subpath resolves to a TypeScript source", () => {
    // "./x/*" -> "./src/*.ts", the shape @convex-dev/workpool uses for
    // "./test" -> "./src/test.ts".
    expect(toolName).toBe("tool");
    expect(widen(21)).toBe(42);
  });

  test("an exact exports key wins over a matching wildcard pattern", () => {
    expect(pinnedName).toBe("pinned-exact");
  });

  test("a scoped package resolves both its main and a subpath export", () => {
    expect(scopedLabel).toBe("scoped-main");
    expect(deepLabel).toBe("scoped-deep");
  });
});
