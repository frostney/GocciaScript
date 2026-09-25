// Bare specifiers resolve against the committed node_modules fixture next to
// this file, enabled for this folder only by its goccia.json
// ("allow-node-modules": true). Without that opt-in every import here fails.
import { exportsLabel, viaDependency } from "pkg-exports";
import { widen, widenName } from "pkg-exports/sub/widen";
import { exactLabel } from "pkg-exports/sub/exact";
import { scopedLabel } from "@fixture/scoped-pkg";
import { scopedDeepLabel } from "@fixture/scoped-pkg/deep";

describe("bare specifier resolution against node_modules", () => {
  test("the main entry comes from the exports map's import condition", () => {
    expect(exportsLabel).toBe("exports-main");
  });

  test("a bare dependency chain resolves three packages deep", () => {
    // pkg-exports (exports condition) -> pkg-main ("main" fallback) ->
    // pkg-nested (string-shorthand "exports"), none of them aliased.
    expect(viaDependency()).toBe("exports>main>nested");
  });

  test("an exports wildcard resolves a subpath to a TypeScript source", () => {
    expect(widenName).toBe("widen");
    expect(widen(21)).toBe(42);
  });

  test("an exact exports key wins over a matching wildcard pattern", () => {
    expect(exactLabel).toBe("sub-exact");
  });

  test("a scoped package resolves its main entry and a subpath export", () => {
    expect(scopedLabel).toBe("scoped-main");
    expect(scopedDeepLabel).toBe("scoped-deep");
  });
});
