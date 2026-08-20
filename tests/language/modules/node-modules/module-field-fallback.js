// GocciaScript prefers the bundler-only "module" field over "main" when a
// package ships no exports map. Node ignores "module" entirely and would load
// ./main.cjs here; that file is CommonJS, which this engine does not load, so
// the deviation is what makes the package usable at all.
import { moduleFieldLabel } from "pkg-module-field";

describe("module field fallback", () => {
  test('"module" is preferred over "main" without an exports map', () => {
    expect(moduleFieldLabel).toBe("from-module-field");
  });
});
