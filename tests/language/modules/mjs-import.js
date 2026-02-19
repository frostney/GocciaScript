import { mjsValue, mjsAdd } from "./helpers/mjs-module.mjs";
import { mjsValue as reExportedValue, mjsAdd as reExportedAdd } from "./helpers/mjs-re-exporter.js";

describe("mjs module import", () => {
  test("import value from .mjs module", () => {
    expect(mjsValue).toBe("from-mjs");
  });

  test("import function from .mjs module", () => {
    expect(mjsAdd(3, 7)).toBe(10);
  });
});

describe("mjs re-export", () => {
  test("re-exported value from .mjs module", () => {
    expect(reExportedValue).toBe("from-mjs");
  });

  test("re-exported function from .mjs module", () => {
    expect(reExportedAdd(5, 8)).toBe(13);
  });
});
