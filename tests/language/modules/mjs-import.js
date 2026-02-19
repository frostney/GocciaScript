import { mjsValue, mjsAdd } from "./helpers/mjs-module.mjs";

describe("mjs module import", () => {
  test("import value from .mjs module", () => {
    expect(mjsValue).toBe("from-mjs");
  });

  test("import function from .mjs module", () => {
    expect(mjsAdd(3, 7)).toBe(10);
  });
});
