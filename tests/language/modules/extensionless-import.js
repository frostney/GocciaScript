import { add, multiply } from "./helpers/math-utils";
import { greet } from "./helpers/string-utils";
import { mjsValue } from "./helpers/mjs-module";

describe("extensionless import", () => {
  test("import .js module without extension", () => {
    expect(add(10, 20)).toBe(30);
    expect(multiply(3, 4)).toBe(12);
  });

  test("import another .js module without extension", () => {
    expect(greet("GocciaScript")).toBe("Hello, GocciaScript!");
  });

  test("import .mjs module without extension", () => {
    expect(mjsValue).toBe("from-mjs");
  });
});
