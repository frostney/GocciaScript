import { add, multiply, PI } from "./helpers/math-utils.js";

describe("inline export", () => {
  test("export const function", () => {
    expect(add(10, 20)).toBe(30);
  });

  test("export const value", () => {
    expect(PI).toBe(3.14159);
  });

  test("export const function - multiply", () => {
    expect(multiply(3, 7)).toBe(21);
  });
});
