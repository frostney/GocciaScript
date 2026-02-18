import { add, multiply, greet } from "./helpers/re-exporter.js";

describe("re-export", () => {
  test("re-exported add from math-utils", () => {
    expect(add(1, 2)).toBe(3);
  });

  test("re-exported multiply from math-utils", () => {
    expect(multiply(6, 7)).toBe(42);
  });

  test("re-exported greet from string-utils", () => {
    expect(greet("Goccia")).toBe("Hello, Goccia!");
  });
});
