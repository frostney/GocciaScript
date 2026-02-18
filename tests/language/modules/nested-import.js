import { middleValue, middleGreet } from "./helpers/nested-middle.js";

describe("nested imports", () => {
  test("value passes through nested modules", () => {
    expect(middleValue).toBe(101);
  });

  test("function composes through nested modules", () => {
    expect(middleGreet("Alice")).toBe("Hi Alice!");
  });
});
