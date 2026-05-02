/*---
description: Goccia.builtIns exposes the array of enabled special-purpose runtime built-in names
features: [Goccia]
---*/

const hasGoccia = typeof Goccia !== "undefined";

describe.runIf(hasGoccia)("Goccia.builtIns", () => {
  test("builtIns is an array", () => {
    expect(Array.isArray(Goccia.builtIns)).toBe(true);
  });

  test("builtIns contains only strings", () => {
    Goccia.builtIns.forEach((name) => {
      expect(typeof name).toBe("string");
    });
  });

  test("builtIns includes TestAssertions when running under TestRunner", () => {
    expect(Goccia.builtIns.includes("TestAssertions")).toBe(true);
  });
});
