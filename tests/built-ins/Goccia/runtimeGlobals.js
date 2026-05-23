/*---
description: Goccia.runtimeGlobals exposes the array of enabled runtime global names
features: [Goccia]
---*/

const hasGoccia = typeof Goccia !== "undefined";

describe.runIf(hasGoccia)("Goccia.runtimeGlobals", () => {
  test("runtimeGlobals is an array", () => {
    expect(Array.isArray(Goccia.runtimeGlobals)).toBe(true);
  });

  test("runtimeGlobals contains only strings", () => {
    Goccia.runtimeGlobals.forEach((name) => {
      expect(typeof name).toBe("string");
    });
  });

  test("runtimeGlobals includes TestAssertions when running under TestRunner", () => {
    expect(Goccia.runtimeGlobals.includes("TestAssertions")).toBe(true);
  });

  test("builtIns is not exposed", () => {
    expect("builtIns" in Goccia).toBe(false);
  });
});
