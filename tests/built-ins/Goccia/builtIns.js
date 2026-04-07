/*---
description: Goccia.builtIns exposes the array of enabled built-in flag names
features: [Goccia]
---*/

const hasGoccia = typeof Goccia !== "undefined";

describe.runIf(hasGoccia)("Goccia.builtIns", () => {
  test("builtIns is an array", () => {
    expect(Array.isArray(Goccia.builtIns)).toBe(true);
  });

  test("builtIns is not empty", () => {
    expect(Goccia.builtIns.length > 0).toBe(true);
  });

  test("builtIns contains only strings", () => {
    Goccia.builtIns.forEach((name) => {
      expect(typeof name).toBe("string");
    });
  });
});
