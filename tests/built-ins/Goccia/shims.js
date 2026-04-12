/*---
description: Goccia.shims exposes the array of registered shim names
features: [Goccia]
---*/

const hasGoccia = typeof Goccia !== "undefined";

describe.runIf(hasGoccia)("Goccia.shims", () => {
  test("shims is an array", () => {
    expect(Array.isArray(Goccia.shims)).toBe(true);
  });

  test("shims is currently empty", () => {
    expect(Goccia.shims.length).toBe(0);
  });

  test("shims is read-only", () => {
    const original = Goccia.shims;
    expect(() => { Goccia.shims = "overwritten"; }).toThrow(TypeError);
    expect(Goccia.shims).toBe(original);
  });
});
