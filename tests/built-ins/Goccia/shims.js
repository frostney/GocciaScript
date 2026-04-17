/*---
description: Goccia.shims exposes the array of registered shim names
features: [Goccia]
---*/

const hasGoccia = typeof Goccia !== "undefined";

describe.runIf(hasGoccia)("Goccia.shims", () => {
  test("shims is an array", () => {
    expect(Array.isArray(Goccia.shims)).toBe(true);
  });

  test("shims contains registered shim names", () => {
    expect(Goccia.shims).toContain("atob");
    expect(Goccia.shims).toContain("btoa");
    expect(Goccia.shims).toContain("parseInt");
    expect(Goccia.shims).toContain("parseFloat");
    expect(Goccia.shims).toContain("Date");
  });

  test("shims is read-only", () => {
    const original = Goccia.shims;
    expect(() => { Goccia.shims = "overwritten"; }).toThrow(TypeError);
    expect(Goccia.shims).toBe(original);
  });
});
