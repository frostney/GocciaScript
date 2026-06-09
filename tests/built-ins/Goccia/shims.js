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
    expect(Goccia.shims).toContain("hasOwnProperty");
    expect(Goccia.shims).toContain("__proto__");
    expect(Goccia.shims).toContain("defineGetter");
    expect(Goccia.shims).toContain("defineSetter");
    expect(Goccia.shims).toContain("lookupGetter");
    expect(Goccia.shims).toContain("lookupSetter");
  });

  test("shims is read-only", () => {
    const original = Goccia.shims;
    expect(() => { Goccia.shims = "overwritten"; }).toThrow(TypeError);
    expect(Goccia.shims).toBe(original);
  });
});
