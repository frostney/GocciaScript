/*---
description: Goccia.spec exposes implemented ECMAScript features keyed by spec year
features: [Goccia]
---*/

const hasGoccia = typeof Goccia !== "undefined";

describe.runIf(hasGoccia)("Goccia.spec", () => {
  test("spec is an object", () => {
    expect(typeof Goccia.spec).toBe("object");
    expect(Goccia.spec !== null).toBe(true);
  });

  test("spec has year keys", () => {
    const keys = Object.keys(Goccia.spec);
    expect(keys.length > 0).toBe(true);
    expect(keys.includes("2015")).toBe(true);
    expect(keys.includes("2025")).toBe(true);
    expect(keys.includes("2026")).toBe(true);
    expect(keys.includes("2027")).toBe(true);
  });

  test("each year maps to an array of feature entries", () => {
    const keys = Object.keys(Goccia.spec);
    keys.forEach((year) => {
      const features = Goccia.spec[year];
      expect(Array.isArray(features)).toBe(true);
      expect(features.length > 0).toBe(true);
      features.forEach((feature) => {
        expect(typeof feature.name).toBe("string");
        expect(typeof feature.link).toBe("string");
        expect(feature.link.startsWith("https://")).toBe(true);
      });
    });
  });

  test("lists WeakMap and WeakSet support", () => {
    const es2015 = Goccia.spec["2015"].map((feature) => feature.name);
    const es2026 = Goccia.spec["2026"].map((feature) => feature.name);
    expect(es2015.includes("WeakMap")).toBe(true);
    expect(es2015.includes("WeakSet")).toBe(true);
    expect(es2026.includes("WeakMap.prototype.getOrInsert / getOrInsertComputed")).toBe(true);
  });

  test("spec is read-only", () => {
    const original = Goccia.spec;
    expect(() => { Goccia.spec = "overwritten"; }).toThrow(TypeError);
    expect(Goccia.spec).toBe(original);
  });
});
