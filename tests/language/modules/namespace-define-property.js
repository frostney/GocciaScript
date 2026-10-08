/*---
description: >
  Object.defineProperty on a module namespace returns the namespace for a
  descriptor that leaves an export unchanged and throws TypeError for any
  other, without corrupting the descriptor it refused.
features: [modules, namespace-imports]
---*/

import * as math from "./helpers/math-utils.js";

describe("Object.defineProperty on a module namespace", () => {
  test("accepts a descriptor that leaves the export unchanged", () => {
    expect(Object.defineProperty(math, "PI", {})).toBe(math);
    expect(Object.defineProperty(math, "PI", { value: 3.14159 })).toBe(math);
    expect(Reflect.defineProperty(math, "PI", { configurable: false })).toBe(true);
  });

  test("throws TypeError for a descriptor it refuses, every time", () => {
    // Each refusal hands the descriptor back to Object.defineProperty, which
    // frees it; the namespace used to free it first as well.
    for (const _ of Array.from({ length: 200 })) {
      expect(() => Object.defineProperty(math, "PI", { value: 1 })).toThrow(TypeError);
      expect(() => Object.defineProperty(math, "missing", {})).toThrow(TypeError);
      expect(() => Object.defineProperty(math, "PI", { configurable: true })).toThrow(TypeError);
    }
    expect(math.PI).toBe(3.14159);
  });

  test("Reflect.defineProperty reports a refusal as false", () => {
    expect(Reflect.defineProperty(math, "PI", { value: 1 })).toBe(false);
    expect(Reflect.defineProperty(math, "missing", {})).toBe(false);
  });
});
