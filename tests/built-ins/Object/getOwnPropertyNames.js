/*---
description: Object.getOwnPropertyNames returns all own property names
features: [Object.getOwnPropertyNames]
---*/

describe("Object.getOwnPropertyNames", () => {
  test("returns enumerable property names", () => {
    const obj = { a: 1, b: 2, c: 3 };
    const names = Object.getOwnPropertyNames(obj);
    expect(names).toContain("a");
    expect(names).toContain("b");
    expect(names).toContain("c");
    expect(names).toHaveLength(3);
  });

  test("returns non-enumerable property names", () => {
    const obj = {};
    Object.defineProperty(obj, "hidden", {
      value: 42,
      enumerable: false,
    });
    Object.defineProperty(obj, "visible", {
      value: 99,
      enumerable: true,
    });
    const names = Object.getOwnPropertyNames(obj);
    expect(names).toContain("hidden");
    expect(names).toContain("visible");
    expect(names).toHaveLength(2);
  });

  test("does not include inherited properties", () => {
    const parent = { inherited: true };
    const child = Object.create(parent);
    child.own = "value";
    const names = Object.getOwnPropertyNames(child);
    expect(names).toContain("own");
    expect(names).toHaveLength(1);
  });

  test("returns empty array for empty object", () => {
    const names = Object.getOwnPropertyNames({});
    expect(names).toHaveLength(0);
  });

  test("includes both data and accessor properties", () => {
    const obj = {};
    Object.defineProperty(obj, "data", {
      value: 1,
      enumerable: true,
    });
    Object.defineProperty(obj, "accessor", {
      get: () => 2,
      enumerable: true,
    });
    const names = Object.getOwnPropertyNames(obj);
    expect(names).toContain("data");
    expect(names).toContain("accessor");
  });

  test("Object.keys only returns enumerable, getOwnPropertyNames returns all", () => {
    const obj = {};
    Object.defineProperty(obj, "enumerable", {
      value: 1,
      enumerable: true,
    });
    Object.defineProperty(obj, "nonEnumerable", {
      value: 2,
      enumerable: false,
    });

    const keys = Object.keys(obj);
    const allNames = Object.getOwnPropertyNames(obj);

    expect(keys).toHaveLength(1);
    expect(keys).toContain("enumerable");

    expect(allNames).toHaveLength(2);
    expect(allNames).toContain("enumerable");
    expect(allNames).toContain("nonEnumerable");
  });
});
