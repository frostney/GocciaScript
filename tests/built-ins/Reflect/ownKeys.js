/*---
description: Reflect.ownKeys
features: [Reflect]
---*/

describe("Reflect.ownKeys", () => {
  test("returns own string keys", () => {
    const obj = { a: 1, b: 2, c: 3 };
    const keys = Reflect.ownKeys(obj);
    expect(keys.length).toBe(3);
    expect(keys).toContain("a");
    expect(keys).toContain("b");
    expect(keys).toContain("c");
  });

  test("includes non-enumerable properties", () => {
    const obj = {};
    Object.defineProperty(obj, "hidden", {
      value: 42,
      enumerable: false,
    });
    obj.visible = 1;
    const keys = Reflect.ownKeys(obj);
    expect(keys).toContain("hidden");
    expect(keys).toContain("visible");
  });

  test("includes symbol keys", () => {
    const sym = Symbol("myKey");
    const obj = { [sym]: "value", name: "test" };
    const keys = Reflect.ownKeys(obj);
    expect(keys).toContain("name");
    expect(keys).toContain(sym);
  });

  test("returns empty array for empty object", () => {
    const obj = Object.create(null);
    expect(Reflect.ownKeys(obj).length).toBe(0);
  });

  test("does not include inherited properties", () => {
    const proto = { inherited: true };
    const obj = Object.create(proto);
    obj.own = 1;
    const keys = Reflect.ownKeys(obj);
    expect(keys).toContain("own");
    expect(keys.length).toBe(1);
  });

  test("throws TypeError if target is not an object", () => {
    expect(() => Reflect.ownKeys(42)).toThrow(TypeError);
    expect(() => Reflect.ownKeys("str")).toThrow(TypeError);
    expect(() => Reflect.ownKeys(null)).toThrow(TypeError);
  });
});
