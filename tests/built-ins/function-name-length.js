/*---
description: Built-in function .name and .length own properties
features: [function-properties]
---*/

describe("built-in function .name property", () => {
  test("Array prototype methods have correct name", () => {
    expect(Array.prototype.map.name).toBe("map");
    expect(Array.prototype.filter.name).toBe("filter");
    expect(Array.prototype.reduce.name).toBe("reduce");
    expect(Array.prototype.forEach.name).toBe("forEach");
  });

  test("name is an own property with correct descriptor", () => {
    const desc = Object.getOwnPropertyDescriptor(Array.prototype.map, "name");
    expect(desc).not.toBe(undefined);
    expect(desc.value).toBe("map");
    expect(desc.enumerable).toBe(false);
    expect(desc.configurable).toBe(true);
    expect(desc.writable).toBe(false);
  });

  test("name is visible to hasOwnProperty", () => {
    expect(Object.hasOwn(Array.prototype.map, "name")).toBe(true);
    expect(Object.hasOwn(Math.max, "name")).toBe(true);
  });

  test("static methods have correct name", () => {
    expect(Array.from.name).toBe("from");
    expect(Array.of.name).toBe("of");
    expect(Object.keys.name).toBe("keys");
    expect(Number.isNaN.name).toBe("isNaN");
  });
});

describe("built-in function .length property", () => {
  test("Array prototype methods have correct length", () => {
    expect(Array.prototype.map.length).toBe(1);
    expect(Array.prototype.reduce.length).toBe(1);
    expect(Array.prototype.push.length).toBe(1);
    expect(Array.prototype.splice.length).toBe(2);
  });

  test("length is an own property with correct descriptor", () => {
    const desc = Object.getOwnPropertyDescriptor(Array.prototype.map, "length");
    expect(desc).not.toBe(undefined);
    expect(desc.value).toBe(1);
    expect(desc.enumerable).toBe(false);
    expect(desc.configurable).toBe(true);
    expect(desc.writable).toBe(false);
  });

  test("length is visible to hasOwnProperty", () => {
    expect(Object.hasOwn(Array.prototype.map, "length")).toBe(true);
    expect(Object.hasOwn(Math.max, "length")).toBe(true);
  });

  test("Math methods have correct length", () => {
    expect(Math.abs.length).toBe(1);
    expect(Math.floor.length).toBe(1);
    expect(Math.pow.length).toBe(2);
    expect(Math.max.length).toBe(2);
    expect(Math.min.length).toBe(2);
  });

  test("variadic static methods have spec-correct length", () => {
    expect(Object.assign.length).toBe(2);
    expect(Array.of.length).toBe(0);
  });

  test("String methods have correct name and length", () => {
    expect(String.prototype.slice.name).toBe("slice");
    expect(String.prototype.slice.length).toBe(2);
    expect(String.prototype.includes.name).toBe("includes");
    expect(String.prototype.includes.length).toBe(1);
  });
});
