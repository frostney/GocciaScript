/*---
description: structuredClone deep-clones Map and Set
features: [structuredClone, Map, Set]
---*/

describe("Map cloning", () => {
  test("clones a Map", () => {
    const original = new Map();
    original.set("a", 1);
    original.set("b", 2);
    const clone = structuredClone(original);
    expect(clone.get("a")).toBe(1);
    expect(clone.get("b")).toBe(2);
    expect(clone.size).toBe(2);
  });

  test("clone is a distinct Map", () => {
    const original = new Map();
    original.set("key", "value");
    const clone = structuredClone(original);
    clone.set("key", "changed");
    expect(original.get("key")).toBe("value");
    expect(clone.get("key")).toBe("changed");
  });

  test("clones Map with object values", () => {
    const obj = { nested: true };
    const original = new Map();
    original.set("obj", obj);
    const clone = structuredClone(original);
    const clonedObj = clone.get("obj");
    expect(clonedObj.nested).toBe(true);
    clonedObj.nested = false;
    expect(obj.nested).toBe(true);
  });
});

describe("Set cloning", () => {
  test("clones a Set", () => {
    const original = new Set([1, 2, 3]);
    const clone = structuredClone(original);
    expect(clone.size).toBe(3);
    expect(clone.has(1)).toBe(true);
    expect(clone.has(2)).toBe(true);
    expect(clone.has(3)).toBe(true);
  });

  test("clone is a distinct Set", () => {
    const original = new Set([1, 2]);
    const clone = structuredClone(original);
    clone.add(3);
    expect(original.size).toBe(2);
    expect(clone.size).toBe(3);
  });

  test("clones Set with object values", () => {
    const obj = { value: 42 };
    const original = new Set([obj]);
    const clone = structuredClone(original);
    expect(clone.size).toBe(1);
    const [clonedObj] = [...clone];
    expect(clonedObj.value).toBe(42);
    clonedObj.value = 99;
    expect(obj.value).toBe(42);
  });
});
