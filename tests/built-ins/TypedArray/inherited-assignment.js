/*---
description: An assignment to an object whose prototype is a typed array goes through the typed array's [[Set]]
features: [TypedArray, prototype-chain]
---*/

// ES2026 §10.1.9.2 OrdinarySetWithOwnDescriptor hands the assignment to the
// parent's [[Set]]; for a typed array that is §10.4.5.5, which ignores a
// numeric key that is not a valid index when the receiver is another object.
describe("assignment through a typed array prototype", () => {
  test("a valid index creates an own property on the receiver", () => {
    const array = new Uint8Array(4);
    const child = Object.create(array);
    child[1] = 5;
    expect(Object.hasOwn(child, "1")).toBe(true);
    expect(child[1]).toBe(5);
    expect(array[1]).toBe(0);
  });

  test("a numeric key that is not a valid index is ignored", () => {
    const child = Object.create(new Uint8Array(4));
    child[10] = 5;
    child["-0"] = 5;
    child["1.5"] = 5;
    expect(Object.hasOwn(child, "10")).toBe(false);
    expect(Object.hasOwn(child, "-0")).toBe(false);
    expect(Object.hasOwn(child, "1.5")).toBe(false);
    expect(child[10]).toBeUndefined();
  });

  test("an assignment agrees with Reflect.set", () => {
    const child = Object.create(new Uint8Array(4));
    expect(Reflect.set(child, "10", 5)).toBe(true);
    expect(Object.hasOwn(child, "10")).toBe(false);
  });

  test("a non-numeric key creates an own property on the receiver", () => {
    const child = Object.create(new Uint8Array(4));
    child.name = "child";
    expect(Object.hasOwn(child, "name")).toBe(true);
  });
});
