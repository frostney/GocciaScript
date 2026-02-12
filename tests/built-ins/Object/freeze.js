/*---
description: Object.freeze and Object.isFrozen
features: [Object.freeze, Object.isFrozen]
---*/

describe("Object.freeze", () => {
  test("freezing an object prevents modification", () => {
    const obj = { a: 1, b: 2 };
    Object.freeze(obj);

    let threw = false;
    try {
      obj.a = 99;
    } catch (e) {
      threw = true;
    }
    expect(threw).toBe(true);
    expect(obj.a).toBe(1);
  });

  test("freeze returns the same object", () => {
    const obj = { x: 1 };
    const frozen = Object.freeze(obj);
    expect(frozen).toBe(obj);
  });

  test("Object.isFrozen returns true for frozen objects", () => {
    const obj = { a: 1 };
    expect(Object.isFrozen(obj)).toBe(false);
    Object.freeze(obj);
    expect(Object.isFrozen(obj)).toBe(true);
  });

  test("non-objects are considered frozen", () => {
    expect(Object.isFrozen(42)).toBe(true);
    expect(Object.isFrozen("hello")).toBe(true);
    expect(Object.isFrozen(true)).toBe(true);
  });

  test("freezing a non-object returns it as-is", () => {
    expect(Object.freeze(42)).toBe(42);
    expect(Object.freeze("hello")).toBe("hello");
  });

  test("frozen objects still allow reads", () => {
    const obj = { x: 10, y: 20 };
    Object.freeze(obj);
    expect(obj.x).toBe(10);
    expect(obj.y).toBe(20);
    expect(Object.keys(obj)).toEqual(["x", "y"]);
  });

  test("frozen properties are non-writable", () => {
    const obj = { a: 1, b: "hello" };
    Object.freeze(obj);

    const desc = Object.getOwnPropertyDescriptor(obj, "a");
    expect(desc.writable).toBe(false);
    expect(desc.configurable).toBe(false);
    expect(desc.value).toBe(1);
  });

  test("frozen properties retain their enumerable flag", () => {
    const obj = {};
    Object.defineProperty(obj, "hidden", {
      value: 42,
      enumerable: false,
      writable: true,
      configurable: true,
    });
    Object.defineProperty(obj, "visible", {
      value: 99,
      enumerable: true,
      writable: true,
      configurable: true,
    });

    Object.freeze(obj);

    const hiddenDesc = Object.getOwnPropertyDescriptor(obj, "hidden");
    expect(hiddenDesc.writable).toBe(false);
    expect(hiddenDesc.configurable).toBe(false);
    expect(hiddenDesc.enumerable).toBe(false);

    const visibleDesc = Object.getOwnPropertyDescriptor(obj, "visible");
    expect(visibleDesc.writable).toBe(false);
    expect(visibleDesc.configurable).toBe(false);
    expect(visibleDesc.enumerable).toBe(true);
  });

  test("cannot add new properties to a frozen object", () => {
    const obj = { a: 1 };
    Object.freeze(obj);

    let threw = false;
    try {
      obj.newProp = "nope";
    } catch (e) {
      threw = true;
    }
    expect(threw).toBe(true);
    expect(obj.newProp).toBe(undefined);
  });

  test("multiple properties all become non-writable", () => {
    const obj = { x: 1, y: 2, z: 3 };
    Object.freeze(obj);

    const keys = Object.keys(obj);
    keys.forEach((key) => {
      const desc = Object.getOwnPropertyDescriptor(obj, key);
      expect(desc.writable).toBe(false);
      expect(desc.configurable).toBe(false);
    });
  });
});
