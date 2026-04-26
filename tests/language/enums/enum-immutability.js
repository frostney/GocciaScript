/*---
description: Enum objects are non-extensible with non-writable, non-configurable members
features: [enum-declaration]
---*/

test("enum members are non-writable", () => {
  enum E { A = 1 }

  expect(() => {
    E.A = 99;
  }).toThrow(TypeError);

  expect(E.A).toBe(1);
});

test("enum object is non-extensible", () => {
  enum E { A = 1 }

  expect(() => {
    E.B = 2;
  }).toThrow(TypeError);
});

test("enum members are non-configurable", () => {
  enum E { A = 1 }

  expect(() => {
    Object.defineProperty(E, "A", { value: 99 });
  }).toThrow(TypeError);
});

test("Object.isExtensible returns false for enum", () => {
  enum E { A = 1 }

  expect(Object.isExtensible(E)).toBe(false);
});

test("Object.isFrozen returns true for enum", () => {
  enum OrderStatus {
    Pending = 0,
    Brewing = 1,
    Ready = 2,
    Delivered = 3,
  }

  expect(Object.isFrozen(OrderStatus)).toBe(true);
});

test("Object.isSealed returns true for enum", () => {
  enum E { A = 1, B = 2 }

  expect(Object.isSealed(E)).toBe(true);
});
