/*---
description: Object.defineProperty and Reflect.defineProperty define the property when reading the attributes deletes it first
features: [Reflect]
---*/

// ES2026 §20.1.2.4 step 3 reads the attributes with ToPropertyDescriptor,
// which runs their getters, before step 4 defines the property. A getter that
// deletes the property must not leave the definition holding the old one.
test("Object.defineProperty after an attribute getter deletes the property", () => {
  const o = { x: 1 };
  Object.defineProperty(o, "x", {
    get value() {
      delete o.x;
      return 2;
    },
  });
  expect(o.x).toBe(2);
  expect(Object.getOwnPropertyDescriptor(o, "x").writable).toBe(false);
});

test("Object.defineProperty after an attribute getter redefines the property", () => {
  const o = { x: 1 };
  Object.defineProperty(o, "x", {
    configurable: true,
    get value() {
      Object.defineProperty(o, "x", { get: () => 0, configurable: true });
      return 3;
    },
  });
  expect(o.x).toBe(3);
});

test("Reflect.defineProperty after an attribute getter deletes the property", () => {
  const o = { x: 1 };
  const defined = Reflect.defineProperty(o, "x", {
    get value() {
      delete o.x;
      return 4;
    },
  });
  expect(defined).toBe(true);
  expect(o.x).toBe(4);
});
