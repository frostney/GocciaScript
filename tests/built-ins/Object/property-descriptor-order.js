/*---
description: ToPropertyDescriptor observes descriptor fields in spec order
features: [Object.defineProperty, Proxy]
---*/

test("Object.defineProperty interleaves descriptor has/get operations", () => {
  const log = [];
  const descriptor = new Proxy({
    enumerable: true,
    configurable: true,
    value: 1,
    writable: true,
  }, {
    has(target, key) {
      log.push("has:" + key);
      return Reflect.has(target, key);
    },
    get(target, key, receiver) {
      log.push("get:" + key);
      return Reflect.get(target, key, receiver);
    },
  });

  Object.defineProperty({}, "value", descriptor);

  expect(log).toEqual([
    "has:enumerable",
    "get:enumerable",
    "has:configurable",
    "get:configurable",
    "has:value",
    "get:value",
    "has:writable",
    "get:writable",
    "has:get",
    "has:set",
  ]);
});
