/*---
description: DisposableStack constructor
features: [explicit-resource-management]
---*/

describe("DisposableStack constructor", () => {
  test("throws when called without new", () => {
    expect(() => DisposableStack()).toThrow(TypeError);
  });

  test("uses the newTarget prototype when constructed reflectively", () => {
    const prototype = {};
    const NewTarget = (class {}).bind(null);
    NewTarget.prototype = prototype;

    const stack = Reflect.construct(DisposableStack, [], NewTarget);

    expect(Object.getPrototypeOf(stack)).toBe(prototype);
  });
});
