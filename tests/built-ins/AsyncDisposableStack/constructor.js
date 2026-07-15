/*---
description: AsyncDisposableStack constructor
features: [explicit-resource-management]
---*/

describe("AsyncDisposableStack constructor", () => {
  test("throws when called without new", () => {
    expect(() => AsyncDisposableStack()).toThrow(TypeError);
  });

  test("uses the newTarget prototype when constructed reflectively", () => {
    const prototype = {};
    const NewTarget = (class {}).bind(null);
    NewTarget.prototype = prototype;

    const stack = Reflect.construct(AsyncDisposableStack, [], NewTarget);

    expect(Object.getPrototypeOf(stack)).toBe(prototype);
  });
});
