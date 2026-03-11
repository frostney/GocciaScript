/*---
description: performance[Symbol.toStringTag]
features: [performance]
---*/

describe("performance[Symbol.toStringTag]", () => {
  test("Object.prototype.toString returns [object Performance]", () => {
    expect(Object.prototype.toString.call(performance)).toBe("[object Performance]");
  });
});
