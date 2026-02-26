/*---
description: ArrayBuffer Symbol.toStringTag
features: [ArrayBuffer, Symbol]
---*/

describe("ArrayBuffer Symbol.toStringTag", () => {
  test("toStringTag is 'ArrayBuffer'", () => {
    const buf = new ArrayBuffer(0);
    expect(buf[Symbol.toStringTag]).toBe("ArrayBuffer");
  });

  test("toStringTag on a non-empty buffer", () => {
    const buf = new ArrayBuffer(16);
    expect(buf[Symbol.toStringTag]).toBe("ArrayBuffer");
  });

  test("Object.prototype.toString returns [object ArrayBuffer]", () => {
    const buf = new ArrayBuffer(8);
    expect(Object.prototype.toString.call(buf)).toBe("[object ArrayBuffer]");
  });

  test("Object.prototype.toString on zero-length buffer", () => {
    const buf = new ArrayBuffer(0);
    expect(Object.prototype.toString.call(buf)).toBe("[object ArrayBuffer]");
  });
});
