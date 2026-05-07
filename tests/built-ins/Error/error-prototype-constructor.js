/*---
description: Error.prototype.constructor and NativeError.prototype.constructor
features: [Error]
---*/

test("prototype.constructor identity and descriptor for all error types", () => {
  const types = [Error, TypeError, RangeError, ReferenceError, SyntaxError, URIError, AggregateError, SuppressedError, DOMException];
  types.forEach((ErrorType) => {
    expect(ErrorType.prototype.constructor).toBe(ErrorType);
    const desc = Object.getOwnPropertyDescriptor(ErrorType.prototype, "constructor");
    expect(desc.writable).toBe(true);
    expect(desc.configurable).toBe(true);
    expect(desc.enumerable).toBe(false);
  });
});

test("error instance .constructor resolves through prototype chain", () => {
  expect(new Error("x").constructor).toBe(Error);
  expect(new TypeError("x").constructor).toBe(TypeError);
  expect(new RangeError("x").constructor).toBe(RangeError);
});

test("constructor is inherited, not own on instances", () => {
  const e = new TypeError("x");
  expect(Object.prototype.hasOwnProperty.call(e, "constructor")).toBe(false);
  expect(e.constructor).toBe(TypeError);
});

test("caught error has .constructor.name", () => {
  try {
    throw new TypeError("fail");
  } catch (e) {
    expect(e.constructor.name).toBe("TypeError");
  }
});
