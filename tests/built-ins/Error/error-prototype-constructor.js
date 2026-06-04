/*---
description: Error.prototype.constructor and NativeError.prototype.constructor
features: [Error]
---*/

test.each([Error, EvalError, TypeError, RangeError, ReferenceError, SyntaxError, URIError, AggregateError, SuppressedError, DOMException])("%s.prototype.constructor identity and descriptor", (ErrorType) => {
  expect(ErrorType.prototype.constructor).toBe(ErrorType);
  const desc = Object.getOwnPropertyDescriptor(ErrorType.prototype, "constructor");
  expect(desc.writable).toBe(true);
  expect(desc.configurable).toBe(true);
  expect(desc.enumerable).toBe(false);
});

test("error instance .constructor resolves through prototype chain", () => {
  expect(new Error("x").constructor).toBe(Error);
  expect(new EvalError("x").constructor).toBe(EvalError);
  expect(new TypeError("x").constructor).toBe(TypeError);
  expect(new RangeError("x").constructor).toBe(RangeError);
});

test("EvalError is a native Error subtype", () => {
  const error = new EvalError("x");
  expect(error.name).toBe("EvalError");
  expect(error.message).toBe("x");
  expect(error instanceof EvalError).toBe(true);
  expect(error instanceof Error).toBe(true);
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
