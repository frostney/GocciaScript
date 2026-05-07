/*---
description: Error.prototype.constructor and NativeError.prototype.constructor
features: [Error]
---*/

test("Error.prototype.constructor === Error", () => {
  expect(Error.prototype.constructor).toBe(Error);
});

test("TypeError.prototype.constructor === TypeError", () => {
  expect(TypeError.prototype.constructor).toBe(TypeError);
});

test("RangeError.prototype.constructor === RangeError", () => {
  expect(RangeError.prototype.constructor).toBe(RangeError);
});

test("ReferenceError.prototype.constructor === ReferenceError", () => {
  expect(ReferenceError.prototype.constructor).toBe(ReferenceError);
});

test("SyntaxError.prototype.constructor === SyntaxError", () => {
  expect(SyntaxError.prototype.constructor).toBe(SyntaxError);
});

test("URIError.prototype.constructor === URIError", () => {
  expect(URIError.prototype.constructor).toBe(URIError);
});

test("AggregateError.prototype.constructor === AggregateError", () => {
  expect(AggregateError.prototype.constructor).toBe(AggregateError);
});

test("SuppressedError.prototype.constructor === SuppressedError", () => {
  expect(SuppressedError.prototype.constructor).toBe(SuppressedError);
});

test("DOMException.prototype.constructor === DOMException", () => {
  expect(DOMException.prototype.constructor).toBe(DOMException);
});

test("Error.prototype.constructor descriptor is {writable: true, configurable: true, enumerable: false}", () => {
  const desc = Object.getOwnPropertyDescriptor(Error.prototype, "constructor");
  expect(desc.writable).toBe(true);
  expect(desc.configurable).toBe(true);
  expect(desc.enumerable).toBe(false);
});

test("NativeError.prototype.constructor descriptors are {writable: true, configurable: true, enumerable: false}", () => {
  const types = [TypeError, RangeError, ReferenceError, SyntaxError, URIError, AggregateError, SuppressedError, DOMException];
  types.forEach((ErrorType) => {
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
