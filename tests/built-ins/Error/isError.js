/*---
description: Error.isError returns true for Error instances and false for non-errors
features: [Error.isError]
---*/

describe("Error.isError", () => {
  test("returns true for Error", () => {
    expect(Error.isError(new Error())).toBe(true);
    expect(Error.isError(new Error("message"))).toBe(true);
  });

  test("returns true for TypeError", () => {
    expect(Error.isError(new TypeError())).toBe(true);
    expect(Error.isError(new TypeError("msg"))).toBe(true);
  });

  test("returns true for RangeError", () => {
    expect(Error.isError(new RangeError())).toBe(true);
  });

  test("returns true for ReferenceError", () => {
    expect(Error.isError(new ReferenceError())).toBe(true);
  });

  test("returns true for SyntaxError", () => {
    expect(Error.isError(new SyntaxError())).toBe(true);
  });

  test("returns true for URIError", () => {
    expect(Error.isError(new URIError())).toBe(true);
  });

  test("returns true for AggregateError", () => {
    expect(Error.isError(new AggregateError([]))).toBe(true);
    expect(Error.isError(new AggregateError([], "msg"))).toBe(true);
  });

  test("returns false for DOMException", () => {
    expect(Error.isError(new DOMException("msg"))).toBe(false);
  });

  test("returns false for null and undefined", () => {
    expect(Error.isError(null)).toBe(false);
    expect(Error.isError(undefined)).toBe(false);
  });

  test("returns false for primitives", () => {
    expect(Error.isError(42)).toBe(false);
    expect(Error.isError("error")).toBe(false);
    expect(Error.isError(true)).toBe(false);
    expect(Error.isError(false)).toBe(false);
  });

  test("returns false for plain objects", () => {
    expect(Error.isError({})).toBe(false);
    expect(Error.isError({ name: "Error", message: "fake" })).toBe(false);
  });

  test("returns false for arrays", () => {
    expect(Error.isError([])).toBe(false);
    expect(Error.isError([new Error()])).toBe(false);
  });

  test("returns false for functions", () => {
    expect(Error.isError(() => {})).toBe(false);
  });

  test("returns false with no arguments", () => {
    expect(Error.isError()).toBe(false);
  });

  test("returns true for caught errors", () => {
    let caught;
    try {
      throw new TypeError("test");
    } catch (e) {
      caught = e;
    }
    expect(Error.isError(caught)).toBe(true);
  });

  test("returns true for error with cause", () => {
    const err = new Error("outer", { cause: new Error("inner") });
    expect(Error.isError(err)).toBe(true);
    expect(Error.isError(err.cause)).toBe(true);
  });
});
