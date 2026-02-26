/*---
description: Error inheritance and prototype chain behavior
features: [Error, TypeError, RangeError, SyntaxError, ReferenceError, URIError, AggregateError]
---*/

describe("Error prototype chain", () => {
  test("new Error() instanceof Error", () => {
    const e = new Error("test");
    expect(e instanceof Error).toBe(true);
  });

  test("new TypeError() instanceof TypeError and Error", () => {
    const e = new TypeError("test");
    expect(e instanceof TypeError).toBe(true);
    expect(e instanceof Error).toBe(true);
  });

  test("new RangeError() instanceof RangeError and Error", () => {
    const e = new RangeError("test");
    expect(e instanceof RangeError).toBe(true);
    expect(e instanceof Error).toBe(true);
  });

  test("new ReferenceError() instanceof ReferenceError and Error", () => {
    const e = new ReferenceError("test");
    expect(e instanceof ReferenceError).toBe(true);
    expect(e instanceof Error).toBe(true);
  });

  test("new SyntaxError() instanceof SyntaxError and Error", () => {
    const e = new SyntaxError("test");
    expect(e instanceof SyntaxError).toBe(true);
    expect(e instanceof Error).toBe(true);
  });

  test("new URIError() instanceof URIError and Error", () => {
    const e = new URIError("test");
    expect(e instanceof URIError).toBe(true);
    expect(e instanceof Error).toBe(true);
  });

  test("new AggregateError() instanceof AggregateError and Error", () => {
    const e = new AggregateError([], "test");
    expect(e instanceof AggregateError).toBe(true);
    expect(e instanceof Error).toBe(true);
  });

  test("error types are not instances of each other", () => {
    const te = new TypeError("test");
    const re = new RangeError("test");
    const ref = new ReferenceError("test");
    expect(te instanceof RangeError).toBe(false);
    expect(te instanceof ReferenceError).toBe(false);
    expect(re instanceof TypeError).toBe(false);
    expect(ref instanceof TypeError).toBe(false);
  });
});

describe("internally thrown TypeError has correct prototype chain", () => {
  test("property access on null throws TypeError", () => {
    let caught;
    try { null.x; } catch (e) { caught = e; }
    expect(caught.name).toBe("TypeError");
    expect(caught instanceof TypeError).toBe(true);
    expect(caught instanceof Error).toBe(true);
    expect(caught instanceof RangeError).toBe(false);
  });

  test("property access on undefined throws TypeError", () => {
    let caught;
    try { undefined.x; } catch (e) { caught = e; }
    expect(caught.name).toBe("TypeError");
    expect(caught instanceof TypeError).toBe(true);
    expect(caught instanceof Error).toBe(true);
  });

  test("calling a non-function throws TypeError", () => {
    let caught;
    try { (5)(); } catch (e) { caught = e; }
    expect(caught.name).toBe("TypeError");
    expect(caught instanceof TypeError).toBe(true);
    expect(caught instanceof Error).toBe(true);
  });

  test("calling a string throws TypeError", () => {
    let caught;
    try { ("hello")(); } catch (e) { caught = e; }
    expect(caught.name).toBe("TypeError");
    expect(caught instanceof TypeError).toBe(true);
    expect(caught instanceof Error).toBe(true);
  });

  test("class constructor without new throws TypeError", () => {
    let caught;
    try { Map(); } catch (e) { caught = e; }
    expect(caught.name).toBe("TypeError");
    expect(caught instanceof TypeError).toBe(true);
    expect(caught instanceof Error).toBe(true);
  });

  test("const reassignment throws TypeError", () => {
    let caught;
    try {
      const x = 1;
      x = 2;
    } catch (e) { caught = e; }
    expect(caught.name).toBe("TypeError");
    expect(caught instanceof TypeError).toBe(true);
    expect(caught instanceof Error).toBe(true);
  });
});

describe("internally thrown RangeError has correct prototype chain", () => {
  test("ArrayBuffer with negative length throws RangeError", () => {
    let caught;
    try { new ArrayBuffer(-1); } catch (e) { caught = e; }
    expect(caught.name).toBe("RangeError");
    expect(caught instanceof RangeError).toBe(true);
    expect(caught instanceof Error).toBe(true);
    expect(caught instanceof TypeError).toBe(false);
  });
});

describe("thrown and caught errors preserve prototype chain", () => {
  test("throw new TypeError is catchable with instanceof", () => {
    let isTypeError = false;
    let isError = false;
    try {
      throw new TypeError("custom");
    } catch (e) {
      isTypeError = e instanceof TypeError;
      isError = e instanceof Error;
    }
    expect(isTypeError).toBe(true);
    expect(isError).toBe(true);
  });

  test("throw new RangeError is catchable with instanceof", () => {
    let isRangeError = false;
    try {
      throw new RangeError("out of range");
    } catch (e) {
      isRangeError = e instanceof RangeError;
    }
    expect(isRangeError).toBe(true);
  });

  test("rethrown errors preserve prototype chain", () => {
    let caught;
    try {
      try {
        throw new TypeError("inner");
      } catch (e) {
        throw e;
      }
    } catch (e) {
      caught = e;
    }
    expect(caught instanceof TypeError).toBe(true);
    expect(caught instanceof Error).toBe(true);
    expect(caught.message).toBe("inner");
  });
});

describe("Error.isError works with all error types", () => {
  test("Error.isError with constructed errors", () => {
    expect(Error.isError(new Error("test"))).toBe(true);
    expect(Error.isError(new TypeError("test"))).toBe(true);
    expect(Error.isError(new RangeError("test"))).toBe(true);
    expect(Error.isError(new ReferenceError("test"))).toBe(true);
    expect(Error.isError(new SyntaxError("test"))).toBe(true);
    expect(Error.isError(new URIError("test"))).toBe(true);
    expect(Error.isError(new AggregateError([], "test"))).toBe(true);
  });

  test("Error.isError with internally thrown errors", () => {
    let caught;
    try { null.x; } catch (e) { caught = e; }
    expect(Error.isError(caught)).toBe(true);
  });

  test("Error.isError with non-errors", () => {
    expect(Error.isError({})).toBe(false);
    expect(Error.isError("string")).toBe(false);
    expect(Error.isError(42)).toBe(false);
    expect(Error.isError(null)).toBe(false);
    expect(Error.isError(undefined)).toBe(false);
  });
});

describe("error .name and .message properties", () => {
  test("internally thrown TypeError has correct name", () => {
    let name;
    try { null.x; } catch (e) { name = e.name; }
    expect(name).toBe("TypeError");
  });

  test("internally thrown TypeError has a message", () => {
    let msg;
    try { null.x; } catch (e) { msg = e.message; }
    expect(typeof msg).toBe("string");
    expect(msg.length > 0).toBe(true);
  });

  test("constructed error name matches type", () => {
    expect(new Error("x").name).toBe("Error");
    expect(new TypeError("x").name).toBe("TypeError");
    expect(new RangeError("x").name).toBe("RangeError");
    expect(new ReferenceError("x").name).toBe("ReferenceError");
    expect(new SyntaxError("x").name).toBe("SyntaxError");
    expect(new URIError("x").name).toBe("URIError");
    expect(new AggregateError([], "x").name).toBe("AggregateError");
  });
});

describe(".toThrow() matcher works with error type checking", () => {
  test("toThrow(TypeError) matches internally thrown TypeError", () => {
    expect(() => { null.x; }).toThrow(TypeError);
  });

  test("toThrow(TypeError) matches calling non-function", () => {
    expect(() => { (42)(); }).toThrow(TypeError);
  });

  test("toThrow(RangeError) matches ArrayBuffer negative length", () => {
    expect(() => { new ArrayBuffer(-1); }).toThrow(RangeError);
  });

  test("toThrow(TypeError) does not match RangeError", () => {
    let matched = false;
    try {
      new ArrayBuffer(-1);
    } catch (e) {
      matched = e instanceof TypeError;
    }
    expect(matched).toBe(false);
  });
});
