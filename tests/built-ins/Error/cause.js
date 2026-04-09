/*---
description: Error cause option (ES2026 §20.5.8.1 InstallErrorCause)
features: [Error, Error.cause]
---*/

describe("Error.cause", () => {
  test("Error with cause option sets cause property", () => {
    const original = new Error("original");
    const error = new Error("wrapper", { cause: original });
    expect(error.message).toBe("wrapper");
    expect(error.cause).toBe(original);
    expect(error.cause.message).toBe("original");
  });

  test("cause can be any value type", () => {
    // String cause
    const e1 = new Error("msg", { cause: "string cause" });
    expect(e1.cause).toBe("string cause");

    // Number cause
    const e2 = new Error("msg", { cause: 42 });
    expect(e2.cause).toBe(42);

    // Boolean cause
    const e3 = new Error("msg", { cause: false });
    expect(e3.cause).toBe(false);

    // Null cause
    const e4 = new Error("msg", { cause: null });
    expect(e4.cause).toBe(null);

    // Undefined cause — spec says install if HasProperty is true
    const e5 = new Error("msg", { cause: undefined });
    expect(e5.cause).toBe(undefined);
    expect("cause" in e5).toBe(true);

    // Zero cause
    const e6 = new Error("msg", { cause: 0 });
    expect(e6.cause).toBe(0);

    // Empty string cause
    const e7 = new Error("msg", { cause: "" });
    expect(e7.cause).toBe("");

    // Array cause
    const arr = [1, 2, 3];
    const e8 = new Error("msg", { cause: arr });
    expect(e8.cause).toBe(arr);

    // Object cause
    const obj = { key: "value" };
    const e9 = new Error("msg", { cause: obj });
    expect(e9.cause).toBe(obj);
  });

  test("no cause property when options is not an object", () => {
    const e1 = new Error("msg", "not an object");
    expect("cause" in e1).toBe(false);

    const e2 = new Error("msg", 42);
    expect("cause" in e2).toBe(false);

    const e3 = new Error("msg", null);
    expect("cause" in e3).toBe(false);

    const e4 = new Error("msg", true);
    expect("cause" in e4).toBe(false);
  });

  test("no cause property when options object lacks cause", () => {
    const error = new Error("msg", {});
    expect("cause" in error).toBe(false);
  });

  test("no cause property when no options argument", () => {
    const error = new Error("msg");
    expect("cause" in error).toBe(false);
  });

  test("cause property is writable and configurable but not enumerable", () => {
    const error = new Error("msg", { cause: "test" });
    const desc = Object.getOwnPropertyDescriptor(error, "cause");
    expect(desc.value).toBe("test");
    expect(desc.writable).toBe(true);
    expect(desc.configurable).toBe(true);
    expect(desc.enumerable).toBe(false);
  });

  test("cause is not included in for-in enumeration", () => {
    const error = new Error("msg", { cause: "test" });
    const keys = [];
    for (const key of Object.keys(error)) {
      keys.push(key);
    }
    expect(keys.includes("cause")).toBe(false);
  });

  test("TypeError with cause", () => {
    const cause = new RangeError("out of bounds");
    const error = new TypeError("invalid type", { cause });
    expect(error.cause).toBe(cause);
    expect(error.cause.message).toBe("out of bounds");
    expect(error.name).toBe("TypeError");
  });

  test("RangeError with cause", () => {
    const error = new RangeError("out of range", { cause: "bad input" });
    expect(error.cause).toBe("bad input");
    expect(error.name).toBe("RangeError");
  });

  test("ReferenceError with cause", () => {
    const error = new ReferenceError("not defined", { cause: 404 });
    expect(error.cause).toBe(404);
    expect(error.name).toBe("ReferenceError");
  });

  test("SyntaxError with cause", () => {
    const error = new SyntaxError("parse failed", { cause: "unexpected token" });
    expect(error.cause).toBe("unexpected token");
    expect(error.name).toBe("SyntaxError");
  });

  test("URIError with cause", () => {
    const error = new URIError("bad URI", { cause: "malformed" });
    expect(error.cause).toBe("malformed");
    expect(error.name).toBe("URIError");
  });

  test("AggregateError with cause", () => {
    const errors = [new Error("e1"), new Error("e2")];
    const error = new AggregateError(errors, "multi", { cause: "root" });
    expect(error.cause).toBe("root");
    expect(error.name).toBe("AggregateError");
    expect(error.errors.length).toBe(2);
  });

  test("AggregateError cause is non-enumerable", () => {
    const error = new AggregateError([], "msg", { cause: "test" });
    const desc = Object.getOwnPropertyDescriptor(error, "cause");
    expect(desc.value).toBe("test");
    expect(desc.writable).toBe(true);
    expect(desc.configurable).toBe(true);
    expect(desc.enumerable).toBe(false);
  });

  test("error cause chaining", () => {
    const root = new Error("disk full");
    const mid = new Error("write failed", { cause: root });
    const top = new Error("save failed", { cause: mid });

    expect(top.cause).toBe(mid);
    expect(top.cause.cause).toBe(root);
    expect(top.cause.cause.message).toBe("disk full");
  });

  test("cause with Error.isError", () => {
    const inner = new TypeError("inner");
    const outer = new Error("outer", { cause: inner });
    expect(Error.isError(outer)).toBe(true);
    expect(Error.isError(outer.cause)).toBe(true);
  });

  test("cause from inherited property on options", () => {
    const proto = { cause: "inherited" };
    const options = Object.create(proto);
    const error = new Error("msg", options);
    expect(error.cause).toBe("inherited");
  });

  test("cause from getter on options", () => {
    const options = {};
    Object.defineProperty(options, "cause", {
      get: () => "from getter",
      enumerable: true,
      configurable: true,
    });
    const error = new Error("msg", options);
    expect(error.cause).toBe("from getter");
  });

  test("runtime-thrown errors do not have cause", () => {
    // Errors thrown by the runtime (e.g., property access on null)
    // should not have a cause property
    let caught;
    try {
      const x = null;
      x.foo;
    } catch (e) {
      caught = e;
    }
    expect("cause" in caught).toBe(false);
  });
});
