/*---
description: Error.prototype.toString (ES2026 §20.5.3.4)
features: [Error]
---*/

test("Error.prototype.toString returns name: message", () => {
  expect(new Error("hi").toString()).toBe("Error: hi");
});

test("TypeError.prototype.toString returns name: message", () => {
  expect(new TypeError("nope").toString()).toBe("TypeError: nope");
});

test("RangeError.prototype.toString returns name: message", () => {
  expect(new RangeError("out").toString()).toBe("RangeError: out");
});

test("ReferenceError.prototype.toString returns name: message", () => {
  expect(new ReferenceError("x").toString()).toBe("ReferenceError: x");
});

test("SyntaxError.prototype.toString returns name: message", () => {
  expect(new SyntaxError("bad").toString()).toBe("SyntaxError: bad");
});

test("URIError.prototype.toString returns name: message", () => {
  expect(new URIError("uri").toString()).toBe("URIError: uri");
});

test("Error.prototype.toString returns just name when message is empty", () => {
  expect(new Error().toString()).toBe("Error");
  expect(new Error("").toString()).toBe("Error");
});

test("Error.prototype.toString returns just message when name is empty", () => {
  const e = new Error("foo");
  e.name = "";
  expect(e.toString()).toBe("foo");
});

test("Error.prototype.toString returns empty string when both are empty", () => {
  const e = new Error();
  e.name = "";
  expect(e.toString()).toBe("");
});

test("Error.prototype.toString uses undefined name as 'Error'", () => {
  const e = new Error("test");
  e.name = undefined;
  expect(e.toString()).toBe("Error: test");
});

test("user class extending Error inherits prototype.toString", () => {
  class MyErr extends Error {}
  expect(new MyErr("x").toString()).toBe("Error: x");
});

test("String(error) uses prototype.toString", () => {
  expect(String(new Error("hi"))).toBe("Error: hi");
  expect(String(new TypeError("t"))).toBe("TypeError: t");
});

test("template literal uses prototype.toString", () => {
  const e = new Error("hello");
  expect(`${e}`).toBe("Error: hello");
});

test("Error.prototype.toString is inherited, not per-subclass", () => {
  const desc = Object.getOwnPropertyDescriptor(TypeError.prototype, "toString");
  expect(desc).toBe(undefined);
  expect(typeof Object.getOwnPropertyDescriptor(Error.prototype, "toString")).toBe("object");
});

test("Error.prototype.toString throws on non-object this", () => {
  const fn = Error.prototype.toString;
  expect(() => fn.call(null)).toThrow(TypeError);
  expect(() => fn.call(undefined)).toThrow(TypeError);
  expect(() => fn.call(42)).toThrow(TypeError);
  expect(() => fn.call("string")).toThrow(TypeError);
});

test("Error.prototype.toString is not a constructor", () => {
  const fn = Error.prototype.toString;
  expect(() => new fn()).toThrow(TypeError);
  expect(() => Reflect.construct(Object, [], fn)).toThrow(TypeError);
});
