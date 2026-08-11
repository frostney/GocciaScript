/*---
description: structuredClone serializes Error values as errors, not as ordinary objects
features: [structuredClone]
---*/

class BatchError extends Error {}
class NamedError extends Error {
  constructor(message) {
    super(message);
    this.name = "NamedError";
  }
}

test("clones a plain Error as an Error", () => {
  const clone = structuredClone(new Error("boom"));

  expect(clone instanceof Error).toBe(true);
  expect(clone.name).toBe("Error");
  expect(clone.message).toBe("boom");
  expect(typeof clone.stack).toBe("string");
});

test("keeps the constructor of a standard error type", () => {
  expect(structuredClone(new TypeError("t")) instanceof TypeError).toBe(true);
  expect(structuredClone(new RangeError("r")) instanceof RangeError).toBe(true);
  expect(structuredClone(new SyntaxError("s")) instanceof SyntaxError).toBe(true);
  expect(structuredClone(new ReferenceError("r")) instanceof ReferenceError).toBe(true);
  expect(structuredClone(new EvalError("e")) instanceof EvalError).toBe(true);
  expect(structuredClone(new URIError("u")) instanceof URIError).toBe(true);
});

test("a subclass deserializes as a plain Error", () => {
  // The serialized form carries a name, not a constructor, and the name is
  // clamped to the seven standard error names.
  const clone = structuredClone(new BatchError("x"));

  expect(clone instanceof Error).toBe(true);
  expect(clone instanceof BatchError).toBe(false);
  expect(clone.name).toBe("Error");
  expect(clone.message).toBe("x");
});

test("a non-standard name is clamped to Error", () => {
  const clone = structuredClone(new NamedError("x"));

  expect(clone.name).toBe("Error");
  expect(clone.message).toBe("x");
});

test("AggregateError is outside the serialized name list", () => {
  const clone = structuredClone(new AggregateError([new Error("inner")], "outer"));

  expect(clone instanceof Error).toBe(true);
  expect(clone.name).toBe("Error");
  expect(clone.message).toBe("outer");
  expect(clone.errors).toBe(undefined);
});

test("own properties are not carried over", () => {
  const original = new Error("boom");
  original.code = "E_BOOM";

  const clone = structuredClone(original);

  expect(clone.message).toBe("boom");
  expect(clone.code).toBe(undefined);
});

test("message is an own, non-enumerable property", () => {
  const clone = structuredClone(new Error("boom"));
  const descriptor = Object.getOwnPropertyDescriptor(clone, "message");

  expect(descriptor.value).toBe("boom");
  expect(descriptor.enumerable).toBe(false);
  expect(descriptor.writable).toBe(true);
  expect(descriptor.configurable).toBe(true);
});

test("name is inherited from the prototype, not an own property", () => {
  const clone = structuredClone(new TypeError("t"));

  expect(Object.prototype.hasOwnProperty.call(clone, "name")).toBe(false);
  expect(clone.name).toBe("TypeError");
});

test("an error without a message has no own message", () => {
  const clone = structuredClone(new Error());

  expect(Object.prototype.hasOwnProperty.call(clone, "message")).toBe(false);
  expect(clone.message).toBe("");
});

test("the clone is a distinct object", () => {
  const original = new Error("boom");
  const clone = structuredClone(original);

  expect(clone !== original).toBe(true);
});

test("clones an error nested inside an object", () => {
  const clone = structuredClone({ cause: new Error("inner") });

  expect(clone.cause instanceof Error).toBe(true);
  expect(clone.cause.message).toBe("inner");
});

test("clones an error inside an array", () => {
  const clone = structuredClone([new TypeError("t")]);

  expect(clone[0] instanceof TypeError).toBe(true);
  expect(clone[0].message).toBe("t");
});

test("a self-referencing error terminates and drops the cycle", () => {
  const original = new Error("cyclic");
  original.self = original;

  const clone = structuredClone(original);

  expect(clone.message).toBe("cyclic");
  // Own properties are not serialized for errors, so the cycle goes with them.
  expect(clone.self).toBe(undefined);
});

test("the same error referenced twice clones to one object", () => {
  const shared = new Error("shared");
  const clone = structuredClone({ first: shared, second: shared });

  expect(clone.first.message).toBe("shared");
  expect(clone.first === clone.second).toBe(true);
});
