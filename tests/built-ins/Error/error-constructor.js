/*---
description: Error constructor creates error objects correctly
features: [Error]
---*/

test("Error constructor", () => {
  const error = new Error("Test error");
  expect(error.message).toBe("Test error");
  expect(error.name).toBe("Error");
});

test("TypeError constructor", () => {
  const error = new TypeError("Type error");
  expect(error.message).toBe("Type error");
  expect(error.name).toBe("TypeError");
});

test("ReferenceError constructor", () => {
  const error = new ReferenceError("Ref error");
  expect(error.message).toBe("Ref error");
  expect(error.name).toBe("ReferenceError");
});

test("Error with no message", () => {
  const error = new Error();
  expect(error.message).toBe("");
  expect(error.name).toBe("Error");
});

test("Error instanceof Error", () => {
  expect(new Error("test") instanceof Error).toBe(true);
  expect(new TypeError("test") instanceof Error).toBe(true);
  expect(new RangeError("test") instanceof Error).toBe(true);
  expect(new ReferenceError("test") instanceof Error).toBe(true);
});

test("RangeError constructor", () => {
  const error = new RangeError("Range error");
  expect(error.message).toBe("Range error");
  expect(error.name).toBe("RangeError");
});

test("SyntaxError constructor", () => {
  const error = new SyntaxError("Syntax error");
  expect(error.message).toBe("Syntax error");
  expect(error.name).toBe("SyntaxError");
});

test("URIError constructor", () => {
  const error = new URIError("URI error");
  expect(error.message).toBe("URI error");
  expect(error.name).toBe("URIError");
});

test("SyntaxError and URIError instanceof Error", () => {
  expect(new SyntaxError("test") instanceof Error).toBe(true);
  expect(new URIError("test") instanceof Error).toBe(true);
});

test("Error with cause option", () => {
  const cause = new Error("original");
  const error = new Error("wrapper", { cause: cause });
  expect(error.message).toBe("wrapper");
  expect(error.cause.message).toBe("original");
});

test("TypeError with cause option", () => {
  const error = new TypeError("type err", { cause: "root cause" });
  expect(error.cause).toBe("root cause");
});

test("AggregateError constructor", () => {
  const errors = [new Error("e1"), new Error("e2")];
  const error = new AggregateError(errors, "multiple errors");
  expect(error.name).toBe("AggregateError");
  expect(error.message).toBe("multiple errors");
  expect(error.errors.length).toBe(2);
  expect(error.errors[0].message).toBe("e1");
  expect(error.errors[1].message).toBe("e2");
});

test("AggregateError instanceof Error", () => {
  const error = new AggregateError([], "test");
  expect(error instanceof Error).toBe(true);
});

test("AggregateError with empty errors", () => {
  const error = new AggregateError([], "empty");
  expect(error.errors.length).toBe(0);
  expect(error.message).toBe("empty");
});
