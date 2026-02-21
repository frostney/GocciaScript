/*---
description: Error objects have a stack property with a formatted stack trace
features: [Error, stack]
---*/

test("Error has stack property", () => {
  const error = new Error("test message");
  expect(typeof error.stack).toBe("string");
});

test("stack starts with error name and message", () => {
  const error = new Error("something went wrong");
  expect(error.stack.startsWith("Error: something went wrong")).toBe(true);
});

test("TypeError stack starts with TypeError", () => {
  const error = new TypeError("bad type");
  expect(error.stack.startsWith("TypeError: bad type")).toBe(true);
});

test("RangeError stack starts with RangeError", () => {
  const error = new RangeError("out of range");
  expect(error.stack.startsWith("RangeError: out of range")).toBe(true);
});

test("ReferenceError stack starts with ReferenceError", () => {
  const error = new ReferenceError("not defined");
  expect(error.stack.startsWith("ReferenceError: not defined")).toBe(true);
});

test("SyntaxError stack starts with SyntaxError", () => {
  const error = new SyntaxError("unexpected token");
  expect(error.stack.startsWith("SyntaxError: unexpected token")).toBe(true);
});

test("Error with empty message shows just the name", () => {
  const error = new Error();
  expect(error.stack.startsWith("Error")).toBe(true);
});

test("stack contains 'at' lines for call chain", () => {
  const makeError = () => new Error("inside function");
  const error = makeError();
  expect(error.stack.includes("at")).toBe(true);
});

test("stack traces through nested function calls", () => {
  const inner = () => new Error("deep");
  const middle = () => inner();
  const outer = () => middle();
  const error = outer();
  const lines = error.stack.split("\n");
  expect(lines.length >= 4).toBe(true);
  expect(lines[0]).toBe("Error: deep");
});

test("stack includes function names", () => {
  const namedFunction = () => new Error("named");
  const error = namedFunction();
  expect(error.stack.includes("namedFunction")).toBe(true);
});

test("stack from caught runtime error has trace", () => {
  let stack = "";
  try {
    const obj = undefined;
    obj.property;
  } catch (e) {
    stack = e.stack;
  }
  expect(typeof stack).toBe("string");
  expect(stack.includes("Error")).toBe(true);
});

test("stack trace shows caller chain", () => {
  const a = () => new Error("trace");
  const b = () => a();
  const c = () => b();
  const error = c();
  expect(error.stack.includes("a")).toBe(true);
  expect(error.stack.includes("b")).toBe(true);
  expect(error.stack.includes("c")).toBe(true);
});

test("AggregateError has stack property", () => {
  const error = new AggregateError([], "aggregate");
  expect(typeof error.stack).toBe("string");
  expect(error.stack.startsWith("AggregateError: aggregate")).toBe(true);
});

test("thrown and caught error preserves stack", () => {
  const thrower = () => {
    throw new Error("thrown");
  };
  let caughtStack = "";
  try {
    thrower();
  } catch (e) {
    caughtStack = e.stack;
  }
  expect(caughtStack.startsWith("Error: thrown")).toBe(true);
  expect(caughtStack.includes("thrower")).toBe(true);
});
