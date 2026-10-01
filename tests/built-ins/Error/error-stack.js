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

test("stack captured inside a function invoked via a native callback has frames", () => {
  const makeError = () => new Error("via callback");
  let captured = "";
  [1].map(() => {
    captured = makeError().stack;
  });
  expect(captured.startsWith("Error: via callback")).toBe(true);
  expect(captured.includes("    at ")).toBe(true);
});

test("error thrown inside a native callback preserves a trace", () => {
  let caughtStack = "";
  try {
    [1, 2, 3].forEach((x) => {
      if (x === 2) throw new Error("in forEach");
    });
  } catch (e) {
    caughtStack = e.stack;
  }
  expect(caughtStack.startsWith("Error: in forEach")).toBe(true);
  expect(caughtStack.includes("    at ")).toBe(true);
});

// The position a stack line ends with: "    at name (file:line:column)".
const positionOf = (stackLine) => {
  const match = stackLine.match(/:(\d+):(\d+)\)$/);
  return { line: Number(match[1]), column: Number(match[2]) };
};

test("an error a built-in creates is located at the call that reached it", () => {
  let first;
  let second;
  try {
    JSON.parse("{");
  } catch (e) {
    first = e;
  }
  try {
    JSON.parse("{");
  } catch (e) {
    second = e;
  }
  const firstPosition = positionOf(first.stack.split("\n")[1]);
  const secondPosition = positionOf(second.stack.split("\n")[1]);

  // The two calls sit five lines apart, at the same column.
  expect(first.stack.startsWith("SyntaxError: ")).toBe(true);
  expect(secondPosition.line - firstPosition.line).toBe(5);
  expect(secondPosition.column).toBe(firstPosition.column);
  expect(firstPosition.column).toBeGreaterThan(0);
});

test("an error a built-in method creates is located at the method call", () => {
  const text = "abc";
  let fromFunction;
  let fromMethod;
  try {
    JSON.parse("{");
  } catch (e) {
    fromFunction = e;
  }
  try {
    text.repeat(-1);
  } catch (e) {
    fromMethod = e;
  }
  const functionPosition = positionOf(fromFunction.stack.split("\n")[1]);
  const methodPosition = positionOf(fromMethod.stack.split("\n")[1]);

  expect(fromMethod.stack.startsWith("RangeError: ")).toBe(true);
  expect(methodPosition.line - functionPosition.line).toBe(5);
});

test("a built-in call that succeeds does not relocate a later error", () => {
  let first;
  let second;
  try {
    JSON.parse("{");
  } catch (e) {
    first = e;
  }
  Math.max(1, 2);
  "abc".toUpperCase();
  try {
    JSON.parse("{");
  } catch (e) {
    second = e;
  }
  const firstPosition = positionOf(first.stack.split("\n")[1]);
  const secondPosition = positionOf(second.stack.split("\n")[1]);

  // Seven lines apart: the two successful calls in between leave no trace.
  expect(secondPosition.line - firstPosition.line).toBe(7);
});
