/*---
description: Error objects have a stack property with a formatted stack trace
features: [Error, stack]
---*/

import {
  importedAfterConstruction,
  importedAfterFunctionCall,
  importedAfterMethodCall,
} from "./helpers/stack-after-operation.js";

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

// An error that a nested call caught. Its stack lists every frame below the
// failure, so the frame that called this shows up as it was left, without that
// frame throwing or calling a built-in itself.
const absent = { value: null };
const failInNestedCall = () => {
  const fail = () => {
    try {
      absent.value.x;
    } catch (e) {
      return e;
    }
  };
  const error = fail();
  return error;
};

// The stacks a scenario yields when it skips and when it performs its
// operation. Both runs are reached from one call site, so the stacks can differ
// only by what the operation left on the scenario's own frame. A scenario binds
// the error before returning it: a call in tail position would replace the
// frame being examined.
const stacksAroundOperation = (scenario) =>
  [false, true].map((perform) => scenario(perform).stack);

const expectFrameUnchanged = (scenario) => {
  const [skipped, performed] = stacksAroundOperation(scenario);

  expect(performed).toBe(skipped);
  expect(performed.includes("at " + scenario.name + " (")).toBe(true);
};

const magnitude = Math.abs;

test("a built-in function call that returns leaves its caller's frame as it found it", () => {
  const afterFunctionCall = (perform) => {
    if (perform) magnitude(-1);
    const error = failInNestedCall();
    return error;
  };
  const locatedThenFunctionCall = (perform) => {
    try {
      absent.value.x;
    } catch (e) {}
    if (perform) magnitude(-1);
    const error = failInNestedCall();
    return error;
  };

  expectFrameUnchanged(afterFunctionCall);
  expectFrameUnchanged(locatedThenFunctionCall);
});

test("a built-in method call that returns leaves its caller's frame as it found it", () => {
  const afterMethodCall = (perform) => {
    if (perform) Math.max(1, 2);
    const error = failInNestedCall();
    return error;
  };
  const locatedThenMethodCall = (perform) => {
    try {
      absent.value.x;
    } catch (e) {}
    if (perform) Math.max(1, 2);
    const error = failInNestedCall();
    return error;
  };

  expectFrameUnchanged(afterMethodCall);
  expectFrameUnchanged(locatedThenMethodCall);
});

test("constructing a built-in leaves the constructing frame as it found it", () => {
  const afterConstruction = (perform) => {
    if (perform) new Map();
    const error = failInNestedCall();
    return error;
  };
  const locatedThenConstruction = (perform) => {
    try {
      absent.value.x;
    } catch (e) {}
    if (perform) new Map();
    const error = failInNestedCall();
    return error;
  };

  expectFrameUnchanged(afterConstruction);
  expectFrameUnchanged(locatedThenConstruction);
});

test("a built-in call or construction in an imported function leaves its frame as it found it", () => {
  expectFrameUnchanged(importedAfterFunctionCall);
  expectFrameUnchanged(importedAfterMethodCall);
  expectFrameUnchanged(importedAfterConstruction);
});

// Each scenario below catches a throw in its own frame, so the frame carries on
// past the throw before it reaches the later error.
const parse = JSON.parse;
const parseArguments = ["{"];
const negativeLength = [-1];

test("a built-in function call that throws leaves its caller's frame as it found it", () => {
  const afterThrowingCall = (perform) => {
    try {
      if (perform) JSON.parse("{");
    } catch (e) {}
    const error = failInNestedCall();
    return error;
  };
  const afterThrowingHeldCall = (perform) => {
    try {
      if (perform) parse("{");
    } catch (e) {}
    const error = failInNestedCall();
    return error;
  };
  const afterThrowingSpreadCall = (perform) => {
    try {
      if (perform) JSON.parse(...parseArguments);
    } catch (e) {}
    const error = failInNestedCall();
    return error;
  };
  const afterThrowingBoundCall = (perform) => {
    const parseOpenBrace = JSON.parse.bind(null, "{");
    try {
      if (perform) parseOpenBrace();
    } catch (e) {}
    const error = failInNestedCall();
    return error;
  };

  expectFrameUnchanged(afterThrowingCall);
  expectFrameUnchanged(afterThrowingHeldCall);
  expectFrameUnchanged(afterThrowingSpreadCall);
  expectFrameUnchanged(afterThrowingBoundCall);
});

test("a built-in method call that throws leaves its caller's frame as it found it", () => {
  const afterThrowingMethodCall = (perform) => {
    try {
      if (perform) "abc".repeat(-1);
    } catch (e) {}
    const error = failInNestedCall();
    return error;
  };
  const afterThrowingReflectApply = (perform) => {
    try {
      if (perform) Reflect.apply(JSON.parse, null, parseArguments);
    } catch (e) {}
    const error = failInNestedCall();
    return error;
  };

  expectFrameUnchanged(afterThrowingMethodCall);
  expectFrameUnchanged(afterThrowingReflectApply);
});

test("constructing a built-in that throws leaves the constructing frame as it found it", () => {
  const throwingIterable = {
    [Symbol.iterator]() {
      throw new Error("no entries");
    },
  };
  const afterThrowingConstruction = (perform) => {
    try {
      if (perform) new ArrayBuffer(-1);
    } catch (e) {}
    const error = failInNestedCall();
    return error;
  };
  const afterThrowingSpreadConstruction = (perform) => {
    try {
      if (perform) new ArrayBuffer(...negativeLength);
    } catch (e) {}
    const error = failInNestedCall();
    return error;
  };
  const afterConstructionCallbackThrows = (perform) => {
    try {
      if (perform) new Map(throwingIterable);
    } catch (e) {}
    const error = failInNestedCall();
    return error;
  };

  expectFrameUnchanged(afterThrowingConstruction);
  expectFrameUnchanged(afterThrowingSpreadConstruction);
  expectFrameUnchanged(afterConstructionCallbackThrows);
});

test("a Proxy trap that throws leaves the calling frame as it found it", () => {
  const callable = new Proxy(() => {}, {
    apply() {
      throw new Error("apply");
    },
  });
  const constructable = new Proxy(class {}, {
    construct() {
      throw new Error("construct");
    },
  });
  const afterThrowingApplyTrap = (perform) => {
    try {
      if (perform) callable();
    } catch (e) {}
    const error = failInNestedCall();
    return error;
  };
  const afterThrowingConstructTrap = (perform) => {
    try {
      if (perform) new constructable();
    } catch (e) {}
    const error = failInNestedCall();
    return error;
  };

  expectFrameUnchanged(afterThrowingApplyTrap);
  expectFrameUnchanged(afterThrowingConstructTrap);
});

test("a callback that throws out of a built-in leaves the calling frame as it found it", () => {
  const afterThrowingCallback = (perform) => {
    try {
      if (perform)
        [1].map(() => {
          throw new Error("callback");
        });
    } catch (e) {}
    const error = failInNestedCall();
    return error;
  };
  const afterNestedThrowingCallback = (perform) => {
    try {
      if (perform) [1].forEach(() => [2].map(() => JSON.parse("{")));
    } catch (e) {}
    const error = failInNestedCall();
    return error;
  };
  const callbackAfterThrowingCall = (perform) => {
    let error;
    [1].forEach(() => {
      try {
        if (perform) JSON.parse("{");
      } catch (e) {}
      error = failInNestedCall();
    });
    return error;
  };

  expectFrameUnchanged(afterThrowingCallback);
  expectFrameUnchanged(afterNestedThrowingCallback);
  expectFrameUnchanged(callbackAfterThrowingCall);
});

test("a fault caught in the same function leaves its frame as it found it", () => {
  const afterCaughtFault = (perform) => {
    try {
      if (perform) absent.value.x;
    } catch (e) {}
    const error = failInNestedCall();
    return error;
  };
  const afterCaughtNotCallable = (perform) => {
    try {
      if (perform) absent.value();
    } catch (e) {}
    const error = failInNestedCall();
    return error;
  };

  expectFrameUnchanged(afterCaughtFault);
  expectFrameUnchanged(afterCaughtNotCallable);
});

test("a throw that runs a finally block or a nested catch leaves the frame as it found it", () => {
  const insideFinallyAfterThrow = (perform) => {
    let error;
    try {
      try {
        if (perform) JSON.parse("{");
      } finally {
        error = failInNestedCall();
      }
    } catch (e) {}
    return error;
  };
  const afterFinallyRethrow = (perform) => {
    let finished = false;
    try {
      try {
        if (perform) JSON.parse("{");
      } finally {
        finished = true;
      }
    } catch (e) {}
    const error = failInNestedCall();
    return error;
  };
  const afterNestedRethrow = (perform) => {
    try {
      try {
        if (perform) JSON.parse("{");
      } catch (e) {
        throw e;
      }
    } catch (e) {}
    const error = failInNestedCall();
    return error;
  };
  const afterThrowInsideCatch = (perform) => {
    try {
      if (perform) JSON.parse("{");
    } catch (e) {
      try {
        if (perform) JSON.parse("{");
      } catch (inner) {}
    }
    const error = failInNestedCall();
    return error;
  };

  expectFrameUnchanged(insideFinallyAfterThrow);
  expectFrameUnchanged(afterFinallyRethrow);
  expectFrameUnchanged(afterNestedRethrow);
  expectFrameUnchanged(afterThrowInsideCatch);
});

test("a throw caught in or around a generator leaves the frames as it found them", () => {
  const generators = {
    *throwing() {
      throw new Error("generator");
    },
    *catching(perform) {
      try {
        if (perform) JSON.parse("{");
      } catch (e) {}
      const error = failInNestedCall();
      yield error;
    },
    *catchingAfterResume(perform) {
      yield 1;
      try {
        if (perform) JSON.parse("{");
      } catch (e) {}
      const error = failInNestedCall();
      yield error;
    },
  };
  const afterThrowingGenerator = (perform) => {
    try {
      if (perform) generators.throwing().next();
    } catch (e) {}
    const error = failInNestedCall();
    return error;
  };
  const insideGenerator = (perform) => {
    const error = generators.catching(perform).next().value;
    return error;
  };
  const insideResumedGenerator = (perform) => {
    const iterator = generators.catchingAfterResume(perform);
    iterator.next();
    const error = iterator.next().value;
    return error;
  };

  expectFrameUnchanged(afterThrowingGenerator);
  expectFrameUnchanged(insideGenerator);
  expectFrameUnchanged(insideResumedGenerator);
});

test("a throw caught in an async function leaves its frame as it found it", async () => {
  const beforeAwait = async (perform) => {
    try {
      if (perform) JSON.parse("{");
    } catch (e) {}
    const error = failInNestedCall();
    return error;
  };
  const afterAwait = async (perform) => {
    await null;
    try {
      if (perform) JSON.parse("{");
    } catch (e) {}
    const error = failInNestedCall();
    return error;
  };

  // Both runs start from one call site before either is awaited, so their
  // stacks can differ only by what the throw left on the scenario's frame.
  const stacksAroundAsyncOperation = async (scenario) =>
    (await Promise.all([false, true].map((perform) => scenario(perform)))).map(
      (error) => error.stack,
    );

  const [skippedBefore, performedBefore] =
    await stacksAroundAsyncOperation(beforeAwait);
  expect(performedBefore).toBe(skippedBefore);
  expect(performedBefore.includes("at beforeAwait (")).toBe(true);

  // Which frames a stack lists after a resumption differs between the
  // executors (#1273), so this compares the two runs only.
  const [skippedAfter, performedAfter] =
    await stacksAroundAsyncOperation(afterAwait);
  expect(performedAfter).toBe(skippedAfter);
});
