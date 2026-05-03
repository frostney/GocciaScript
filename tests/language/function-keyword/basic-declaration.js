/*---
description: Basic function declaration
features: [compat-function]
---*/

var __gocciaFunctionCapturedGlobalVar = false;
function __gocciaSetFunctionCapturedGlobalVar() {
  __gocciaFunctionCapturedGlobalVar = true;
  return __gocciaFunctionCapturedGlobalVar;
}
const __gocciaFunctionCapturedGlobalVarInside = __gocciaSetFunctionCapturedGlobalVar();
const __gocciaFunctionCapturedGlobalVarObserved = __gocciaFunctionCapturedGlobalVar;

test("function declaration with return value", () => {
  function add(a, b) {
    return a + b;
  }
  expect(add(1, 2)).toBe(3);
});

test("function declaration without return", () => {
  let called = false;
  function doSomething() {
    called = true;
  }
  doSomething();
  expect(called).toBe(true);
});

test("function declaration with multiple statements", () => {
  function compute(x) {
    const doubled = x * 2;
    const incremented = doubled + 1;
    return incremented;
  }
  expect(compute(5)).toBe(11);
});

test("top-level function declaration writes captured global-backed var", () => {
  expect(__gocciaFunctionCapturedGlobalVarInside).toBe(true);
  expect(__gocciaFunctionCapturedGlobalVarObserved).toBe(true);
  expect(globalThis.__gocciaFunctionCapturedGlobalVar).toBe(true);
});

test("block-scoped function declaration does not overwrite outer var binding", () => {
  var f = () => "outer";
  {
    function f() {
      return "inner";
    }
    expect(f()).toBe("inner");
  }
  expect(f()).toBe("outer");
});

test("block-scoped function declaration captures same-block lexical bindings", () => {
  {
    let z = 4;
    const v = 6;
    function f() {
      return z + v;
    }
    expect(f()).toBe(10);
  }
});

test("try block function declaration captures same-block lexical bindings", () => {
  let value = 0;
  try {
    let z = 4;
    const v = 6;
    function f() {
      return z + v;
    }
    value = f();
  } catch (error) {
    value = -1;
  }
  expect(value).toBe(10);
});

test("catch block function declaration is hoisted within the catch block", () => {
  let value;
  try {
    throw "caught";
  } catch (error) {
    value = f();
    function f() {
      return error;
    }
  }
  expect(value).toBe("caught");
});

test("finally block function declaration is hoisted within the finally block", () => {
  let value;
  try {
    value = "try";
  } finally {
    value = f();
    function f() {
      return "finally";
    }
  }
  expect(value).toBe("finally");
});

test("block function declarations can capture later block functions", () => {
  let value;
  {
    function first() {
      return second();
    }
    function second() {
      return "second";
    }
    value = first();
  }
  expect(value).toBe("second");
});

test("hoisted function declaration captures initialized var binding", () => {
  var value = 41;
  function readValue() {
    return value;
  }
  expect(readValue()).toBe(41);
});

test("hoisted generator declaration captures initialized var binding", () => {
  var value = 42;
  function* readValue() {
    yield value;
  }
  expect(readValue().next().value).toBe(42);
});

test("hoisted async function declaration captures initialized var binding", async () => {
  var value = 43;
  async function readValue() {
    return value;
  }
  expect(await readValue()).toBe(43);
});

test("hoisted async generator declaration captures initialized var binding", async () => {
  var value = 44;
  async function* readValue() {
    yield value;
  }
  const result = await readValue().next();
  expect(result.value).toBe(44);
});
