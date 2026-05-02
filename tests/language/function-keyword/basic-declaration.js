/*---
description: Basic function declaration
features: [compat-function]
---*/

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
