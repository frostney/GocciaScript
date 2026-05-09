// delete identifier: unresolvable reference returns true
test("delete unresolvable identifier returns true", () => {
  expect(delete undeclaredVariable).toBe(true);
});

// delete identifier: declared binding returns false
test("delete declared binding returns false", () => {
  let x = 42;
  expect(delete x).toBe(false);
});

// delete non-configurable property returns false instead of TypeError
test("delete non-configurable property returns false", () => {
  const obj = {};
  Object.defineProperty(obj, "fixed", {
    value: 1,
    configurable: false,
  });
  expect(delete obj.fixed).toBe(false);
});

// delete configurable property still returns true
test("delete configurable property returns true", () => {
  const obj = { a: 1 };
  expect(delete obj.a).toBe(true);
  expect(obj.a).toBe(undefined);
});

// this-coercion: unattached function call gets globalThis
test("function call this-coercion to globalThis", () => {
  function getThis() {
    return this;
  }
  const result = getThis();
  expect(result).toBe(globalThis);
});

// arrow functions use lexical this (script-level this is globalThis)
test("arrow function this is lexical", () => {
  function outer() {
    const getThis = () => this;
    return getThis();
  }
  expect(outer()).toBe(globalThis);
});

// silent fail: assignment to non-writable property
test("assignment to non-writable property silently fails", () => {
  const obj = {};
  Object.defineProperty(obj, "ro", {
    value: 42,
    writable: false,
  });
  obj.ro = 100;
  expect(obj.ro).toBe(42);
});

// silent fail: assignment to frozen object
test("assignment to frozen object silently fails", () => {
  const obj = Object.freeze({ x: 1 });
  obj.x = 2;
  expect(obj.x).toBe(1);
});

// arguments object: basic access
test("arguments object is available in functions", () => {
  function f(a, b) {
    return arguments.length;
  }
  expect(f(1, 2)).toBe(2);
  expect(f(1, 2, 3)).toBe(3);
});

// arguments object: indexed access
test("arguments object indexed access", () => {
  function f() {
    return arguments[0] + arguments[1];
  }
  expect(f(10, 20)).toBe(30);
});

// arguments object: callee
test("arguments.callee points to the function", () => {
  function f() {
    return arguments.callee === f;
  }
  expect(f()).toBe(true);
});

// with statement: property lookup
test("with statement property lookup", () => {
  const obj = { x: 42, y: 10 };
  let result;
  with (obj) {
    result = x + y;
  }
  expect(result).toBe(52);
});

// with statement: property assignment
test("with statement property assignment", () => {
  const obj = { x: 1 };
  with (obj) {
    x = 99;
  }
  expect(obj.x).toBe(99);
});

// with statement: falls through to outer scope
test("with statement falls through for missing properties", () => {
  const outer = 100;
  const obj = { x: 1 };
  let result;
  with (obj) {
    result = outer;
  }
  expect(result).toBe(100);
});
