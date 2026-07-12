/*---
description: Generator function declarations and expressions
features: [compat-function, generators]
---*/

test("function* declaration yields values", () => {
  function* numbers() {
    yield 1;
    yield 2;
    return 3;
  }

  const iter = numbers();
  expect(iter.next()).toEqual({ value: 1, done: false });
  expect(iter.next()).toEqual({ value: 2, done: false });
  expect(iter.next()).toEqual({ value: 3, done: true });
});

test("function* expression yields values", () => {
  const numbers = function* () {
    yield 4;
  };
  const iter = numbers();

  expect(iter.next()).toEqual({ value: 4, done: false });
  expect(iter.next()).toEqual({ value: undefined, done: true });
});

test("anonymous function* expression infers name from assignment target", () => {
  let gen;

  gen = function* () {};

  expect(gen.name).toBe("gen");
});

test("named function* expression exposes its name internally", () => {
  const make = function* named() {
    yield named.name;
  };

  expect(make().next()).toEqual({ value: "named", done: false });
});

test("contextual keyword can name a function* expression", () => {
  const make = function* from() {
    yield from.name;
  };

  expect(make().next()).toEqual({ value: "from", done: false });
});

test("function* is not constructable", () => {
  const make = function* () {
    yield 1;
  };

  expect(() => new make()).toThrow(TypeError);
});

test("class static getter backed by function* returns a generator object", () => {
  class C {}
  Object.defineProperty(C, "value", {
    get: function* () {
      yield 1;
    },
    configurable: true,
  });

  const iter = C.value;

  expect(iter.next()).toEqual({ value: 1, done: false });
  expect(iter.next()).toEqual({ value: undefined, done: true });
});

test("class static setter backed by function* is called as a generator function", () => {
  let called = 0;
  class C {}
  Object.defineProperty(C, "value", {
    set: function* (next) {
      called = next;
      yield next;
    },
    configurable: true,
  });

  C.value = 5;

  expect(called).toBe(0);
});
