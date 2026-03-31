/*---
description: Nullish coalescing assignment operator (??=) assigns only for null and undefined
features: [nullish-coalescing-assignment]
---*/

test("nullish coalescing assignment works for identifiers", () => {
  let missing = undefined;
  expect(missing ??= 5).toBe(5);
  expect(missing).toBe(5);

  let empty = null;
  expect(empty ??= 7).toBe(7);
  expect(empty).toBe(7);

  let zero = 0;
  expect(zero ??= 9).toBe(0);
  expect(zero).toBe(0);
});

test("nullish coalescing assignment short-circuits the right-hand side", () => {
  let calls = 0;
  const compute = () => {
    calls += 1;
    return 11;
  };

  let present = "value";
  expect(present ??= compute()).toBe("value");
  expect(calls).toBe(0);

  let absent = undefined;
  expect(absent ??= compute()).toBe(11);
  expect(calls).toBe(1);
});

test("nullish coalescing assignment works for properties", () => {
  const obj = { present: false };

  expect(obj.present ??= true).toBe(false);
  expect(obj.present).toBe(false);

  expect(obj.missing ??= 3).toBe(3);
  expect(obj.missing).toBe(3);
});

test("nullish coalescing assignment works for computed properties and evaluates the key once", () => {
  const obj = {};
  let keyCalls = 0;
  const key = () => {
    keyCalls += 1;
    return "dynamic";
  };

  expect(obj[key()] ??= 9).toBe(9);
  expect(obj.dynamic).toBe(9);
  expect(keyCalls).toBe(1);
});

test("nullish coalescing assignment works for private fields", () => {
  class Counter {
    #value;

    read() {
      return this.#value;
    }

    initialize(value) {
      return this.#value ??= value;
    }
  }

  const counter = new Counter();

  expect(counter.initialize(4)).toBe(4);
  expect(counter.read()).toBe(4);
  expect(counter.initialize(8)).toBe(4);
  expect(counter.read()).toBe(4);
});

test("nullish coalescing assignment only throws for const bindings when an assignment is needed", () => {
  const keep = 1;
  expect(keep ??= 2).toBe(1);

  const missing = null;
  expect(() => {
    missing ??= 2;
  }).toThrow(TypeError);
});
