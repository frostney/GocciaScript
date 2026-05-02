/*---
description: Logical AND assignment operator (&&=) assigns only when the left-hand side is truthy
features: [logical-assignment]
---*/

test("logical AND assignment assigns when the left-hand side is truthy", () => {
  let truthy = 1;
  expect(truthy &&= 99).toBe(99);
  expect(truthy).toBe(99);

  let str = "hello";
  expect(str &&= "world").toBe("world");
  expect(str).toBe("world");

  let flag = true;
  expect(flag &&= false).toBe(false);
  expect(flag).toBe(false);
});

test("logical AND assignment short-circuits when the left-hand side is falsy", () => {
  let zero = 0;
  expect(zero &&= 5).toBe(0);
  expect(zero).toBe(0);

  let empty = "";
  expect(empty &&= "value").toBe("");
  expect(empty).toBe("");

  let no = false;
  expect(no &&= true).toBe(false);
  expect(no).toBe(false);

  let nul = null;
  expect(nul &&= 1).toBe(null);
  expect(nul).toBe(null);

  let undef = undefined;
  expect(undef &&= 1).toBe(undefined);
  expect(undef).toBe(undefined);
});

test("logical AND assignment does not evaluate the right-hand side when short-circuited", () => {
  let calls = 0;
  const compute = () => {
    calls += 1;
    return 42;
  };

  let falsy = 0;
  expect(falsy &&= compute()).toBe(0);
  expect(calls).toBe(0);

  let truthy = 1;
  expect(truthy &&= compute()).toBe(42);
  expect(calls).toBe(1);
});

test("logical AND assignment works for properties", () => {
  const obj = { truthy: 1, falsy: 0 };

  expect(obj.truthy &&= 42).toBe(42);
  expect(obj.truthy).toBe(42);

  expect(obj.falsy &&= 99).toBe(0);
  expect(obj.falsy).toBe(0);

  expect(obj.missing &&= 3).toBe(undefined);
  expect(obj.missing).toBe(undefined);
});

test("logical AND assignment works for computed properties", () => {
  const obj = { a: 1, b: 0 };
  let keyCalls = 0;
  const key = (k) => {
    keyCalls += 1;
    return k;
  };

  expect(obj[key("a")] &&= 9).toBe(9);
  expect(obj.a).toBe(9);
  expect(keyCalls).toBe(1);

  expect(obj[key("b")] &&= 9).toBe(0);
  expect(obj.b).toBe(0);
  expect(keyCalls).toBe(2);
});

test("logical AND assignment works for private fields", () => {
  class Box {
    #value = 1;

    read() {
      return this.#value;
    }

    update(value) {
      return this.#value &&= value;
    }
  }

  const box = new Box();
  expect(box.update("replaced")).toBe("replaced");
  expect(box.read()).toBe("replaced");
});

test("logical AND assignment only throws for const bindings when an assignment is needed", () => {
  const falsy = 0;
  expect(falsy &&= 2).toBe(0);

  const truthy = 1;
  expect(() => {
    truthy &&= 2;
  }).toThrow(TypeError);
});

test("logical AND assignment evaluates RHS before throwing for assigned const", () => {
  let sideEffect = 0;
  const truthy = 1;

  expect(() => {
    truthy &&= (sideEffect = 1);
  }).toThrow(TypeError);

  expect(sideEffect).toBe(1);
});

test("logical AND assignment throws for unresolved identifiers", () => {
  expect(() => {
    missingAndValue &&= 1;
  }).toThrow(ReferenceError);
});

test("logical AND assignment uses private accessors and throws for getter-only fields", () => {
  class GetterOnly {
    #storage = 10;

    get #value() {
      return this.#storage;
    }

    update(value) {
      return this.#value &&= value;
    }
  }

  const obj = new GetterOnly();
  expect(() => {
    obj.update(99);
  }).toThrow(TypeError);
});

test("logical AND assignment short-circuits getter-only private accessors when falsy", () => {
  class GetterOnly {
    #storage = 0;

    get #value() {
      return this.#storage;
    }

    update(value) {
      return this.#value &&= value;
    }
  }

  const obj = new GetterOnly();
  expect(obj.update(99)).toBe(0);
});
