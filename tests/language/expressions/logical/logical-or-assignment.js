/*---
description: Logical OR assignment operator (||=) assigns only when the left-hand side is falsy
features: [logical-assignment]
---*/

test("logical OR assignment assigns when the left-hand side is falsy", () => {
  let zero = 0;
  expect(zero ||= 5).toBe(5);
  expect(zero).toBe(5);

  let empty = "";
  expect(empty ||= "default").toBe("default");
  expect(empty).toBe("default");

  let no = false;
  expect(no ||= true).toBe(true);
  expect(no).toBe(true);

  let nul = null;
  expect(nul ||= "filled").toBe("filled");
  expect(nul).toBe("filled");

  let undef = undefined;
  expect(undef ||= 99).toBe(99);
  expect(undef).toBe(99);
});

test("logical OR assignment short-circuits when the left-hand side is truthy", () => {
  let truthy = 1;
  expect(truthy ||= 5).toBe(1);
  expect(truthy).toBe(1);

  let str = "hello";
  expect(str ||= "world").toBe("hello");
  expect(str).toBe("hello");

  let flag = true;
  expect(flag ||= false).toBe(true);
  expect(flag).toBe(true);
});

test("logical OR assignment does not evaluate the right-hand side when short-circuited", () => {
  let calls = 0;
  const compute = () => {
    calls += 1;
    return 42;
  };

  let truthy = "present";
  expect(truthy ||= compute()).toBe("present");
  expect(calls).toBe(0);

  let falsy = 0;
  expect(falsy ||= compute()).toBe(42);
  expect(calls).toBe(1);
});

test("logical OR assignment works for properties", () => {
  const obj = { present: 1, missing: 0 };

  expect(obj.present ||= 99).toBe(1);
  expect(obj.present).toBe(1);

  expect(obj.missing ||= 99).toBe(99);
  expect(obj.missing).toBe(99);

  expect(obj.absent ||= 3).toBe(3);
  expect(obj.absent).toBe(3);
});

test("logical OR assignment works for computed properties", () => {
  const obj = { a: 1, b: 0 };
  let keyCalls = 0;
  const key = (k) => {
    keyCalls += 1;
    return k;
  };

  expect(obj[key("a")] ||= 9).toBe(1);
  expect(obj.a).toBe(1);
  expect(keyCalls).toBe(1);

  expect(obj[key("b")] ||= 9).toBe(9);
  expect(obj.b).toBe(9);
  expect(keyCalls).toBe(2);
});

test("logical OR assignment works for private fields", () => {
  class Box {
    #value;

    read() {
      return this.#value;
    }

    initialize(value) {
      return this.#value ||= value;
    }
  }

  const box = new Box();
  expect(box.initialize(4)).toBe(4);
  expect(box.read()).toBe(4);
  expect(box.initialize(8)).toBe(4);
  expect(box.read()).toBe(4);
});

test("logical OR assignment only throws for const bindings when an assignment is needed", () => {
  const truthy = 1;
  expect(truthy ||= 2).toBe(1);

  const falsy = null;
  expect(() => {
    falsy ||= 2;
  }).toThrow(TypeError);
});

test("logical OR assignment throws for unresolved identifiers", () => {
  expect(() => {
    missingOrValue ||= 1;
  }).toThrow(ReferenceError);
});

test("logical OR assignment uses private accessors and throws for getter-only fields", () => {
  class GetterOnly {
    #storage = 0;

    get #value() {
      return this.#storage;
    }

    update(value) {
      return this.#value ||= value;
    }
  }

  const obj = new GetterOnly();
  expect(() => {
    obj.update(99);
  }).toThrow(TypeError);
});

test("logical OR assignment short-circuits getter-only private accessors when truthy", () => {
  class GetterOnly {
    #storage = 10;

    get #value() {
      return this.#storage;
    }

    update(value) {
      return this.#value ||= value;
    }
  }

  const obj = new GetterOnly();
  expect(obj.update(99)).toBe(10);
});
