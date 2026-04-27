/*---
description: Object generator methods
features: [generators]
---*/

test("object generator method works without compat-function", () => {
  const obj = {
    *numbers() {
      yield this.start;
      yield this.start + 1;
    },
    start: 10,
  };

  const iter = obj.numbers();
  expect(iter.next()).toEqual({ value: 10, done: false });
  expect(iter.next()).toEqual({ value: 11, done: false });
  expect(iter.next()).toEqual({ value: undefined, done: true });
});

test("object generator method supports next value send-back", () => {
  const obj = {
    *echo() {
      const value = yield "ready";
      yield value + 1;
    },
  };

  const iter = obj.echo();
  expect(iter.next()).toEqual({ value: "ready", done: false });
  expect(iter.next(41)).toEqual({ value: 42, done: false });
});

test("object generator method supports yield delegation", () => {
  const obj = {
    *numbers() {
      yield* [1, 2, 3];
    },
  };

  expect([...obj.numbers()]).toEqual([1, 2, 3]);
});

test("object generator method return closes through finally", () => {
  let closed = false;
  const obj = {
    *numbers() {
      try {
        yield 1;
      } finally {
        closed = true;
      }
    },
  };

  const iter = obj.numbers();
  expect(iter.next()).toEqual({ value: 1, done: false });
  expect(iter.return(9)).toEqual({ value: 9, done: true });
  expect(closed).toBe(true);
});

test("object generator method throw is catchable", () => {
  const obj = {
    *numbers() {
      try {
        yield 1;
      } catch (value) {
        yield value;
      }
    },
  };

  const iter = obj.numbers();
  expect(iter.next()).toEqual({ value: 1, done: false });
  expect(iter.throw(7)).toEqual({ value: 7, done: false });
});

test("object generator method captures closure and binds rich params", () => {
  const outer = 3;
  const obj = {
    *sum(a, ...rest) {
      yield a + rest[0] + outer;
    },
    *defaulted(a = 5) {
      yield a;
    },
    *destructure({ value }) {
      yield value;
    },
  };

  expect(obj.sum(1, 4).next().value).toBe(8);
  expect(obj.defaulted().next().value).toBe(5);
  expect(obj.destructure({ value: 9 }).next().value).toBe(9);
});

test("object generator method works with for of and Array.from", () => {
  const obj = {
    *numbers() {
      yield 1;
      yield 2;
    },
  };
  const values = [];

  for (const value of obj.numbers()) {
    values.push(value);
  }

  expect(values).toEqual([1, 2]);
  expect(Array.from(obj.numbers())).toEqual([1, 2]);
});
