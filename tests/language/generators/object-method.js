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
  expect(iter.next()).toEqual({ value: undefined, done: true });
  expect(iter.next(99)).toEqual({ value: undefined, done: true });
  expect(iter.return(99)).toEqual({ value: 99, done: true });
});

test("object generator method does not replay call arguments before a yielded argument", () => {
  const events = [];
  const obj = {
    *values() {
      const left = () => {
        events.push("left");
        return 10;
      };
      const combine = (a, b) => {
        events.push("combine");
        return a + b;
      };

      const result = combine(left(), yield "resume");
      yield result;
    },
  };

  const iter = obj.values();
  expect(iter.next()).toEqual({ value: "resume", done: false });
  expect(events).toEqual(["left"]);
  expect(iter.next(5)).toEqual({ value: 15, done: false });
  expect(events).toEqual(["left", "combine"]);
});

test("object generator method supports yield delegation", () => {
  const obj = {
    *numbers() {
      yield* [1, 2, 3];
    },
  };

  expect([...obj.numbers()]).toEqual([1, 2, 3]);
});

test("object generator yield delegation uses the sync iterator", () => {
  const source = {
    [Symbol.asyncIterator]() {
      return {
        next() {
          throw new Error("async iterator should not be used");
        },
      };
    },
    [Symbol.iterator]() {
      return [1, 2][Symbol.iterator]();
    },
  };
  const obj = {
    *numbers() {
      yield* source;
    },
  };

  expect([...obj.numbers()]).toEqual([1, 2]);
});

test("object generator yield delegation rejects iterator protocol violations", () => {
  const missingNext = {
    [Symbol.iterator]() {
      return {};
    },
  };
  const primitiveResult = {
    [Symbol.iterator]() {
      return {
        next() {
          return 1;
        },
      };
    },
  };
  const obj = {
    *missingNext() {
      yield* missingNext;
    },
    *primitiveResult() {
      yield* primitiveResult;
    },
  };

  expect(() => obj.missingNext().next()).toThrow(TypeError);
  expect(() => obj.primitiveResult().next()).toThrow(TypeError);
});

test("object generator yield delegation does not await sync iterator results", () => {
  let thenCalled = false;
  const source = {
    [Symbol.iterator]() {
      return {
        next() {
          return {
            value: 5,
            done: false,
            then() {
              thenCalled = true;
            },
          };
        },
      };
    },
  };
  const obj = {
    *numbers() {
      yield* source;
    },
  };

  expect(obj.numbers().next()).toEqual({ value: 5, done: false });
  expect(thenCalled).toBe(false);
});

test("object generator yield delegation does not replay side effects before the delegation", () => {
  const events = [];
  const obj = {
    *numbers() {
      events.push("before");
      yield* [1, 2];
      events.push("after");
    },
  };

  const iter = obj.numbers();
  expect(iter.next()).toEqual({ value: 1, done: false });
  expect(events).toEqual(["before"]);
  expect(iter.next()).toEqual({ value: 2, done: false });
  expect(events).toEqual(["before"]);
  expect(iter.next()).toEqual({ value: undefined, done: true });
  expect(events).toEqual(["before", "after"]);
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

test("object generator method return does not replay side effects before a yielded try block", () => {
  const events = [];
  const obj = {
    *numbers() {
      try {
        events.push("before");
        yield 1;
        events.push("after");
      } finally {
        events.push("finally");
      }
    },
  };

  const iter = obj.numbers();
  expect(iter.next()).toEqual({ value: 1, done: false });
  expect(iter.return(9)).toEqual({ value: 9, done: true });
  expect(events).toEqual(["before", "finally"]);
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

test("object generator method throw does not replay side effects before a yielded try block", () => {
  const events = [];
  const obj = {
    *numbers() {
      try {
        events.push("before");
        yield 1;
      } catch (value) {
        events.push("catch");
        yield value;
      }
    },
  };

  const iter = obj.numbers();
  expect(iter.next()).toEqual({ value: 1, done: false });
  expect(iter.throw(7)).toEqual({ value: 7, done: false });
  expect(events).toEqual(["before", "catch"]);
});

test("object generator method uncaught throw closes generator", () => {
  const obj = {
    *numbers() {
      yield 1;
      throw 7;
    },
  };

  const iter = obj.numbers();
  expect(iter.next()).toEqual({ value: 1, done: false });
  expect(() => iter.next()).toThrow();
  expect(iter.next()).toEqual({ value: undefined, done: true });
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
