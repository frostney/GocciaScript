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

test("object generator method preserves completed siblings across multiple yields in one statement", () => {
  const events = [];
  const obj = {
    *values() {
      const left = () => {
        events.push("left");
        return 1;
      };
      const combine = (a, b, c) => {
        events.push("combine");
        return a + b + c;
      };

      const result = combine(left(), yield "a", yield "b");
      yield result;
    },
  };

  const iter = obj.values();
  expect(iter.next()).toEqual({ value: "a", done: false });
  expect(events).toEqual(["left"]);
  expect(iter.next(2)).toEqual({ value: "b", done: false });
  expect(events).toEqual(["left"]);
  expect(iter.next(3)).toEqual({ value: 6, done: false });
  expect(events).toEqual(["left", "combine"]);
});

test("object generator method does not reuse completed loop body expressions", () => {
  const obj = {
    *values() {
      let total = 0;

      for (const n of [1, 2, 3]) total = total + n;

      yield total;
    },
  };

  const iter = obj.values();
  expect(iter.next()).toEqual({ value: 6, done: false });
  expect(iter.next()).toEqual({ value: undefined, done: true });
});

test("object generator method preserves for-of iterator across yielded loop body", () => {
  const obj = {
    *values() {
      for (const n of [1, 2, 3]) yield n;
    },
  };

  const iter = obj.values();
  expect(iter.next()).toEqual({ value: 1, done: false });
  expect(iter.next()).toEqual({ value: 2, done: false });
  expect(iter.next()).toEqual({ value: 3, done: false });
  expect(iter.next()).toEqual({ value: undefined, done: true });
});

test("object generator method preserves for-of iterator across yielded loop head", () => {
  const obj = {
    *values() {
      for (const [value = yield "default"] of [[undefined], [2]]) {
        yield value;
      }
    },
  };

  const iter = obj.values();
  expect(iter.next()).toEqual({ value: "default", done: false });
  expect(iter.next(1)).toEqual({ value: 1, done: false });
  expect(iter.next()).toEqual({ value: 2, done: false });
  expect(iter.next()).toEqual({ value: undefined, done: true });
});

test("object generator method resumes binary expressions instead of returning the cached left operand", () => {
  const events = [];
  const obj = {
    *values() {
      const left = () => {
        events.push("left");
        return 10;
      };

      const result = left() + (yield "resume");
      yield result;
    },
  };

  const iter = obj.values();
  expect(iter.next()).toEqual({ value: "resume", done: false });
  expect(events).toEqual(["left"]);
  expect(iter.next(5)).toEqual({ value: 15, done: false });
  expect(events).toEqual(["left"]);
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

test("object generator yield delegation clears delegate after protocol error", () => {
  const events = [];
  const primitiveResult = {
    [Symbol.iterator]() {
      return {
        next() {
          events.push("bad-next");
          return 1;
        },
      };
    },
  };
  const missingNext = {
    [Symbol.iterator]() {
      return {};
    },
  };
  const obj = {
    *primitiveThenGood() {
      try {
        yield* primitiveResult;
      } catch (error) {
        events.push(error instanceof TypeError ? "caught-primitive" : "wrong");
      }
      yield* ["ok"];
    },
    *missingThenGood() {
      try {
        yield* missingNext;
      } catch (error) {
        events.push(error instanceof TypeError ? "caught-missing" : "wrong");
      }
      yield* ["done"];
    },
  };

  const primitiveIter = obj.primitiveThenGood();
  expect(primitiveIter.next()).toEqual({ value: "ok", done: false });
  expect(primitiveIter.next()).toEqual({ value: undefined, done: true });

  const missingIter = obj.missingThenGood();
  expect(missingIter.next()).toEqual({ value: "done", done: false });
  expect(missingIter.next()).toEqual({ value: undefined, done: true });
  expect(events).toEqual([
    "bad-next",
    "caught-primitive",
    "caught-missing",
  ]);
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

test("object generator yield delegation forwards next values to delegated generators", () => {
  const inner = {
    *values() {
      const value = yield 1;
      yield value;
    },
  };
  const outer = {
    *values() {
      yield* inner.values();
    },
  };

  const iter = outer.values();
  expect(iter.next()).toEqual({ value: 1, done: false });
  expect(iter.next(42)).toEqual({ value: 42, done: false });
});

test("object generator yield delegation forwards return values and runs delegated finally", () => {
  const events = [];
  const inner = {
    *values() {
      try {
        yield 1;
      } finally {
        events.push("finally");
      }
    },
  };
  const outer = {
    *values() {
      yield* inner.values();
    },
  };

  const iter = outer.values();
  expect(iter.next()).toEqual({ value: 1, done: false });
  expect(iter.return(9)).toEqual({ value: 9, done: true });
  expect(events).toEqual(["finally"]);
});

test("object generator yield delegation preserves delegated finally yields on return", () => {
  const events = [];
  const inner = {
    *values() {
      try {
        yield 1;
      } finally {
        events.push("finally");
        yield "cleanup";
      }
    },
  };
  const outer = {
    *values() {
      yield* inner.values();
    },
  };

  const iter = outer.values();
  expect(iter.next()).toEqual({ value: 1, done: false });
  expect(iter.return(9)).toEqual({ value: "cleanup", done: false });
  expect(events).toEqual(["finally"]);
  expect(iter.next()).toEqual({ value: undefined, done: true });
});

test("object generator yield delegation rejects non-callable return and throw", () => {
  const nonCallableReturn = {
    [Symbol.iterator]() {
      return {
        next() {
          return { value: 1, done: false };
        },
        return: 1,
      };
    },
  };
  const nonCallableThrow = {
    [Symbol.iterator]() {
      return {
        next() {
          return { value: 1, done: false };
        },
        throw: 1,
        return() {
          return { value: "closed", done: true };
        },
      };
    },
  };
  const obj = {
    *returning() {
      yield* nonCallableReturn;
    },
    *throwing() {
      yield* nonCallableThrow;
    },
  };

  const returnIter = obj.returning();
  expect(returnIter.next()).toEqual({ value: 1, done: false });
  expect(() => returnIter.return(9)).toThrow(TypeError);

  const throwIter = obj.throwing();
  expect(throwIter.next()).toEqual({ value: 1, done: false });
  expect(() => throwIter.throw(9)).toThrow(TypeError);
});

test("object generator yield delegation closes delegate when throw is missing", () => {
  const events = [];
  const source = {
    [Symbol.iterator]() {
      return {
        next() {
          return { value: 1, done: false };
        },
        return() {
          events.push("return");
          return { value: "closed", done: true };
        },
      };
    },
  };
  const obj = {
    *values() {
      yield* source;
    },
  };

  const iter = obj.values();
  expect(iter.next()).toEqual({ value: 1, done: false });
  expect(() => iter.throw(9)).toThrow(TypeError);
  expect(events).toEqual(["return"]);
});

test("object generator yield delegation closes after next throws", () => {
  const events = [];
  const source = {
    [Symbol.iterator]() {
      return {
        next() {
          events.push("bad-next");
          throw "boom";
        },
      };
    },
  };
  const obj = {
    *values() {
      yield* source;
    },
  };

  const iter = obj.values();
  expect(() => iter.next()).toThrow();
  expect(iter.next()).toEqual({ value: undefined, done: true });
  expect(events).toEqual(["bad-next"]);
});

test("object generator yield delegation closes after throw method throws", () => {
  const events = [];
  const source = {
    [Symbol.iterator]() {
      return {
        next() {
          return { value: 1, done: false };
        },
        throw(value) {
          events.push("throw:" + value);
          throw "boom";
        },
      };
    },
  };
  const obj = {
    *values() {
      yield* source;
    },
  };

  const iter = obj.values();
  expect(iter.next()).toEqual({ value: 1, done: false });
  expect(() => iter.throw(9)).toThrow();
  expect(iter.next()).toEqual({ value: undefined, done: true });
  expect(events).toEqual(["throw:9"]);
});

test("object generator yield delegation closes after return method throws", () => {
  const events = [];
  const source = {
    [Symbol.iterator]() {
      return {
        next() {
          return { value: 1, done: false };
        },
        return(value) {
          events.push("return:" + value);
          throw "boom";
        },
      };
    },
  };
  const obj = {
    *values() {
      yield* source;
    },
  };

  const iter = obj.values();
  expect(iter.next()).toEqual({ value: 1, done: false });
  expect(() => iter.return(9)).toThrow();
  expect(iter.next()).toEqual({ value: undefined, done: true });
  expect(events).toEqual(["return:9"]);
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

test("object generator method finally yield preserves pending return without replaying try", () => {
  const events = [];
  const obj = {
    *values() {
      try {
        events.push("try");
        return 7;
      } finally {
        events.push("finally");
        yield "cleanup";
      }
    },
  };

  const iter = obj.values();
  expect(iter.next()).toEqual({ value: "cleanup", done: false });
  expect(events).toEqual(["try", "finally"]);
  expect(iter.next()).toEqual({ value: 7, done: true });
  expect(events).toEqual(["try", "finally"]);
});

test("object generator method finally yield preserves pending throw without replaying try", () => {
  const events = [];
  const obj = {
    *values() {
      try {
        events.push("try");
        throw 7;
      } finally {
        events.push("finally");
        yield "cleanup";
      }
    },
  };

  const iter = obj.values();
  expect(iter.next()).toEqual({ value: "cleanup", done: false });
  expect(events).toEqual(["try", "finally"]);
  expect(() => iter.next()).toThrow();
  expect(events).toEqual(["try", "finally"]);
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
