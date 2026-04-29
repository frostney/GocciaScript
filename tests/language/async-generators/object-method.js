/*---
description: Object async generator methods
features: [async-generators]
---*/

test("object async generator method works without compat-function", async () => {
  const obj = {
    async *numbers() {
      yield await Promise.resolve(1);
      yield 2;
    },
  };

  const seen = [];
  for await (const value of obj.numbers()) {
    seen.push(value);
  }
  expect(seen).toEqual([1, 2]);
});

test("object async generator method does not replay call arguments before a yielded argument", async () => {
  const events = [];
  const obj = {
    async *values() {
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
  await expect(iter.next()).resolves.toEqual({ value: "resume", done: false });
  expect(events).toEqual(["left"]);
  await expect(iter.next(5)).resolves.toEqual({ value: 15, done: false });
  expect(events).toEqual(["left", "combine"]);
});

test("object async generator method preserves for-await iterator across yielded loop body", async () => {
  const obj = {
    async *values() {
      for await (const n of [1, 2, 3]) yield n;
    },
  };

  const iter = obj.values();
  await expect(iter.next()).resolves.toEqual({ value: 1, done: false });
  await expect(iter.next()).resolves.toEqual({ value: 2, done: false });
  await expect(iter.next()).resolves.toEqual({ value: 3, done: false });
  await expect(iter.next()).resolves.toEqual({ value: undefined, done: true });
});

test("object async generator method preserves for-await iterator across yielded loop head", async () => {
  const obj = {
    async *values() {
      for await (const [value = yield "default"] of [[undefined], [2]]) {
        yield value;
      }
    },
  };

  const iter = obj.values();
  await expect(iter.next()).resolves.toEqual({ value: "default", done: false });
  await expect(iter.next(1)).resolves.toEqual({ value: 1, done: false });
  await expect(iter.next()).resolves.toEqual({ value: 2, done: false });
  await expect(iter.next()).resolves.toEqual({ value: undefined, done: true });
});

test("object async generator method preserves async iterator across yielded for-await body", async () => {
  const source = {
    [Symbol.asyncIterator]() {
      let index = 0;
      return {
        next() {
          index = index + 1;
          if (index > 3) return Promise.resolve({ value: undefined, done: true });
          return Promise.resolve({ value: index, done: false });
        },
      };
    },
  };
  const obj = {
    async *values() {
      for await (const n of source) yield n;
    },
  };

  const iter = obj.values();
  await expect(iter.next()).resolves.toEqual({ value: 1, done: false });
  await expect(iter.next()).resolves.toEqual({ value: 2, done: false });
  await expect(iter.next()).resolves.toEqual({ value: 3, done: false });
  await expect(iter.next()).resolves.toEqual({ value: undefined, done: true });
});

test("object async generator method preserves async iterator across yielded for-await head", async () => {
  const source = {
    [Symbol.asyncIterator]() {
      let index = 0;
      return {
        next() {
          index = index + 1;
          if (index === 1) return Promise.resolve({ value: [undefined], done: false });
          if (index === 2) return Promise.resolve({ value: [2], done: false });
          return Promise.resolve({ value: undefined, done: true });
        },
      };
    },
  };
  const obj = {
    async *values() {
      for await (const [value = yield "default"] of source) {
        yield value;
      }
    },
  };

  const iter = obj.values();
  await expect(iter.next()).resolves.toEqual({ value: "default", done: false });
  await expect(iter.next(1)).resolves.toEqual({ value: 1, done: false });
  await expect(iter.next()).resolves.toEqual({ value: 2, done: false });
  await expect(iter.next()).resolves.toEqual({ value: undefined, done: true });
});

test("object async generator method delegates to async iterable", async () => {
  const source = {
    async *values() {
      yield 1;
      yield 2;
    },
  };
  const obj = {
    async *numbers() {
      yield* source.values();
    },
  };
  const seen = [];

  for await (const value of obj.numbers()) {
    seen.push(value);
  }

  expect(seen).toEqual([1, 2]);
});

test("object async generator yield delegation does not replay side effects before the delegation", async () => {
  const events = [];
  const obj = {
    async *numbers() {
      events.push("before");
      yield* [1, 2];
      events.push("after");
    },
  };

  const iter = obj.numbers();
  await expect(iter.next()).resolves.toEqual({ value: 1, done: false });
  expect(events).toEqual(["before"]);
  await expect(iter.next()).resolves.toEqual({ value: 2, done: false });
  expect(events).toEqual(["before"]);
  await expect(iter.next()).resolves.toEqual({ value: undefined, done: true });
  expect(events).toEqual(["before", "after"]);
});

test("object async generator method delegates to sync iterable", async () => {
  const obj = {
    async *numbers() {
      yield* [Promise.resolve(1), 2];
    },
  };
  const seen = [];

  for await (const value of obj.numbers()) {
    seen.push(value);
  }

  expect(seen).toEqual([1, 2]);
});

test("object async generator yield delegation keeps sync iterator alive across return cleanup yield", async () => {
  const events = [];
  const source = {
    *values() {
      try {
        yield 1;
      } finally {
        events.push("finally");
        yield Promise.resolve("cleanup");
        events.push("after");
      }
    },
  };
  const obj = {
    async *values() {
      yield* source.values();
    },
  };

  const iter = obj.values();
  await expect(iter.next()).resolves.toEqual({ value: 1, done: false });
  await expect(iter.return(9)).resolves.toEqual({ value: "cleanup", done: false });
  expect(events).toEqual(["finally"]);
  await expect(iter.next()).resolves.toEqual({ value: undefined, done: true });
  expect(events).toEqual(["finally", "after"]);
});

test("object async generator yield delegation awaits sync iterator return and throw values", async () => {
  const returnSource = {
    [Symbol.iterator]() {
      return {
        next() {
          return { value: 1, done: false };
        },
        return(value) {
          return { value: Promise.resolve("return:" + value), done: true };
        },
      };
    },
  };
  const throwSource = {
    [Symbol.iterator]() {
      return {
        next() {
          return { value: 1, done: false };
        },
        throw(value) {
          return { value: Promise.resolve("throw:" + value), done: false };
        },
      };
    },
  };
  const obj = {
    async *returning() {
      yield* returnSource;
    },
    async *throwing() {
      yield* throwSource;
    },
  };

  const returnIter = obj.returning();
  await expect(returnIter.next()).resolves.toEqual({ value: 1, done: false });
  await expect(returnIter.return(9)).resolves.toEqual({ value: "return:9", done: true });

  const throwIter = obj.throwing();
  await expect(throwIter.next()).resolves.toEqual({ value: 1, done: false });
  await expect(throwIter.throw(9)).resolves.toEqual({ value: "throw:9", done: false });
});

test("object async generator yield delegation awaits short-circuit sync iterator return value", async () => {
  const source = {
    [Symbol.iterator]() {
      return {
        next() {
          return { value: 1, done: false };
        },
      };
    },
  };
  const obj = {
    async *values() {
      yield* source;
    },
  };

  const iter = obj.values();
  await expect(iter.next()).resolves.toEqual({ value: 1, done: false });
  await expect(iter.return(Promise.resolve(9))).resolves.toEqual({ value: 9, done: true });
});

test("object async generator yield delegation rejects sync iterator failures asynchronously", async () => {
  const nextSource = {
    [Symbol.iterator]() {
      return {
        next() {
          throw "next boom";
        },
      };
    },
  };
  const returnSource = {
    [Symbol.iterator]() {
      return {
        next() {
          return { value: 1, done: false };
        },
        return() {
          throw "return boom";
        },
      };
    },
  };
  const throwSource = {
    [Symbol.iterator]() {
      return {
        next() {
          return { value: 1, done: false };
        },
        throw() {
          throw "throw boom";
        },
      };
    },
  };
  const rejectedValueEvents = [];
  const resultGetterEvents = [];
  const rejectedThrowValueEvents = [];
  const rejectedReturnValueEvents = [];
  const rejectedThenGetterEvents = [];
  const rejectedValueSource = {
    [Symbol.iterator]() {
      return {
        next() {
          rejectedValueEvents.push("next");
          return { value: Promise.reject("value boom"), done: false };
        },
        return() {
          rejectedValueEvents.push("return");
          return { value: "closed", done: true };
        },
      };
    },
  };
  const resultGetterSource = {
    [Symbol.iterator]() {
      return {
        next() {
          resultGetterEvents.push("next");
          return {
            get value() {
              return missingValue;
            },
            done: false,
          };
        },
        return() {
          resultGetterEvents.push("return");
          return { value: "closed", done: true };
        },
      };
    },
  };
  const rejectedThrowValueSource = {
    [Symbol.iterator]() {
      return {
        next() {
          rejectedThrowValueEvents.push("next");
          return { value: 1, done: false };
        },
        throw(value) {
          rejectedThrowValueEvents.push("throw:" + value);
          return { value: Promise.reject("throw value boom"), done: false };
        },
        return() {
          rejectedThrowValueEvents.push("return");
          return { value: "closed", done: true };
        },
      };
    },
  };
  const rejectedReturnValueSource = {
    [Symbol.iterator]() {
      return {
        next() {
          rejectedReturnValueEvents.push("next");
          return { value: 1, done: false };
        },
        return(value) {
          rejectedReturnValueEvents.push("return:" + value);
          return { value: Promise.reject("return value boom"), done: false };
        },
      };
    },
  };
  const rejectedThenGetterSource = {
    [Symbol.iterator]() {
      return {
        next() {
          rejectedThenGetterEvents.push("next");
          return {
            value: {
              get then() {
                rejectedThenGetterEvents.push("then");
                throw "then getter boom";
              },
            },
            done: false,
          };
        },
        return() {
          rejectedThenGetterEvents.push("return");
          return { value: "closed", done: true };
        },
      };
    },
  };
  const obj = {
    async *nexting() {
      yield* nextSource;
    },
    async *returning() {
      yield* returnSource;
    },
    async *throwing() {
      yield* throwSource;
    },
    async *rejectingValue() {
      yield* rejectedValueSource;
    },
    async *throwingFromResultGetter() {
      yield* resultGetterSource;
    },
    async *rejectingThrowValue() {
      yield* rejectedThrowValueSource;
    },
    async *rejectingReturnValue() {
      yield* rejectedReturnValueSource;
    },
    async *rejectingThenGetter() {
      yield* rejectedThenGetterSource;
    },
  };

  await expect(obj.nexting().next()).rejects.toBe("next boom");

  const returnIter = obj.returning();
  await expect(returnIter.next()).resolves.toEqual({ value: 1, done: false });
  await expect(returnIter.return(9)).rejects.toBe("return boom");

  const throwIter = obj.throwing();
  await expect(throwIter.next()).resolves.toEqual({ value: 1, done: false });
  await expect(throwIter.throw(9)).rejects.toBe("throw boom");

  const rejectingValueIter = obj.rejectingValue();
  await expect(rejectingValueIter.next()).rejects.toBe("value boom");
  expect(rejectedValueEvents).toEqual(["next", "return"]);
  await expect(rejectingValueIter.next()).resolves.toEqual({ value: undefined, done: true });
  await expect(obj.throwingFromResultGetter().next()).rejects.toThrow(ReferenceError);
  expect(resultGetterEvents).toEqual(["next"]);

  const rejectedThrowValueIter = obj.rejectingThrowValue();
  await expect(rejectedThrowValueIter.next()).resolves.toEqual({ value: 1, done: false });
  await expect(rejectedThrowValueIter.throw(9)).rejects.toBe("throw value boom");
  expect(rejectedThrowValueEvents).toEqual(["next", "throw:9", "return"]);
  await expect(rejectedThrowValueIter.next()).resolves.toEqual({ value: undefined, done: true });

  const rejectedReturnValueIter = obj.rejectingReturnValue();
  await expect(rejectedReturnValueIter.next()).resolves.toEqual({ value: 1, done: false });
  await expect(rejectedReturnValueIter.return(9)).rejects.toBe("return value boom");
  expect(rejectedReturnValueEvents).toEqual(["next", "return:9"]);

  await expect(obj.rejectingThenGetter().next()).rejects.toBe("then getter boom");
  expect(rejectedThenGetterEvents).toEqual(["next", "then", "return"]);
});

test("object async generator yield delegation closes sync wrapper after completion", async () => {
  const events = [];
  const source = {
    [Symbol.iterator]() {
      let step = 0;
      return {
        next() {
          events.push("next");
          step += 1;
          if (step === 1) {
            return { value: Promise.resolve(1), done: false };
          }
          return { value: Promise.resolve("done"), done: true };
        },
        return() {
          events.push("return");
          return { value: "returned", done: true };
        },
        throw() {
          events.push("throw");
          return { value: "thrown", done: true };
        },
      };
    },
  };
  const obj = {
    async *values() {
      return yield* source;
    },
  };

  const iter = obj.values();
  await expect(iter.next()).resolves.toEqual({ value: 1, done: false });
  await expect(iter.next()).resolves.toEqual({ value: "done", done: true });
  await expect(iter.return(9)).resolves.toEqual({ value: 9, done: true });
  await expect(iter.throw(10)).rejects.toBe(10);
  expect(events).toEqual(["next", "next"]);
});

test("object async generator yield delegation rejects non-callable async next", async () => {
  const source = {
    [Symbol.asyncIterator]() {
      return {};
    },
  };
  const obj = {
    async *values() {
      yield* source;
    },
  };

  await expect(obj.values().next()).rejects.toThrow(TypeError);
});

test("object async generator return and throw use promises", async () => {
  const obj = {
    async *numbers() {
      yield 1;
    },
    async *fails() {
      throw 5;
    },
  };

  await expect(obj.numbers().return(9)).resolves.toEqual({ value: 9, done: true });
  await expect(obj.numbers().next()).resolves.toEqual({ value: 1, done: false });
  const iter = obj.numbers();
  await expect(iter.next()).resolves.toEqual({ value: 1, done: false });
  await expect(iter.next(9)).resolves.toEqual({ value: undefined, done: true });
  await expect(obj.fails().next()).rejects.toBe(5);
});
