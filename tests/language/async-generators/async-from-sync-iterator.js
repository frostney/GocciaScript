/*---
description: Async-from-Sync iterator PromiseResolve and return argument semantics
features: [async-generators]
---*/

const poisonedPromise = (reason) => {
  const promise = Promise.resolve("unreachable");
  Object.defineProperty(promise, "constructor", {
    get() {
      throw reason;
    },
  });
  return promise;
};

test("async-from-sync next closes a generator when PromiseResolve is abrupt", async () => {
  const reason = {};
  let finallyCount = 0;

  const values = ({
    *values() {
      try {
        yield poisonedPromise(reason);
      } finally {
        finallyCount += 1;
      }
    },
  }).values;

  const delegate = ({
    async *delegate() {
      yield* values();
    },
  }).delegate;

  await expect(delegate().next()).rejects.toBe(reason);
  expect(finallyCount).toBe(1);
});

test("async-from-sync next closes a generic iterator when PromiseResolve is abrupt", async () => {
  const reason = {};
  let returnCount = 0;
  const source = {
    [Symbol.iterator]() {
      return {
        next() {
          return { value: poisonedPromise(reason), done: false };
        },
        return() {
          returnCount += 1;
          return { value: undefined, done: true };
        },
      };
    },
  };

  const delegate = ({
    async *delegate() {
      yield* source;
    },
  }).delegate;

  await expect(delegate().next()).rejects.toBe(reason);
  expect(returnCount).toBe(1);
});

test("async-from-sync throw closes the iterator when PromiseResolve is abrupt", async () => {
  const reason = {};
  let returnCount = 0;
  const source = {
    [Symbol.iterator]() {
      return {
        next() {
          return { value: 1, done: false };
        },
        throw() {
          return { value: poisonedPromise(reason), done: false };
        },
        return() {
          returnCount += 1;
          return { value: undefined, done: true };
        },
      };
    },
  };

  const delegate = ({
    async *delegate() {
      return yield* source;
    },
  }).delegate;

  const iterator = delegate();
  await expect(iterator.next()).resolves.toEqual({ value: 1, done: false });
  await expect(iterator.throw("unused")).rejects.toBe(reason);
  expect(returnCount).toBe(1);
  await expect(iterator.next()).resolves.toEqual({ value: undefined, done: true });
});

test("async-from-sync return preserves an absent value argument", async () => {
  let returnArgumentsLength = -1;
  const source = {
    [Symbol.iterator]() {
      return this;
    },
    next() {
      return { value: 1, done: false };
    },
    return(...args) {
      returnArgumentsLength = args.length;
      return { value: undefined, done: true };
    },
  };

  for await (const value of source) {
    expect(value).toBe(1);
    break;
  }

  expect(returnArgumentsLength).toBe(0);
});

test("async-from-sync next forwards the value yield* sends to the sync iterator", async () => {
  const received = [];
  const sync = {
    [Symbol.iterator]() {
      return this;
    },
    next(...args) {
      received.push(args.length === 0 ? "absent" : args[0]);
      return { value: received.length, done: received.length > 2 };
    },
  };
  const generator = ({
    async *delegate() {
      return yield* sync;
    },
  }).delegate();

  expect(await generator.next("first")).toEqual({ value: 1, done: false });
  expect(await generator.next("second")).toEqual({ value: 2, done: false });
  expect(await generator.next("third")).toEqual({ value: 3, done: true });
  // The first step's argument is the undefined yield* starts with; the
  // values next sends after that reach the sync iterator.
  expect(received.slice(1)).toEqual(["second", "third"]);
});

test("yield* over an array ignores the value next sends", async () => {
  const generator = ({
    async *delegate() {
      return yield* [10, Promise.resolve(20), 30];
    },
  }).delegate();

  expect(await generator.next("ignored")).toEqual({ value: 10, done: false });
  expect(await generator.next("ignored")).toEqual({ value: 20, done: false });
  expect(await generator.next("ignored")).toEqual({ value: 30, done: false });
  expect(await generator.next("ignored")).toEqual({
    value: undefined,
    done: true,
  });
});
