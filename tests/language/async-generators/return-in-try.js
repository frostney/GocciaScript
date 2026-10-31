/*---
description: In an async generator, `return <expr>` awaits its operand before the completion leaves an enclosing try (ES2026 §14.10.1 ReturnStatement Evaluation, step 3)
features: [async-generators]
---*/

test("catch sees a rejection of the returned value", async () => {
  const helpers = {
    async *gen() {
      try {
        return Promise.reject(7);
      } catch (e) {
        yield "caught " + e;
      }
    },
  };
  const iter = helpers.gen();
  expect(await iter.next()).toEqual({ value: "caught 7", done: false });
  expect(await iter.next()).toEqual({ value: undefined, done: true });
});

test("catch sees a thenable that rejects", async () => {
  const thenable = {
    then(resolve, reject) {
      reject("thenable error");
    },
  };
  const helpers = {
    async *gen() {
      try {
        return thenable;
      } catch (e) {
        return "caught " + e;
      }
    },
  };
  expect(await helpers.gen().next()).toEqual({
    value: "caught thenable error",
    done: true,
  });
});

test("finally runs after a plain returned value is awaited", async () => {
  const log = [];
  const thenable = {
    then(resolve) {
      log.push("then");
      resolve(3);
    },
  };
  const helpers = {
    async *plain() {
      try {
        return 3;
      } finally {
        log.push("finally");
      }
    },
    async *viaThenable() {
      try {
        return thenable;
      } finally {
        log.push("finally");
      }
    },
  };
  Promise.resolve().then(() => log.push("earlier job"));
  const result = helpers.plain().next();
  log.push("next returned");
  expect(await result).toEqual({ value: 3, done: true });
  expect(log).toEqual(["next returned", "earlier job", "finally"]);

  log.length = 0;
  expect(await helpers.viaThenable().next()).toEqual({ value: 3, done: true });
  expect(log).toEqual(["then", "finally"]);
});

test("finally runs after the returned promise settles", async () => {
  const log = [];
  let release;
  const pending = new Promise((resolve) => {
    release = resolve;
  });
  const helpers = {
    async *gen() {
      try {
        return pending.then((v) => {
          log.push("settled");
          return v;
        });
      } finally {
        log.push("finally");
      }
    },
  };
  const result = helpers.gen().next();
  await Promise.resolve();
  log.push("released");
  release("done");
  expect(await result).toEqual({ value: "done", done: true });
  expect(log).toEqual(["released", "settled", "finally"]);
});

test("finally runs after a rejected value is awaited and its rejection propagates", async () => {
  const log = [];
  const thenable = {
    then(resolve, reject) {
      log.push("then");
      reject("boom");
    },
  };
  const helpers = {
    async *gen() {
      try {
        return thenable;
      } finally {
        log.push("finally");
      }
    },
  };
  const iter = helpers.gen();
  let caught;
  try {
    await iter.next();
  } catch (e) {
    caught = e;
  }
  expect(caught).toBe("boom");
  expect(log).toEqual(["then", "finally"]);
  expect(await iter.next()).toEqual({ value: undefined, done: true });
});

test("a return in finally overrides the rejection of the returned value", async () => {
  const helpers = {
    async *gen() {
      try {
        return Promise.reject("ignored");
      } finally {
        return "from finally";
      }
    },
  };
  expect(await helpers.gen().next()).toEqual({ value: "from finally", done: true });
});

test("nested try statements see the rejection innermost first and run finally blocks in order", async () => {
  const log = [];
  const helpers = {
    async *gen() {
      try {
        try {
          try {
            return Promise.reject("inner");
          } finally {
            log.push("finally 1");
          }
        } catch (e) {
          log.push("catch " + e);
          return Promise.resolve("recovered");
        } finally {
          log.push("finally 2");
        }
      } finally {
        log.push("finally 3");
      }
    },
  };
  expect(await helpers.gen().next()).toEqual({ value: "recovered", done: true });
  expect(log).toEqual(["finally 1", "catch inner", "finally 2", "finally 3"]);
});

test("a for-of loop's iterator is closed after the returned value is awaited", async () => {
  const log = [];
  const iterable = {
    [Symbol.iterator]() {
      return {
        next() {
          return { value: 1, done: false };
        },
        return() {
          log.push("iterator return");
          return {};
        },
      };
    },
  };
  const thenable = {
    then(resolve) {
      log.push("then");
      resolve("value");
    },
  };
  const helpers = {
    async *gen() {
      for (const x of iterable) {
        return thenable;
      }
    },
  };
  expect(await helpers.gen().next()).toEqual({ value: "value", done: true });
  expect(log).toEqual(["then", "iterator return"]);
});

test("a rejection of the returned value inside for-of still closes the iterator", async () => {
  const log = [];
  const iterable = {
    [Symbol.iterator]() {
      return {
        next() {
          return { value: 1, done: false };
        },
        return() {
          log.push("iterator return");
          return {};
        },
      };
    },
  };
  const thenable = {
    then(resolve, reject) {
      log.push("then");
      reject("loop error");
    },
  };
  const helpers = {
    async *gen() {
      for (const x of iterable) {
        return thenable;
      }
    },
  };
  let caught;
  try {
    await helpers.gen().next();
  } catch (e) {
    caught = e;
  }
  expect(caught).toBe("loop error");
  expect(log).toEqual(["then", "iterator return"]);
});

test("a for await loop's iterator is closed after the returned value is awaited", async () => {
  const log = [];
  const iterable = {
    [Symbol.asyncIterator]() {
      return {
        next() {
          return Promise.resolve({ value: 1, done: false });
        },
        return() {
          log.push("iterator return");
          return Promise.resolve({ done: true });
        },
      };
    },
  };
  const thenable = {
    then(resolve) {
      log.push("then");
      resolve("value");
    },
  };
  const helpers = {
    async *gen() {
      for await (const x of iterable) {
        return thenable;
      }
    },
  };
  expect(await helpers.gen().next()).toEqual({ value: "value", done: true });
  expect(log).toEqual(["then", "iterator return"]);
});

test("a return without an operand runs finally without awaiting", async () => {
  const log = [];
  const helpers = {
    async *gen() {
      try {
        return;
      } finally {
        log.push("finally");
      }
    },
  };
  expect(await helpers.gen().next()).toEqual({ value: undefined, done: true });
  expect(log).toEqual(["finally"]);
});

test("a returned value outside any try is awaited once", async () => {
  const helpers = {
    async *resolved() {
      return Promise.resolve("resolved");
    },
    async *rejected() {
      return Promise.reject("rejected");
    },
  };
  expect(await helpers.resolved().next()).toEqual({ value: "resolved", done: true });
  let caught;
  try {
    await helpers.rejected().next();
  } catch (e) {
    caught = e;
  }
  expect(caught).toBe("rejected");
});

test("sync generators and async functions keep their return order", async () => {
  const log = [];
  const thenable = {
    then(resolve) {
      log.push("then");
      resolve("value");
    },
  };
  const helpers = {
    *syncGen() {
      try {
        return thenable;
      } finally {
        log.push("finally");
      }
    },
    async asyncFn() {
      try {
        return thenable;
      } finally {
        log.push("finally");
      }
    },
  };
  const syncResult = helpers.syncGen().next();
  expect(syncResult.value).toBe(thenable);
  expect(syncResult.done).toBe(true);
  expect(log).toEqual(["finally"]);

  log.length = 0;
  expect(await helpers.asyncFn()).toBe("value");
  expect(log).toEqual(["finally", "then"]);
});
