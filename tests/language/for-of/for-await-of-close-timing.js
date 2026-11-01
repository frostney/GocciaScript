/*---
description: An early exit from for-await-of closes the iterator with ES2026 §7.4.14 AsyncIteratorClose, which awaits return()'s result as a suspension and skips the Await when there is no return method
features: [for-of, async-await, async-generators]
---*/

// Logs "t1", "t2", ... one job apart, so a log records the job each step ran in.
const ticker = (log, count) => {
  let chain = Promise.resolve();
  for (const i of Array.from({ length: count }, (_, j) => j + 1)) {
    chain = chain.then(() => {
      log.push("t" + i);
    });
  }
  return chain;
};

describe("for-await-of close timing", () => {
  test("break with no return method continues in the same job", async () => {
    const log = [];
    const iterator = {
      [Symbol.asyncIterator]() {
        return this;
      },
      next() {
        return { value: 1, done: false };
      },
    };
    const loop = (async () => {
      for await (const x of iterator) {
        log.push("body");
        break;
      }
      log.push("after");
    })();
    await Promise.all([loop, ticker(log, 3)]);
    expect(log).toEqual(["body", "after", "t1", "t2", "t3"]);
  });

  test("a throw with no return method reaches the catch in the same job", async () => {
    const log = [];
    const iterator = {
      [Symbol.asyncIterator]() {
        return this;
      },
      next() {
        return { value: 1, done: false };
      },
    };
    const loop = (async () => {
      try {
        for await (const x of iterator) {
          log.push("body");
          throw "e";
        }
      } catch (e) {
        log.push("caught:" + e);
      }
    })();
    await Promise.all([loop, ticker(log, 3)]);
    expect(log).toEqual(["body", "caught:e", "t1", "t2", "t3"]);
  });

  test("a null return method counts as none", async () => {
    const log = [];
    const iterator = {
      [Symbol.asyncIterator]() {
        return this;
      },
      next() {
        return { value: 1, done: false };
      },
      return: null,
    };
    const loop = (async () => {
      for await (const x of iterator) {
        log.push("body");
        break;
      }
      log.push("after");
    })();
    await Promise.all([loop, ticker(log, 3)]);
    expect(log).toEqual(["body", "after", "t1", "t2", "t3"]);
  });

  test("break awaits an async generator's return before continuing", async () => {
    const log = [];
    const helpers = {
      async *values() {
        try {
          yield 1;
          yield 2;
        } finally {
          log.push("finally");
        }
      },
    };
    const loop = (async () => {
      for await (const x of helpers.values()) {
        log.push("body:" + x);
        break;
      }
      log.push("after");
    })();
    await Promise.all([loop, ticker(log, 5)]);
    expect(log).toEqual([
      "t1",
      "body:1",
      "t2",
      "finally",
      "t3",
      "after",
      "t4",
      "t5",
    ]);
  });

  test("return from the loop awaits the close before the function settles", async () => {
    const log = [];
    const helpers = {
      async *values() {
        try {
          yield 1;
        } finally {
          log.push("finally");
        }
      },
    };
    const run = async () => {
      for await (const x of helpers.values()) {
        log.push("body");
        return "rv";
      }
    };
    const loop = run().then((v) => log.push("result:" + v));
    await Promise.all([loop, ticker(log, 6)]);
    expect(log).toEqual([
      "t1",
      "body",
      "t2",
      "finally",
      "t3",
      "t4",
      "result:rv",
      "t5",
      "t6",
    ]);
  });

  test("break awaits a sync iterator's return through the async-from-sync iterator", async () => {
    const log = [];
    let count = 0;
    const iterator = {
      [Symbol.iterator]() {
        return this;
      },
      next() {
        count++;
        log.push("next" + count);
        return { value: count, done: false };
      },
      return() {
        log.push("return");
        return {};
      },
    };
    const loop = (async () => {
      for await (const x of iterator) {
        log.push("body:" + x);
        break;
      }
      log.push("after");
    })();
    await Promise.all([loop, ticker(log, 4)]);
    expect(log).toEqual([
      "next1",
      "t1",
      "body:1",
      "return",
      "t2",
      "t3",
      "after",
      "t4",
    ]);
  });

  test("a throw awaits a sync iterator's return before the catch", async () => {
    const log = [];
    let count = 0;
    const iterator = {
      [Symbol.iterator]() {
        return this;
      },
      next() {
        count++;
        log.push("next" + count);
        return { value: count, done: false };
      },
      return() {
        log.push("return");
        return {};
      },
    };
    const loop = (async () => {
      try {
        for await (const x of iterator) {
          log.push("body:" + x);
          throw "e";
        }
      } catch (e) {
        log.push("caught:" + e);
      }
    })();
    await Promise.all([loop, ticker(log, 4)]);
    expect(log).toEqual([
      "next1",
      "t1",
      "body:1",
      "return",
      "t2",
      "t3",
      "caught:e",
      "t4",
    ]);
  });

  test("a sync iterator without return closes after one job", async () => {
    const log = [];
    const loop = (async () => {
      for await (const x of [1, 2]) {
        log.push("body:" + x);
        break;
      }
      log.push("after");
    })();
    await Promise.all([loop, ticker(log, 4)]);
    expect(log).toEqual(["t1", "body:1", "t2", "after", "t3", "t4"]);
  });

  test("return is called with no arguments and the iterator as this", async () => {
    const log = [];
    const iterator = {
      [Symbol.asyncIterator]() {
        return this;
      },
      next() {
        return { value: 1, done: false };
      },
      return(...args) {
        log.push("return:" + args.length + ":" + (this === iterator));
        return Promise.resolve({ done: true });
      },
    };
    const loop = (async () => {
      for await (const x of iterator) {
        break;
      }
      log.push("after");
    })();
    await Promise.all([loop, ticker(log, 3)]);
    expect(log).toEqual(["return:0:true", "t1", "after", "t2", "t3"]);
  });

  test("a result that is not an object throws TypeError after the await", async () => {
    const log = [];
    const iterator = {
      [Symbol.asyncIterator]() {
        return this;
      },
      next() {
        return { value: 1, done: false };
      },
      return() {
        log.push("return");
        return 5;
      },
    };
    const loop = (async () => {
      try {
        for await (const x of iterator) {
          log.push("body");
          break;
        }
        log.push("after");
      } catch (e) {
        log.push("caught:" + (e instanceof TypeError));
      }
    })();
    await Promise.all([loop, ticker(log, 3)]);
    expect(log).toEqual(["body", "return", "t1", "caught:true", "t2", "t3"]);
  });

  test("an error from return replaces a break", async () => {
    const log = [];
    const throwing = {
      [Symbol.asyncIterator]() {
        return this;
      },
      next() {
        return { value: 1, done: false };
      },
      return() {
        throw "return error";
      },
    };
    const rejecting = {
      [Symbol.asyncIterator]() {
        return this;
      },
      next() {
        return { value: 1, done: false };
      },
      return() {
        return Promise.reject("rejected");
      },
    };
    const first = (async () => {
      try {
        for await (const x of throwing) {
          break;
        }
      } catch (e) {
        log.push("caught:" + e);
      }
    })();
    const second = (async () => {
      try {
        for await (const x of rejecting) {
          break;
        }
      } catch (e) {
        log.push("caught:" + e);
      }
    })();
    await Promise.all([first, second, ticker(log, 2)]);
    expect(log).toEqual(["caught:return error", "t1", "caught:rejected", "t2"]);
  });

  test("a throw from the body wins over any error from the close", async () => {
    const log = [];
    const make = (descriptor) =>
      Object.defineProperty(
        {
          [Symbol.asyncIterator]() {
            return this;
          },
          next() {
            return { value: 1, done: false };
          },
        },
        "return",
        descriptor
      );
    const iterators = [
      make({
        value() {
          throw "thrown";
        },
      }),
      make({
        value() {
          return Promise.reject("rejected");
        },
      }),
      make({
        value() {
          return 5;
        },
      }),
      make({ value: 5 }),
      make({
        get() {
          throw "getter";
        },
      }),
    ];
    const loops = iterators.map((iterator, index) =>
      (async () => {
        try {
          for await (const x of iterator) {
            throw "body" + index;
          }
        } catch (e) {
          log.push("caught:" + e);
        }
      })()
    );
    await Promise.all([...loops, ticker(log, 2)]);
    expect(log).toEqual([
      "caught:body0",
      "caught:body3",
      "caught:body4",
      "t1",
      "caught:body1",
      "caught:body2",
      "t2",
    ]);
  });

  test("break from an inner loop closes only the inner iterator", async () => {
    const log = [];
    const helpers = {
      async *values(name) {
        try {
          yield 1;
          yield 2;
        } finally {
          log.push("finally:" + name);
        }
      },
    };
    const loop = (async () => {
      for await (const a of helpers.values("outer")) {
        for await (const b of helpers.values("inner")) {
          break;
        }
        log.push("inner done");
        break;
      }
      log.push("after");
    })();
    await Promise.all([loop, ticker(log, 10)]);
    expect(log).toEqual([
      "t1",
      "t2",
      "t3",
      "t4",
      "finally:inner",
      "t5",
      "inner done",
      "t6",
      "finally:outer",
      "t7",
      "after",
      "t8",
      "t9",
      "t10",
    ]);
  });

  test("an async generator's return() closes the loop it is suspended in", async () => {
    const log = [];
    const helpers = {
      async *inner() {
        try {
          yield 1;
        } finally {
          log.push("inner finally");
        }
      },
      async *outer() {
        for await (const x of helpers.inner()) {
          log.push("outer body");
          yield x;
        }
      },
    };
    const run = (async () => {
      const iterator = helpers.outer();
      await iterator.next();
      log.push("next");
      const result = await iterator.return("x");
      log.push("return:" + result.value + ":" + result.done);
    })();
    await Promise.all([run, ticker(log, 9)]);
    expect(log).toEqual([
      "t1",
      "outer body",
      "t2",
      "t3",
      "next",
      "t4",
      "t5",
      "inner finally",
      "t6",
      "t7",
      "return:x:true",
      "t8",
      "t9",
    ]);
  });
});
