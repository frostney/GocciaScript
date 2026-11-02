/*---
description: for await with a using or await using head iterates its source asynchronously and disposes each value at the end of its iteration (ES2027 §14.7.5, §7.5.4 DisposeResources)
features: [explicit-resource-management, async-iteration]
---*/

const asyncSource = (log, count = 2) => ({
  [Symbol.asyncIterator]() {
    let i = 0;
    return {
      next() {
        i++;
        const n = i;
        return Promise.resolve(n <= count
          ? {
            value: {
              n,
              async [Symbol.asyncDispose]() {
                await null;
                log.push("async dispose " + n);
              },
              [Symbol.dispose]() {
                log.push("sync dispose " + n);
              },
            },
            done: false,
          }
          : { value: undefined, done: true });
      },
      return() {
        log.push("return");
        return Promise.resolve({ value: undefined, done: true });
      },
    };
  },
});

describe("for await (await using x of source)", () => {
  test("iterates an async iterable and awaits each disposal", async () => {
    const log = [];
    for await (await using x of asyncSource(log)) {
      log.push("body " + x.n);
    }
    expect(log).toEqual([
      "body 1", "async dispose 1", "body 2", "async dispose 2",
    ]);
  });

  test("awaits the values of a sync iterable", async () => {
    const log = [];
    const values = [1, 2].map((n) => Promise.resolve({
      n,
      [Symbol.asyncDispose]() {
        log.push("dispose " + n);
      },
    }));
    for await (await using x of values) {
      log.push("body " + x.n);
    }
    expect(log).toEqual(["body 1", "dispose 1", "body 2", "dispose 2"]);
  });

  test("a break disposes the value before the iterator closes", async () => {
    const log = [];
    for await (await using x of asyncSource(log)) {
      log.push("body " + x.n);
      break;
    }
    expect(log).toEqual(["body 1", "async dispose 1", "return"]);
  });

  test("a throw disposes the value, closes the iterator and propagates", async () => {
    const log = [];
    let caught;
    try {
      for await (await using x of asyncSource(log)) {
        throw new Error("body " + x.n);
      }
    } catch (e) {
      caught = e;
    }
    expect(caught.message).toBe("body 1");
    expect(log).toEqual(["async dispose 1", "return"]);
  });

  test("null and undefined values are not disposed", async () => {
    const log = [];
    const values = {
      async *[Symbol.asyncIterator]() {
        yield null;
        yield undefined;
      },
    };
    for await (await using x of values) {
      log.push("body " + x);
    }
    expect(log).toEqual(["body null", "body undefined"]);
  });

  test("a value without a dispose method throws a TypeError", async () => {
    const log = [];
    const values = {
      async *[Symbol.asyncIterator]() {
        try {
          yield {};
        } finally {
          log.push("closed");
        }
      },
    };
    let caught;
    try {
      for await (await using x of values) {
        log.push("body");
      }
    } catch (e) {
      caught = e;
    }
    expect(caught instanceof TypeError).toBe(true);
    expect(log).toEqual(["closed"]);
  });
});

describe("for await (using x of source)", () => {
  test("iterates an async iterable and disposes each value synchronously", async () => {
    const log = [];
    for await (using x of asyncSource(log)) {
      log.push("body " + x.n);
    }
    expect(log).toEqual([
      "body 1", "sync dispose 1", "body 2", "sync dispose 2",
    ]);
  });

  test("a break disposes the value before the iterator closes", async () => {
    const log = [];
    for await (using x of asyncSource(log)) {
      log.push("body " + x.n);
      break;
    }
    expect(log).toEqual(["body 1", "sync dispose 1", "return"]);
  });
});

describe("async generator return() in a for await with a using head", () => {
  test("disposes the iteration's value before the iterator closes", async () => {
    const log = [];
    const holder = {
      async *run() {
        for await (await using x of asyncSource(log)) {
          yield x.n;
        }
      },
    };
    const it = holder.run();
    expect(await it.next()).toEqual({ value: 1, done: false });
    expect(await it.return(9)).toEqual({ value: 9, done: true });
    expect(log).toEqual(["async dispose 1", "return"]);
  });
});
