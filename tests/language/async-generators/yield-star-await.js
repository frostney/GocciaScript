/*---
description: Async yield delegation suspends at each Await without draining unrelated jobs
features: [async-generators]
---*/

const delegate = (source) => ({ async *g() { return yield* source; } }).g();
const tick = (log, count) => {
  let p = Promise.resolve();
  for (const n of Array.from({ length: count }, (_, i) => i + 1)) {
    p = p.then(() => { log.push("t" + n); });
  }
  return p;
};

describe("async yield star awaits", () => {
  test("next returns before unrelated queued jobs run", async () => {
    const log = [];
    Promise.resolve().then(() => { log.push("job"); });
    const source = { async *g() { log.push("inner"); yield "a"; } };
    const pending = delegate(source.g()).next();
    log.push("sync");
    expect(log).toEqual(["inner", "sync"]);
    expect(await pending).toEqual({ value: "a", done: false });
    expect(log).toEqual(["inner", "sync", "job"]);
  });

  for (const kind of ["async", "sync", "manual"]) {
    test(kind + " delegate matches Node's chained-next ticks", async () => {
      const log = [];
      const source = kind === "async"
        ? ({ async *g() { yield "a"; yield "b"; } }).g()
        : kind === "sync" ? ["a", "b"] : {
          count: 0,
          [Symbol.asyncIterator]() { return this; },
          next() {
            this.count += 1;
            log.push("src.next" + this.count);
            return { value: "v" + this.count, done: this.count === 3 };
          },
        };
      const it = ({ async *g() { yield* source; } }).g();
      const steps = it.next().then((r) => {
        log.push("n1:" + r.value);
        return it.next();
      }).then((r) => {
        log.push("n2:" + r.value);
        return it.next();
      }).then((r) => { log.push("n3:" + r.done); });
      await Promise.all([steps, tick(log, 12)]);
      const expected = kind === "async"
        ? "t1 t2 n1:a t3 t4 t5 t6 n2:b t7 t8 t9 n3:true t10 t11 t12"
        : kind === "sync"
          ? "t1 t2 n1:a t3 t4 t5 t6 n2:b t7 t8 t9 t10 n3:true t11 t12"
          : "src.next1 t1 n1:v1 src.next2 t2 t3 t4 n2:v2 src.next3 t5 t6 t7 n3:true t8 t9 t10 t11 t12";
      expect(log.join(" ")).toBe(expected);
    });
  }

  for (const method of ["next", "throw", "return"]) {
    for (const done of [false, true]) {
      test(method + " result can settle after the caller returns, done=" + done, async () => {
        let settle;
        const source = {
          [Symbol.asyncIterator]() { return this; },
          next() { return { value: "start", done: false }; },
        };
        const it = delegate(source);
        if (method !== "next") {
          await it.next();
        }
        source[method] = () => new Promise((resolve) => { settle = resolve; });
        // next is cached when delegation starts.
        const target = method === "next" ? delegate(source) : it;
        const result = target[method]("arg");
        await Promise.resolve(); // return awaits its argument before forwarding.
        expect(typeof settle).toBe("function");
        settle({ value: "late", done });
        expect(await result).toEqual({ value: "late", done });
      });
    }
  }

  test("a rejected next result throws at yield star without calling delegate.throw", async () => {
    let reject;
    let forwarded = false;
    const error = {};
    const source = {
      [Symbol.asyncIterator]() { return this; },
      next() { return new Promise((_, r) => { reject = r; }); },
      throw() { forwarded = true; return { done: true }; },
    };
    const it = delegate(source);
    const result = it.next();
    reject(error);
    await expect(result).rejects.toBe(error);
    expect(forwarded).toBe(false);
    expect(await it.next()).toEqual({ value: undefined, done: true });
  });

  test("missing throw awaits a late close result with no arguments", async () => {
    let settle;
    let argumentCount = -1;
    const source = {
      [Symbol.asyncIterator]() { return this; },
      next() { return { value: 1, done: false }; },
      return(...args) {
        argumentCount = args.length;
        return new Promise((r) => { settle = r; });
      },
    };
    const it = delegate(source);
    await it.next();
    let finished = false;
    const result = it.throw("reason");
    result.then(() => { finished = true; }, () => { finished = true; });
    await Promise.resolve();
    expect(finished).toBe(false);
    expect(argumentCount).toBe(0);
    settle({ done: true });
    await expect(result).rejects.toThrow(TypeError);
  });

  test("queued requests stay pending until each delegate result settles", async () => {
    let settle;
    let calls = 0;
    const source = {
      [Symbol.asyncIterator]() { return this; },
      next() { calls += 1; return new Promise((r) => { settle = r; }); },
    };
    const it = delegate(source);
    const first = it.next();
    const second = it.next();
    expect(calls).toBe(1);
    settle({ value: 1, done: false });
    expect(await first).toEqual({ value: 1, done: false });
    expect(calls).toBe(2);
    settle({ value: 2, done: true });
    expect(await second).toEqual({ value: 2, done: true });
  });
});

for (const method of ["throw", "return"]) {
  test("a late " + method + " rejection reaches finally with its original value", async () => {
    let reject;
    const log = [];
    const reason = {};
    const source = {
      [Symbol.asyncIterator]() { return this; },
      next() { return { value: 1, done: false }; },
      [method]() { return new Promise((_, r) => { reject = r; }); },
    };
    const it = ({ async *g() { try { yield* source; } finally { log.push("finally"); } } }).g();
    await it.next();
    const pending = it[method]("arg");
    await Promise.resolve();
    expect(log).toEqual([]);
    reject(reason);
    await expect(pending).rejects.toBe(reason);
    expect(log).toEqual(["finally"]);
  });
}

for (const closeResult of ["reject", "primitive"]) {
  test("missing throw waits for " + closeResult + " close before selecting its error", async () => {
    let settle;
    const reason = {};
    const source = {
      [Symbol.asyncIterator]() { return this; },
      next() { return { value: 1, done: false }; },
      return() {
        return new Promise((resolve, reject) => {
          settle = closeResult === "reject" ? reject : resolve;
        });
      },
    };
    const it = delegate(source);
    await it.next();
    const pending = it.throw("original");
    settle(closeResult === "reject" ? reason : 1);
    if (closeResult === "reject") {
      await expect(pending).rejects.toBe(reason);
    } else {
      await expect(pending).rejects.toThrow(TypeError);
    }
  });
}

test("a non-callable throw does not close the async delegate", async () => {
  let closed = false;
  const source = {
    [Symbol.asyncIterator]() { return this; },
    next() { return { value: 1, done: false }; },
    throw: 42,
    return() { closed = true; return { done: true }; },
  };
  const it = delegate(source);
  await it.next();
  await expect(it.throw("error")).rejects.toThrow(TypeError);
  expect(closed).toBe(false);
});

test("return waits for its argument before reading the delegate method", async () => {
  let settle;
  const log = [];
  const source = {
    [Symbol.asyncIterator]() { return this; },
    next() { return { value: 1, done: false }; },
    get return() {
      log.push("get return");
      return (value) => ({ value, done: true });
    },
  };
  const it = delegate(source);
  await it.next();
  const pending = it.return(new Promise((r) => { settle = r; }));
  expect(log).toEqual([]);
  const value = {};
  settle(value);
  expect(await pending).toEqual({ value, done: true });
  expect(log).toEqual(["get return"]);
});

test("async delegate next is cached, receives undefined first, and keeps its receiver", async () => {
  const values = [];
  let gets = 0;
  const source = {
    [Symbol.asyncIterator]() { return this; },
    get next() {
      gets += 1;
      return ({ step(value) {
        expect(this).toBe(source);
        values.push(value);
        return { value, done: values.length === 2 };
      } }).step;
    },
  };
  const it = delegate(source);
  expect(await it.next("ignored")).toEqual({ value: undefined, done: false });
  expect(await it.next("sent")).toEqual({ value: "sent", done: true });
  expect(values).toEqual([undefined, "sent"]);
  expect(gets).toBe(1);
});

test("for await observes delegated values on the same ticks as Node", async () => {
  const log = [];
  const source = ({ async *g() { yield "a"; yield "b"; } }).g();
  const it = ({ async *g() { yield* source; } }).g();
  const consume = async () => {
    for await (const value of it) {
      log.push("v:" + value);
    }
    log.push("done");
  };
  await Promise.all([consume(), tick(log, 10)]);
  expect(log.join(" ")).toBe("t1 t2 v:a t3 t4 t5 v:b t6 t7 done t8 t9 t10");
});

test("a catch starts fresh delegation with a normal undefined resumption", async () => {
  const source = {
    [Symbol.asyncIterator]() { return this; },
    next() { return Promise.reject("reason"); },
  };
  const it = ({ async *g() {
    try { yield* source; } catch (e) { yield* [2]; }
  } }).g();
  expect(await it.next()).toEqual({ value: 2, done: false });
  expect(await it.next()).toEqual({ value: undefined, done: true });
});

test("a return completion can enter fresh delegation in finally", async () => {
  const it = ({ async *g() {
    try { yield 1; } finally { yield* [2]; }
  } }).g();
  expect(await it.next()).toEqual({ value: 1, done: false });
  expect(await it.return(9)).toEqual({ value: 2, done: false });
  expect(await it.next()).toEqual({ value: 9, done: true });
});

test("an abrupt return-argument await is forwarded to the delegate's throw", async () => {
  const error = {};
  const log = [];
  const source = {
    [Symbol.asyncIterator]() { return this; },
    next() { return { value: 1, done: false }; },
    throw(reason) {
      expect(reason).toBe(error);
      log.push("throw");
      return { value: "caught", done: true };
    },
    return() { log.push("return"); return { done: true }; },
  };
  const it = delegate(source);
  await it.next();
  const argument = Promise.resolve(1);
  Object.defineProperty(argument, "constructor", {
    get() { log.push("constructor"); throw error; },
  });
  const result = it.return(argument);
  log.push("sync");
  expect(await result).toEqual({ value: "caught", done: true });
  expect(log).toEqual(["constructor", "throw", "sync"]);
});

test("an abrupt return-argument await closes a delegate without throw", async () => {
  const error = {};
  const log = [];
  const source = {
    [Symbol.asyncIterator]() { return this; },
    next() { return { value: 1, done: false }; },
    return(...args) { log.push(args); return { done: true }; },
  };
  const it = delegate(source);
  await it.next();
  const argument = Promise.resolve(1);
  Object.defineProperty(argument, "constructor", { get() { throw error; } });
  await expect(it.return(argument)).rejects.toThrow(TypeError);
  expect(log).toEqual([[]]);
});
