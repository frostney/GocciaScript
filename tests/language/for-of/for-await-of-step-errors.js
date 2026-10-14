/*---
description: for-await-of closes the iterator only when the binding or the body completes abruptly, never when the iteration step fails
features: [for-of, async-await]
---*/

// ES2026 §14.7.5.7 ForIn/OfBodyEvaluation: steps 6.a-6.f (next(), its Await,
// the Object check, done and value) use `?`, so an error there leaves the
// loop without calling return(). Only an abrupt binding (step 6.i.iv) or
// body (step 6.l.iv) runs AsyncIteratorClose. Every loop body here is
// non-empty, so the loop has to be able to close its iterator.

const makeIterator = (wrap, step, calls) => {
  const iterator = {
    next() {
      calls.next++;
      return step(calls.next);
    },
    return() {
      calls.return++;
      return wrap === "async" ? Promise.resolve({ done: true }) : { done: true };
    },
  };
  const iterable = wrap === "async"
    ? { [Symbol.asyncIterator]() { return iterator; } }
    : { [Symbol.iterator]() { return iterator; } };
  return iterable;
};

const resultWith = (wrap, result) =>
  wrap === "async" ? Promise.resolve(result) : result;

const throwingDone = (wrap) => () => resultWith(wrap, {
  get done() { throw new Error("done getter"); },
  value: 1,
});

const throwingValue = (wrap) => () => resultWith(wrap, {
  done: false,
  get value() { throw new Error("value getter"); },
});

const runLoop = async (iterable, body) => {
  try {
    for await (const item of iterable) {
      body(item);
    }
    return "completed";
  } catch (e) {
    return e;
  }
};

describe("for-await-of does not call return() when the iteration step fails", () => {
  // For a sync iterator, %AsyncFromSyncIteratorPrototype%.next (§27.1.5.2.1
  // step 6) and AsyncFromSyncIteratorContinuation (§27.1.5.4 steps 2-5)
  // reject without closing when next() throws or done/value throw. Bun agrees;
  // Node 24 (V8) calls the sync iterator's return() here even outside
  // for-await (from `yield*` in an async generator too), which the spec does
  // not do.
  for (const wrap of ["async", "sync"]) {
    describe(wrap + " iterator", () => {
      test("next() throws synchronously", async () => {
        const calls = { next: 0, return: 0 };
        const bodyValues = [];
        const error = await runLoop(makeIterator(wrap, () => {
          throw new Error("next threw");
        }, calls), (item) => bodyValues.push(item));
        expect(error.message).toBe("next threw");
        expect(calls.return).toBe(0);
        expect(bodyValues).toEqual([]);
      });

      test("done getter throws", async () => {
        const calls = { next: 0, return: 0 };
        const bodyValues = [];
        const error = await runLoop(makeIterator(wrap, throwingDone(wrap), calls),
          (item) => bodyValues.push(item));
        expect(error.message).toBe("done getter");
        expect(calls.return).toBe(0);
        expect(bodyValues).toEqual([]);
      });

      test("value getter throws", async () => {
        const calls = { next: 0, return: 0 };
        const bodyValues = [];
        const error = await runLoop(makeIterator(wrap, throwingValue(wrap), calls),
          (item) => bodyValues.push(item));
        expect(error.message).toBe("value getter");
        expect(calls.return).toBe(0);
        expect(bodyValues).toEqual([]);
      });

      test("next() result is not an object", async () => {
        const calls = { next: 0, return: 0 };
        const bodyValues = [];
        const error = await runLoop(makeIterator(wrap,
          () => resultWith(wrap, 42), calls), (item) => bodyValues.push(item));
        expect(error instanceof TypeError).toBe(true);
        expect(calls.return).toBe(0);
        expect(bodyValues).toEqual([]);
      });

      test("the step fails after earlier iterations ran the body", async () => {
        const calls = { next: 0, return: 0 };
        const bodyValues = [];
        const error = await runLoop(makeIterator(wrap, (n) => {
          if (n < 3) return resultWith(wrap, { done: false, value: n });
          throw new Error("next threw on " + n);
        }, calls), (item) => bodyValues.push(item));
        expect(error.message).toBe("next threw on 3");
        expect(bodyValues).toEqual([1, 2]);
        expect(calls.return).toBe(0);
      });

      test("the step fails under a destructuring binding", async () => {
        const calls = { next: 0, return: 0 };
        const bodyValues = [];
        let error;
        try {
          for await (const [a] of makeIterator(wrap, throwingDone(wrap), calls)) {
            bodyValues.push(a);
          }
        } catch (e) {
          error = e;
        }
        expect(error.message).toBe("done getter");
        expect(calls.return).toBe(0);
        expect(bodyValues).toEqual([]);
      });
    });
  }

  test("async iterator: next() rejects", async () => {
    const calls = { next: 0, return: 0 };
    const bodyValues = [];
    const error = await runLoop(makeIterator("async", () =>
      Promise.reject(new Error("next rejected")), calls),
      (item) => bodyValues.push(item));
    expect(error.message).toBe("next rejected");
    expect(calls.return).toBe(0);
    expect(bodyValues).toEqual([]);
  });

  test("async iterator: next() returns a promise that rejects later", async () => {
    const calls = { next: 0, return: 0 };
    const bodyValues = [];
    const error = await runLoop(makeIterator("async", () =>
      Promise.resolve().then(() => { throw new Error("late reject"); }), calls),
      (item) => bodyValues.push(item));
    expect(error.message).toBe("late reject");
    expect(calls.return).toBe(0);
    expect(bodyValues).toEqual([]);
  });
});

describe("for-await-of still calls return() when the binding or the body completes abruptly", () => {
  for (const wrap of ["async", "sync"]) {
    describe(wrap + " iterator", () => {
      const yieldsValues = (calls, values) => makeIterator(wrap, (n) =>
        resultWith(wrap, n <= values.length
          ? { done: false, value: values[n - 1] }
          : { done: true, value: undefined }), calls);

      test("break", async () => {
        const calls = { next: 0, return: 0 };
        const bodyValues = [];
        for await (const item of yieldsValues(calls, [1, 2, 3])) {
          bodyValues.push(item);
          if (item === 2) break;
        }
        expect(bodyValues).toEqual([1, 2]);
        expect(calls.return).toBe(1);
      });

      test("throw in the body", async () => {
        const calls = { next: 0, return: 0 };
        const error = await runLoop(yieldsValues(calls, [1, 2]), () => {
          throw new Error("body threw");
        });
        expect(error.message).toBe("body threw");
        expect(calls.return).toBe(1);
      });

      test("return in the body", async () => {
        const calls = { next: 0, return: 0 };
        const result = await (async () => {
          for await (const item of yieldsValues(calls, [1, 2])) {
            return "returned " + item;
          }
          return "completed";
        })();
        expect(result).toBe("returned 1");
        expect(calls.return).toBe(1);
      });

      test("abrupt destructuring of a lexical binding", async () => {
        const calls = { next: 0, return: 0 };
        const bodyValues = [];
        let error;
        try {
          for await (const [a] of yieldsValues(calls, [[1], null])) {
            bodyValues.push(a);
          }
        } catch (e) {
          error = e;
        }
        expect(error instanceof TypeError).toBe(true);
        expect(bodyValues).toEqual([1]);
        expect(calls.return).toBe(1);
      });

      test("abrupt destructuring of an object binding", async () => {
        const calls = { next: 0, return: 0 };
        const bodyValues = [];
        let error;
        const throwing = { get x() { throw new Error("x getter"); } };
        try {
          for await (const { x } of yieldsValues(calls, [throwing])) {
            bodyValues.push(x);
          }
        } catch (e) {
          error = e;
        }
        expect(error.message).toBe("x getter");
        expect(bodyValues).toEqual([]);
        expect(calls.return).toBe(1);
      });

      test("abrupt assignment to the loop target", async () => {
        const calls = { next: 0, return: 0 };
        const target = { set item(v) { throw new Error("setter " + v); } };
        let error;
        try {
          for await (target.item of yieldsValues(calls, [7])) {
            calls.body = true;
          }
        } catch (e) {
          error = e;
        }
        expect(error.message).toBe("setter 7");
        expect(calls.body).toBe(undefined);
        expect(calls.return).toBe(1);
      });
    });
  }
});

describe("for-await-of step errors reach the catch without an extra close tick", () => {
  const ticker = (log, n) => {
    let p = Promise.resolve();
    for (const i of Array.from({ length: n }, (_, j) => j + 1)) {
      p = p.then(() => { log.push("t" + i); });
    }
    return p;
  };

  test("async iterator whose next() rejects", async () => {
    const log = [];
    const iterable = {
      [Symbol.asyncIterator]() { return this; },
      next() { log.push("next"); return Promise.reject("x"); },
      return() { log.push("return"); return {}; },
    };
    const loop = (async () => {
      try {
        for await (const v of iterable) log.push("body:" + v);
      } catch (e) {
        log.push("caught:" + e);
      }
    })();
    await Promise.all([loop, ticker(log, 2)]);
    // Node 24: next caught:x t1 t2
    expect(log).toEqual(["next", "caught:x", "t1", "t2"]);
  });

  // Node 24 logs "t1 caught:x t2 t3"; --mode=interpreted waits for each step
  // by draining the microtask queue and catches before t1, so these assert
  // only that no close step delays the catch past t2.
  test("array holding a rejected promise", async () => {
    const log = [];
    const rejected = Promise.reject("x");
    const loop = (async () => {
      try {
        for await (const v of [rejected]) log.push("body:" + v);
      } catch (e) {
        log.push("caught:" + e);
      }
    })();
    await Promise.all([loop, ticker(log, 3)]);
    expect(log.indexOf("caught:x")).toBeLessThan(log.indexOf("t2"));
    expect(log.length).toBe(4);
  });

  test("sync iterator yielding a rejected promise", async () => {
    const log = [];
    let count = 0;
    const iterable = {
      [Symbol.iterator]() { return this; },
      next() {
        count++;
        log.push("next" + count);
        return count === 1
          ? { value: Promise.reject("x"), done: false }
          : { value: undefined, done: true };
      },
      return() { log.push("return"); return {}; },
    };
    const loop = (async () => {
      try {
        for await (const v of iterable) log.push("body:" + v);
      } catch (e) {
        log.push("caught:" + e);
      }
    })();
    await Promise.all([loop, ticker(log, 3)]);
    // Node 24: next1 return t1 caught:x t2 t3. The async-from-sync wrapper
    // closes the sync iterator itself (§27.1.5.4 step 13); the loop does not
    // close it a second time.
    expect(log.indexOf("caught:x")).toBeLessThan(log.indexOf("t2"));
    expect(log.filter((entry) => entry === "return").length).toBeLessThan(2);
  });
});
