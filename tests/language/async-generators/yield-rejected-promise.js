/*---
description: Async generator yields a rejected promise; calling next() from the rejection handler must not crash or repeat prior microtasks
features: [async-generators]
---*/

test("calling next() from a rejection handler does not recurse infinitely", async () => {
  // Regression: previously, the bytecode VM's microtask drain re-ran every
  // already-processed task during a recursive drain. An async generator
  // yielding a rejected promise rejects iter.next()'s outer promise inside
  // an AwaitValue, and any handler that called iter.next() again would loop
  // until the call stack overflowed (observed as SIGSEGV). This test only
  // requires the chain to terminate; the post-rejection iterator semantics
  // (whether the generator is "completed" or resumes after the failed yield)
  // are not asserted here because they depend on AsyncGeneratorYield handling
  // tracked separately.
  const source = {
    async *gen() {
      yield Promise.reject(0);
      yield 1;
    },
  };
  const iter = source.gen();

  let firstRejection;
  let chainSettled = false;
  await new Promise((resolve, reject) => {
    iter.next().catch((err) => {
      firstRejection = err;
      iter.next().then(
        () => { chainSettled = true; resolve(); },
        () => { chainSettled = true; resolve(); },
      );
    });
  });

  expect(firstRejection).toBe(0);
  expect(chainSettled).toBe(true);
});

test("yield* of an iterable whose Symbol.iterator returns null rejects the consumer", async () => {
  const obj = { [Symbol.iterator]() { return null; } };
  const source = {
    async *gen() {
      yield* obj;
    },
  };
  const iter = source.gen();

  let caught;
  try {
    await iter.next();
  } catch (e) {
    caught = e;
  }
  expect(caught).not.toBe(undefined);
  expect(caught instanceof TypeError).toBe(true);
});

test("yield Promise.reject preserves the rejection value at the consumer", async () => {
  const sentinel = new Error("boom");
  const source = {
    async *gen() {
      yield Promise.reject(sentinel);
    },
  };
  const iter = source.gen();

  let caught;
  try {
    await iter.next();
  } catch (e) {
    caught = e;
  }
  expect(caught).toBe(sentinel);
});

test("a generator that throws synchronously can be drained by repeated next()", async () => {
  // Same code path as yield-rejected-promise — the failure surfaces on the
  // first iter.next(), and subsequent calls must observe the iterator as
  // completed rather than crashing the runtime.
  const source = {
    async *gen() {
      throw "fail";
    },
  };
  const iter = source.gen();

  let caught;
  try { await iter.next(); } catch (e) { caught = e; }
  expect(caught).toBe("fail");

  const second = await iter.next();
  expect(second).toEqual({ value: undefined, done: true });
});
