/*---
description: Async await and async generator scheduling regression tests
features: [async-await, async-generators]
---*/

test("await on thenables resumes after already queued promise reactions", async () => {
  const actual = [];
  let thenCallCount = 0;
  const thenable = {
    then(resolve) {
      thenCallCount++;
      resolve(thenCallCount);
    },
  };

  async function trigger() {
    actual.push("await " + await thenable);
    actual.push("await " + await thenable);
  }

  const triggerDone = trigger();
  const promiseDone = new Promise((resolve) => {
    actual.push("promise 1");
    resolve();
  })
    .then(() => actual.push("promise 2"))
    .then(() => actual.push("promise 3"))
    .then(() => actual.push("promise 4"));

  await Promise.all([triggerDone, promiseDone]);

  expect(actual).toEqual([
    "promise 1",
    "promise 2",
    "await 1",
    "promise 3",
    "promise 4",
    "await 2",
  ]);
  expect(thenCallCount).toBe(2);
});

test("async generator yield star over manual async iterator preserves promise values", async () => {
  const innerPromise = Promise.resolve("unwrapped value");
  const asyncIterator = {
    [Symbol.asyncIterator]() {
      return this;
    },
    next() {
      return {
        done: false,
        value: innerPromise,
      };
    },
  };

  async function* values() {
    yield* asyncIterator;
  }

  const result = await values().next();
  expect(result.value).toBe(innerPromise);
  expect(result.done).toBe(false);
});

test("async generator bare return is not delayed like explicit undefined return", async () => {
  const actual = [];

  async function* g1() {}
  async function* g2() {
    return;
  }
  async function* g3() {
    return undefined;
  }
  async function* g4() {
    return void 0;
  }

  const ticks = Promise.resolve()
    .then(() => actual.push("tick 1"))
    .then(() => actual.push("tick 2"));
  const p1 = g1().next().then(() => actual.push("g1 ret"));
  const p2 = g2().next().then(() => actual.push("g2 ret"));
  const p3 = g3().next().then(() => actual.push("g3 ret"));
  const p4 = g4().next().then(() => actual.push("g4 ret"));

  await Promise.all([ticks, p1, p2, p3, p4]);

  expect(actual).toEqual([
    "tick 1",
    "g1 ret",
    "g2 ret",
    "tick 2",
    "g3 ret",
    "g4 ret",
  ]);
});

test("async generator return resumption awaits thenable before later ticks", async () => {
  const actual = [];

  async function* values() {
    actual.push("start");
    yield 123;
    actual.push("stop");
  }

  const ticks = Promise.resolve()
    .then(() => actual.push("tick 1"))
    .then(() => actual.push("tick 2"));
  const iterator = values();
  const first = iterator.next();
  const returned = iterator.return({
    get then() {
      actual.push("get then");
    },
  });

  await Promise.all([ticks, first, returned]);

  expect(actual).toEqual(["start", "tick 1", "get then", "tick 2"]);
});
