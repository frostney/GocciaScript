/*---
description: Async generator scheduling regression tests
features: [async-generators]
---*/

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
  const helpers = {
    async *values() {
      yield* asyncIterator;
    },
  };

  const result = await helpers.values().next();
  expect(result.value).toBe(innerPromise);
  expect(result.done).toBe(false);
});

test("async generator bare return is not delayed like explicit undefined return", async () => {
  const actual = [];
  const helpers = {
    async *g1() {},
    async *g2() {
      return;
    },
    async *g3() {
      return undefined;
    },
    async *g4() {
      return void 0;
    },
  };

  const ticks = Promise.resolve()
    .then(() => actual.push("tick 1"))
    .then(() => actual.push("tick 2"));
  const p1 = helpers.g1().next().then(() => actual.push("g1 ret"));
  const p2 = helpers.g2().next().then(() => actual.push("g2 ret"));
  const p3 = helpers.g3().next().then(() => actual.push("g3 ret"));
  const p4 = helpers.g4().next().then(() => actual.push("g4 ret"));

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
  const helpers = {
    async *values() {
      actual.push("start");
      yield 123;
      actual.push("stop");
    },
  };

  const ticks = Promise.resolve()
    .then(() => actual.push("tick 1"))
    .then(() => actual.push("tick 2"));
  const iterator = helpers.values();
  const first = iterator.next();
  const returned = iterator.return({
    get then() {
      actual.push("get then");
    },
  });

  await Promise.all([ticks, first, returned]);

  expect(actual).toEqual(["start", "tick 1", "get then", "tick 2"]);
});
