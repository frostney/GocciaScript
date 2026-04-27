/*---
description: Async generator function declarations and expressions
features: [compat-function, async-generators]
---*/

test("async function* declaration yields promised values", async () => {
  async function* numbers() {
    yield await Promise.resolve(1);
    return 2;
  }

  const iter = numbers();
  expect(await iter.next()).toEqual({ value: 1, done: false });
  expect(await iter.next()).toEqual({ value: 2, done: true });
});

test("async function* expression yields values", async () => {
  const numbers = async function* () {
    yield 3;
  };

  expect(await numbers().next()).toEqual({ value: 3, done: false });
});

