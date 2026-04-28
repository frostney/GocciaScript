/*---
description: Object async generator methods
features: [async-generators]
---*/

test("object async generator method works without compat-function", async () => {
  const obj = {
    async *numbers() {
      yield await Promise.resolve(1);
      yield 2;
    },
  };

  const seen = [];
  for await (const value of obj.numbers()) {
    seen.push(value);
  }
  expect(seen).toEqual([1, 2]);
});

test("object async generator method delegates to async iterable", async () => {
  const source = {
    async *values() {
      yield 1;
      yield 2;
    },
  };
  const obj = {
    async *numbers() {
      yield* source.values();
    },
  };
  const seen = [];

  for await (const value of obj.numbers()) {
    seen.push(value);
  }

  expect(seen).toEqual([1, 2]);
});

test("object async generator method delegates to sync iterable", async () => {
  const obj = {
    async *numbers() {
      yield* [Promise.resolve(1), 2];
    },
  };
  const seen = [];

  for await (const value of obj.numbers()) {
    seen.push(value);
  }

  expect(seen).toEqual([1, 2]);
});

test("object async generator yield delegation rejects non-callable async next", async () => {
  const source = {
    [Symbol.asyncIterator]() {
      return {};
    },
  };
  const obj = {
    async *values() {
      yield* source;
    },
  };

  await expect(obj.values().next()).rejects.toThrow(TypeError);
});

test("object async generator return and throw use promises", async () => {
  const obj = {
    async *numbers() {
      yield 1;
    },
    async *fails() {
      throw 5;
    },
  };

  await expect(obj.numbers().return(9)).resolves.toEqual({ value: 9, done: true });
  await expect(obj.numbers().next()).resolves.toEqual({ value: 1, done: false });
  const iter = obj.numbers();
  await expect(iter.next()).resolves.toEqual({ value: 1, done: false });
  await expect(iter.next(9)).resolves.toEqual({ value: undefined, done: true });
  await expect(obj.fails().next()).rejects.toBe(5);
});
