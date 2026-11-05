/*---
description: Async yield delegation errors identify the bad result or method
features: [async-generators]
---*/

const delegate = (methods) => ({
  async *g() {
    yield* {
      [Symbol.asyncIterator]() { return this; },
      next() { return { value: 1, done: false }; },
      ...methods,
    };
  },
}).g();

const expectTypeError = async (pending, message) => {
  const error = await pending.then(() => undefined, (reason) => reason);
  expect(error).toBeInstanceOf(TypeError);
  expect(error.message).toBe(message);
};

describe("async yield star error messages", () => {
  for (const method of ["next", "return", "throw"]) {
    for (const value of [3, null, undefined, "bad", true, Symbol("bad"), 5n]) {
      test(method + " identifies non-object result " + String(value), async () => {
        const it = delegate({ [method]() { return Promise.resolve(value); } });
        if (method !== "next") await it.next();
        await expectTypeError(it[method](0),
          "Iterator result " + String(value) + " is not an object");
      });
    }
  }

  for (const method of ["return", "throw"]) {
    test("identifies non-callable " + method, async () => {
      const it = delegate({ [method]: 5 });
      await it.next();
      await expectTypeError(it[method](0), "Iterator " + method + " is not callable");
    });
  }
});
