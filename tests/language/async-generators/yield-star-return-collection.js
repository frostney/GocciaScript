/*---
description: >
  An async generator returning through yield* keeps the awaited return value
  alive across a collection that runs while it reads the delegate's return
  method.
features: [async-generators, getters, Goccia.gc]
---*/

const hasGoccia = typeof Goccia !== "undefined";

describe.runIf(hasGoccia)("a collection while yield* reads the delegate's return", () => {
  test("return() resolves with the awaited value", async () => {
    const delegate = {
      [Symbol.asyncIterator]() {
        return this;
      },
      next() {
        return { value: 1, done: false };
      },
      get return() {
        Goccia.gc();
        return undefined;
      },
    };
    const helpers = {
      async *gen() {
        yield* delegate;
      },
    };
    const iter = helpers.gen();
    await iter.next();
    const result = await iter.return({
      then(resolve) {
        resolve({ tag: "alive" });
      },
    });
    expect(result.value.tag).toBe("alive");
    expect(result.done).toBe(true);
  });
});
