/*---
description: Async delegation roots pending results and values across collections in iterator getters
features: [async-generators, Goccia.gc]
---*/

describe.runIf(typeof Goccia !== "undefined")("async delegation result collection", () => {
  for (const method of ["next", "throw", "return"]) {
    test(method + " keeps a pending result alive through done and value getters", async () => {
      let settle;
      const source = {
        [Symbol.asyncIterator]() { return this; },
        next() { return { value: 1, done: false }; },
      };
      const it = ({ async *g() { yield* source; } }).g();
      if (method !== "next") {
        await it.next();
      }
      source[method] = () => new Promise((r) => { settle = r; });
      const target = method === "next" ? ({ async *g() { yield* source; } }).g() : it;
      const pending = target[method]({ tag: "argument" });
      await Promise.resolve();
      Goccia.gc();
      settle({
        get done() { Goccia.gc(); return false; },
        get value() { Goccia.gc(); return { tag: "result" }; },
      });
      Goccia.gc();
      const result = await pending;
      expect(result.done).toBe(false);
      expect(result.value.tag).toBe("result");
    });
  }
});
