/*---
description: Async generator benchmarks
---*/

suite("async generators", () => {
  const values = Array.from({ length: 25 }, (_, i) => i);

  const source = {
    async *values() {
      yield* values;
    },
    async *awaitedValues() {
      yield await Promise.resolve(1);
      yield await Promise.resolve(2);
    },
  };

  bench("for-await-of over async generator", {
    run: async () => {
      let sum = 0;
      for await (const value of source.values()) {
        sum = sum + value;
      }
      return sum;
    },
  });

  bench("async generator with await in body", {
    run: async () => {
      let sum = 0;
      for await (const value of source.awaitedValues()) {
        sum = sum + value;
      }
      return sum;
    },
  });
});

