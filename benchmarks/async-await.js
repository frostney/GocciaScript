/*---
description: async/await benchmarks
---*/

suite("async/await", () => {
  bench("single await", {
    run: async () => {
      const val = await Promise.resolve(42);
    },
  });

  bench("multiple awaits", {
    run: async () => {
      const a = await Promise.resolve(1);
      const b = await Promise.resolve(2);
      const c = await Promise.resolve(3);
    },
  });

  bench("await non-Promise value", {
    run: async () => {
      const val = await 42;
    },
  });

  bench("await with try/catch", {
    run: async () => {
      try {
        const val = await Promise.resolve(42);
      } catch (e) {
        // unreachable
      }
    },
  });

  bench("await Promise.all", {
    run: async () => {
      const result = await Promise.all([
        Promise.resolve(1),
        Promise.resolve(2),
        Promise.resolve(3)
      ]);
    },
  });

  bench("nested async function call", {
    run: async () => {
      const inner = async () => await Promise.resolve(42);
      const val = await inner();
    },
  });
});
