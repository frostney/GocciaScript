/*---
description: Fibonacci computation benchmarks
---*/

suite("fibonacci", () => {
  bench("recursive fib(15)", {
    run: () => {
      const fib = (n) => n <= 1 ? n : fib(n - 1) + fib(n - 2);
      fib(15);
    },
  });

  bench("recursive fib(20)", {
    run: () => {
      const fib = (n) => n <= 1 ? n : fib(n - 1) + fib(n - 2);
      fib(20);
    },
  });

  bench("iterative fib(20) via reduce", {
    run: () => {
      const fib = (n) => Array.from({ length: n }).reduce(
        (acc) => [acc[1], acc[0] + acc[1]],
        [0, 1]
      )[0];
      fib(20);
    },
  });

  bench("iterator fib(20)", {
    run: () => {
      const fibIter = {
        [Symbol.iterator]() {
          let a = 0;
          let b = 1;
          let count = 0;
          return {
            next() {
              if (count >= 20) {
                return { value: undefined, done: true };
              }
              const value = a;
              const next = a + b;
              a = b;
              b = next;
              count = count + 1;
              return { value: value, done: false };
            }
          };
        }
      };
      const fibs = [...fibIter];
    },
  });

  bench("iterator fib(20) via Iterator.from + take", {
    run: () => {
      let a = 0;
      let b = 1;
      const fibs = Iterator.from({
        next() {
          const value = a;
          const next = a + b;
          a = b;
          b = next;
          return { value: value, done: false };
        }
      }).take(20).toArray();
    },
  });

  bench("iterator fib(20) last value via reduce", {
    run: () => {
      let a = 0;
      let b = 1;
      const last = Iterator.from({
        next() {
          const value = a;
          const next = a + b;
          a = b;
          b = next;
          return { value: value, done: false };
        }
      }).take(20).reduce((_, v) => v, 0);
    },
  });
});
