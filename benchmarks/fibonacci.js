/*---
description: Fibonacci computation benchmarks
---*/

suite("fibonacci", () => {
  bench("recursive fib(15)", () => {
    const fib = (n) => n <= 1 ? n : fib(n - 1) + fib(n - 2);
    fib(15);
  });

  bench("recursive fib(20)", () => {
    const fib = (n) => n <= 1 ? n : fib(n - 1) + fib(n - 2);
    fib(20);
  });

  bench("iterative fib(20) via reduce", () => {
    const fib = (n) => Array.from({ length: n }).reduce(
      (acc) => [acc[1], acc[0] + acc[1]],
      [0, 1]
    )[0];
    fib(20);
  });
});
