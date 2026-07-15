__gocciaRegisterProbe({
  name: "fib-recursive",
  run: (innerIterations) => {
    const fib = (n) => n <= 1 ? n : fib(n - 1) + fib(n - 2);
    let total = 0;
    for (let i = 0; i < innerIterations; i = i + 1) {
      total = total + fib(20);
    }
    return total;
  },
  verify: (checksum, innerIterations) => checksum === 6765 * innerIterations,
});
