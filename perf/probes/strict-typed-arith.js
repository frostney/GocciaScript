__gocciaRegisterProbe({
  name: "strict-typed-arith",
  run: (innerIterations) => {
    const add = (a: number, b: number): number => a + b;
    let total: number = 0;
    for (let i: number = 0; i < innerIterations; i = i + 1) {
      total = add(total, (i % 31) + 0.25);
    }
    return total;
  },
  verify: (checksum) => checksum === checksum,
});
