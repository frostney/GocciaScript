__gocciaRegisterProbe({
  name: "strict-typed-call",
  run: (innerIterations) => {
    const mix = (a: number, b: number, c: number): number => (a + b) * c;
    let total: number = 0;
    for (let i: number = 0; i < innerIterations; i = i + 1) {
      total = total + mix(i % 13, 2, 3);
    }
    return total;
  },
  verify: (checksum) => checksum === checksum,
});
