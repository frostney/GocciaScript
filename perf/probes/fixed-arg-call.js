__gocciaRegisterProbe({
  name: "fixed-arg-call",
  run: (innerIterations) => {
    const add3 = (a, b, c) => a + b + c;
    let total = 0;
    for (let i = 0; i < innerIterations; i = i + 1) {
      total = total + add3(i & 15, 7, 3);
    }
    return total | 0;
  },
  verify: (checksum) => checksum === (checksum | 0),
});
