__gocciaRegisterProbe({
  name: "generic-plus-scalars",
  run: (innerIterations) => {
    let total = 0;
    for (let i = 0; i < innerIterations; i = i + 1) {
      const a = i % 97;
      const b = (i + 17) % 89;
      total = total + a + b + 0.5;
    }
    return total;
  },
  verify: (checksum) => checksum === checksum,
});
