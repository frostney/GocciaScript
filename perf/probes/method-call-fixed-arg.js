__gocciaRegisterProbe({
  name: "method-call-fixed-arg",
  run: (innerIterations) => {
    const accumulator = {
      scale: 3,
      add(a, b) {
        return (a + b) * this.scale;
      },
    };
    let total = 0;
    for (let i = 0; i < innerIterations; i = i + 1) {
      total = total + accumulator.add(i & 31, 5);
    }
    return total | 0;
  },
  verify: (checksum) => checksum === (checksum | 0),
});
