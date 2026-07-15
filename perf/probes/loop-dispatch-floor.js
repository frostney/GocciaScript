__gocciaRegisterProbe({
  name: "loop-dispatch-floor",
  run: (innerIterations) => {
    let x = 0;
    let y = 1;
    for (let i = 0; i < innerIterations; i = i + 1) {
      x = x + y;
      if ((x & 7) === 0) {
        y = y + 1;
      } else {
        y = y ^ 3;
      }
    }
    return (x ^ y) | 0;
  },
  verify: (checksum) => checksum === (checksum | 0),
});
