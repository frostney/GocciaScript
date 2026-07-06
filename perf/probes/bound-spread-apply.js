__gocciaRegisterProbe({
  name: "bound-spread-apply",
  run: (innerIterations) => {
    const add3 = (a, b, c) => a + b + c;
    const bound = add3.bind(null, 1);
    const args = [2, 3];
    let total = 0;
    for (let i = 0; i < innerIterations; i = i + 1) {
      total = total + bound(...args);
      total = total + add3.call(null, i & 7, 2, 3);
      total = total + add3.apply(null, args);
    }
    return total | 0;
  },
  verify: (checksum) => checksum === (checksum | 0),
});
