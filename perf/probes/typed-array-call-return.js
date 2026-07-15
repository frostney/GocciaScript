__gocciaRegisterProbe({
  name: "typed-array-call-return",
  run: (innerIterations) => {
    const values = new Int32Array(64);
    for (let i = 0; i < values.length; i = i + 1) {
      values[i] = i;
    }
    const read = (array, index) => array[index & 63];
    const bump = (value) => value + 1;
    let total = 0;
    for (let i = 0; i < innerIterations; i = i + 1) {
      total = total + bump(read(values, i));
    }
    return total | 0;
  },
  verify: (checksum) => checksum === (checksum | 0),
});
