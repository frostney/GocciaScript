__gocciaRegisterProbe({
  name: "sort-comparator",
  run: (innerIterations) => {
    let checksum = 0;
    for (let r = 0; r < innerIterations; r = r + 1) {
      const a = [];
      let seed = 42 + r;
      for (let i = 0; i < 80; i = i + 1) {
        seed = (seed * 1103515245 + 12345) % 2147483648;
        a.push(seed);
      }
      a.sort((x, y) => x - y);
      checksum = (checksum + a[0] + a[a.length - 1]) | 0;
    }
    return checksum;
  },
  verify: (checksum) => checksum === (checksum | 0),
});
