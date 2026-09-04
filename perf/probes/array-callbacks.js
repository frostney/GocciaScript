__gocciaRegisterProbe({
  name: "array-callbacks",
  run: (innerIterations) => {
    const values = Array.from({ length: 128 }, (_, index) => index);
    let checksum = 0;
    for (let i = 0; i < innerIterations; i++) {
      checksum += values.map(value => value + 1).reduce((sum, value) => sum + value, 0);
    }
    return checksum;
  },
  verify: (checksum, innerIterations) => checksum === 8256 * innerIterations,
});
