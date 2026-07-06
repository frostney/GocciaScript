const genericPlusScalarsModularSum = (count, modulus, offset) => {
  const fullCycles = Math.floor(count / modulus);
  const remainder = count % modulus;
  let total = fullCycles * ((modulus - 1) * modulus / 2);
  for (let i = 0; i < remainder; i = i + 1) {
    total = total + ((i + offset) % modulus);
  }
  return total;
};

const expectedGenericPlusScalarsChecksum = (innerIterations) =>
  genericPlusScalarsModularSum(innerIterations, 97, 0) +
  genericPlusScalarsModularSum(innerIterations, 89, 17) +
  innerIterations * 0.5;

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
  verify: (checksum, innerIterations) => {
    const expected = expectedGenericPlusScalarsChecksum(innerIterations);
    let tolerance = Math.abs(expected) * 1e-12;
    if (tolerance < 1e-9) tolerance = 1e-9;
    return Math.abs(checksum - expected) <= tolerance;
  },
});
