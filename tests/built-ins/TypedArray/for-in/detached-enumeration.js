test("detached typed arrays do not enumerate inherited built-in names", () => {
  const sample = new Uint8Array(3);
  const keys = [];

  sample.buffer.transfer();
  for (const key in sample) {
    keys.push(key);
  }

  expect(keys).toEqual([]);
});
