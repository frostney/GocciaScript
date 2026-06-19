test("detached typed arrays do not enumerate inherited built-in names", () => {
  const sample = new Uint8Array(3);
  const keys = [];

  sample.buffer.transfer();
  for (const key in sample) {
    keys.push(key);
  }

  expect(keys).toEqual([]);
});

test("typed arrays enumerate numeric element keys", () => {
  const sample = new Uint8Array([4, 5, 6]);
  let keys = "";

  for (const key in sample) {
    keys += key;
  }

  expect(keys).toBe("012");
});
