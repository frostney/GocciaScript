test("in operator with strings", () => {
  const str = "hello";

  // Valid character indices
  expect(0 in str).toBe(true);
  expect(1 in str).toBe(true);
  expect(4 in str).toBe(true);
  expect("0" in str).toBe(true);
  expect("4" in str).toBe(true);

  // Invalid indices
  expect(5 in str).toBe(false);
  expect(-1 in str).toBe(false);
  expect("5" in str).toBe(false);
  expect("-1" in str).toBe(false);

  // String properties
  expect("length" in str).toBe(true);
  expect("nonexistent" in str).toBe(false);
});

test("in operator with computed properties access in strings", () => {
  const str = "hello";
  const computed = 0;
  expect(computed in str).toBe(true);

  const computed2 = 1;
  expect(computed2 in str).toBe(true);
});

test("in operator in empty string", () => {
  const str = "";
  expect(0 in str).toBe(false);
  expect(1 in str).toBe(false);
  expect("0" in str).toBe(false);
  expect("1" in str).toBe(false);
});
