test("in operator with arrays", () => {
  const arr = [10, 20, 30, 40, 50];

  // Valid indices
  expect(0 in arr).toBe(true);
  expect(1 in arr).toBe(true);
  expect(4 in arr).toBe(true);
  expect("0" in arr).toBe(true);
  expect("4" in arr).toBe(true);

  // Invalid indices
  expect(5 in arr).toBe(false);
  expect(-1 in arr).toBe(false);
  expect("5" in arr).toBe(false);
  expect("-1" in arr).toBe(false);

  // Array properties and methods
  expect("length" in arr).toBe(true);
  expect("push" in arr).toBe(true);
  expect("pop" in arr).toBe(true);
  expect("map" in arr).toBe(true);
  expect("filter" in arr).toBe(true);
  expect("nonexistent" in arr).toBe(false);
});

test("in operator with sparse arrays", () => {
  const arr = [1, , 3, , 5];
  expect(0 in arr).toBe(true);
  expect(1 in arr).toBe(false);
  expect(2 in arr).toBe(true);
  expect(3 in arr).toBe(false);
  expect(4 in arr).toBe(true);
  expect(5 in arr).toBe(false);
});

test("in operator with computed properties access in arrays", () => {
  const arr = [1, , 3, , 5];
  const computed = 0;
  expect(computed in arr).toBe(true);

  const computed2 = 1;
  expect(computed2 in arr).toBe(false);

  const computed3 = 2;
  expect(computed3 in arr).toBe(true);

  const computed4 = 3;
  expect(computed4 in arr).toBe(false);

  const computed5 = 4;
  expect(computed5 in arr).toBe(true);

  const computed6 = 5;
  expect(computed6 in arr).toBe(false);

  const computed7 = 100;
  expect(computed7 in arr).toBe(false);
});

test("in operator in empty array", () => {
  const arr = [];
  expect(0 in arr).toBe(false);
  expect(1 in arr).toBe(false);
  expect("0" in arr).toBe(false);
  expect("1" in arr).toBe(false);
});
