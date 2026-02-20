test("sparse array with one hole", () => {
  const arr = [1, , 3];
  expect(arr.length).toBe(3);
  expect(arr[0]).toBe(1);
  expect(arr[1]).toBeUndefined();
  expect(arr[2]).toBe(3);
});

test("sparse array with multiple holes", () => {
  const arr = [1, , 3, , 5];
  expect(arr.length).toBe(5);
  expect(arr[0]).toBe(1);
  expect(arr[1]).toBeUndefined();
  expect(arr[2]).toBe(3);
  expect(arr[3]).toBeUndefined();
  expect(arr[4]).toBe(5);
});
