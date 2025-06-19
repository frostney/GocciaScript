test("nested array with single value", () => {
  const arr = [[1]];
  expect(arr[0][0]).toEqual(1);
});

test("nested array with multiple values", () => {
  const arr = [
    [1, 2],
    [3, 4],
  ];
  expect(arr[0][0]).toEqual(1);
  expect(arr[0][1]).toEqual(2);
  expect(arr[1][0]).toEqual(3);
  expect(arr[1][1]).toEqual(4);
});
