test("Array.flatMap should return a new array with the mapped values", () => {
  const array = [1, 2, 3, 4, 5];

  expect(array.flatMap((x) => [x * 2])).toEqual([2, 4, 6, 8, 10]);
});

test("Array.flatMap will only flatten one level", () => {
  const array = [1, 2, 3, 4, 5];

  expect(array.flatMap((x) => [[x * 2]])).toEqual([[2], [4], [6], [8], [10]]);
});

test("Array.flatMap with index and array parameters", () => {
  const array = [1, 2, 3];

  expect(
    array.flatMap((x, i, arr) => {
      expect(arr).toBe(array);

      if (i === 0) {
        expect(x).toBe(1);
        expect(i).toBe(0);
      }

      if (i === 1) {
        expect(x).toBe(2);
        expect(i).toBe(1);
      }

      if (i === 2) {
        expect(x).toBe(3);
        expect(i).toBe(2);
      }

      return [x * 2];
    })
  ).toEqual([2, 4, 6]);
});

test("Array.flatMap with index and array parameters to create a new array", () => {
  const array = [1, 2, 3];

  expect(
    array.flatMap((x, i, arr) => {
      return [x * 2, i, arr];
    })
  ).toEqual([2, 0, [1, 2, 3], 4, 1, [1, 2, 3], 6, 2, [1, 2, 3]]);
});

test("Array.flatMap on an empty array should return an empty array", () => {
  const array = [];

  expect(array.flatMap((x) => [x * 2])).toEqual([]);
});

test("Array.flatMap on a sparse array", () => {
  expect([1, 2, , 4, 5].flatMap((x) => [x, x * 2])).toEqual([
    1, 2, 2, 4, 4, 8, 5, 10,
  ]);
  expect([1, 2, 3, 4].flatMap((x) => [, x * 2])).toEqual([2, 4, 6, 8]);
});
