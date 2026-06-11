test("assignment to a far index completes without materializing dense storage", () => {
  const arr = [];
  arr[2 ** 31 - 2] = 7;
  expect(arr.length).toBe(2 ** 31 - 1);
  expect(arr[2 ** 31 - 2]).toBe(7);
  expect(arr[0]).toBeUndefined();
  expect(arr[2 ** 30]).toBeUndefined();
});

test("far index after existing elements preserves both", () => {
  const arr = [1, 2, 3];
  arr[2 ** 30] = "far";
  expect(arr.length).toBe(2 ** 30 + 1);
  expect(arr[0]).toBe(1);
  expect(arr[2]).toBe(3);
  expect(arr[2 ** 30]).toBe("far");
  expect(arr[2 ** 29]).toBeUndefined();
});

test("power-of-two index sweep stays fast", () => {
  const arr = [];
  let k = 1;
  for (const _ of Array(31).keys()) {
    k = k * 2;
    arr[k - 2] = k;
  }
  expect(arr.length).toBe(k - 1);
  expect(arr[k - 2]).toBe(k);
  expect(arr[2]).toBe(4);
  expect(arr[0]).toBe(2);
});

test("near-gap assignment stays within dense growth", () => {
  const arr = [];
  arr[1000] = "near";
  expect(arr.length).toBe(1001);
  expect(arr[1000]).toBe("near");
  expect(arr[999]).toBeUndefined();
});

test("far index then truncating length removes the element", () => {
  const arr = [];
  arr[2 ** 30] = "far";
  arr.length = 10;
  expect(arr.length).toBe(10);
  expect(arr[2 ** 30]).toBeUndefined();
});
