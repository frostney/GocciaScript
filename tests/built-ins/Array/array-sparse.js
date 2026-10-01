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

test("a hole read by index consults the prototype chain", () => {
  const arr = [1, , 3];
  Array.prototype[1] = "inherited";
  try {
    expect(arr[1]).toBe("inherited");
    const index = 1;
    expect(arr[index]).toBe("inherited");
  } finally {
    delete Array.prototype[1];
  }
  expect(arr[1]).toBeUndefined();
});

test("an index past the elements reads undefined or an inherited value", () => {
  const arr = [1, 2];
  const index = 5;
  expect(arr[index]).toBeUndefined();
  Array.prototype[5] = "far";
  try {
    expect(arr[index]).toBe("far");
  } finally {
    delete Array.prototype[5];
  }
});

test("an index defined as an accessor runs its getter on every read", () => {
  const arr = [1, 2, 3];
  let reads = 0;
  Object.defineProperty(arr, "1", {
    get() {
      reads += 1;
      return "computed";
    },
    configurable: true,
  });
  const index = 1;
  expect(arr[index]).toBe("computed");
  expect(arr[1]).toBe("computed");
  expect(reads).toBe(2);
});

test("negative and fractional number keys are ordinary properties", () => {
  const arr = [1, 2, 3];
  const negative = -1;
  const fractional = 1.5;
  expect(arr[negative]).toBeUndefined();
  expect(arr[fractional]).toBeUndefined();
  arr[negative] = "minus";
  arr[fractional] = "half";
  expect(arr[negative]).toBe("minus");
  expect(arr[fractional]).toBe("half");
  expect(arr.length).toBe(3);
});
