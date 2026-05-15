/*---
description: Non-strict compatibility keeps array length truncation behavior for high indexes
features: [compat-non-strict-mode]
---*/

describe("non-strict array length", () => {
  test("setting length deletes the maximum array index property", () => {
    const array = [0, 1, 2];

    array[4294967294] = 4294967294;
    array.length = 2;

    expect(array[0]).toBe(0);
    expect(array[1]).toBe(1);
    expect(array[2]).toBeUndefined();
    expect(array[4294967294]).toBeUndefined();
  });
});
