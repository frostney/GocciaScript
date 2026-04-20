describe("TypedArray [Symbol.iterator]", () => {
  test("for-of loop works", () => {
    const ta = new Int32Array([5, 10, 15]);
    const result = [];
    for (const v of ta) {
      result.push(v);
    }
    expect(result.length).toBe(3);
    expect(result[0]).toBe(5);
    expect(result[1]).toBe(10);
    expect(result[2]).toBe(15);
  });

  test("spread operator works", () => {
    const ta = new Int32Array([1, 2, 3]);
    const arr = [...ta];
    expect(arr.length).toBe(3);
    expect(arr[0]).toBe(1);
    expect(arr[1]).toBe(2);
    expect(arr[2]).toBe(3);
  });

  test("destructuring works", () => {
    const ta = new Int32Array([10, 20, 30]);
    const [a, b, c] = ta;
    expect(a).toBe(10);
    expect(b).toBe(20);
    expect(c).toBe(30);
  });

  test("for-of on empty iterates zero times", () => {
    const ta = new Int32Array(0);
    let count = 0;
    for (const v of ta) {
      count = count + 1;
    }
    expect(count).toBe(0);
  });

  test("spread empty produces empty array", () => {
    const ta = new Int32Array(0);
    const arr = [...ta];
    expect(arr.length).toBe(0);
  });

  describe.each([Int8Array, Uint8Array, Uint8ClampedArray, Int16Array, Uint16Array, Int32Array, Uint32Array, Float16Array, Float32Array, Float64Array])("%s", (TA) => {
    test("spread works", () => {
      const ta = new TA([1, 2, 3]);
      const arr = [...ta];
      expect(arr).toEqual([1, 2, 3]);
    });

    test("for-of iterates values", () => {
      const ta = new TA([1, 2, 3]);
      const collected = [];
      for (const v of ta) collected.push(v);
      expect(collected).toEqual([1, 2, 3]);
    });
  });
});
