describe("TypedArray.prototype.forEach", () => {
  test("calls callback for each element", () => {
    const ta = new Int32Array([10, 20, 30]);
    const collected = [];
    ta.forEach((val, idx) => {
      collected.push([idx, val]);
    });
    expect(collected.length).toBe(3);
    expect(collected[0][0]).toBe(0);
    expect(collected[0][1]).toBe(10);
    expect(collected[2][1]).toBe(30);
  });

  test("returns undefined", () => {
    const ta = new Int32Array([1, 2, 3]);
    const result = ta.forEach((_x) => {});
    expect(result).toBeUndefined();
  });

  test("without callback throws TypeError", () => {
    const ta = new Int32Array([1, 2, 3]);
    expect(() => ta.forEach()).toThrow(TypeError);
  });

  test("passes thisArg to callback", () => {
    const ta = new Int32Array([1, 2, 3]);
    const ctx = { sum: 0 };
    const obj = { fn(x) { this.sum += x; } };
    ta.forEach(obj.fn, ctx);
    expect(ctx.sum).toBe(6);
  });

  test.each([BigInt64Array, BigUint64Array])("%s forEach", (TA) => {
    const ta = new TA([1n, 2n, 3n]);
    const collected = [];
    ta.forEach(x => collected.push(x));
    expect(collected[0]).toBe(1n);
    expect(collected[1]).toBe(2n);
    expect(collected[2]).toBe(3n);
  });
});
