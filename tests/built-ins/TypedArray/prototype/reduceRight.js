describe("TypedArray.prototype.reduceRight", () => {
  test("processes right to left", () => {
    const ta = new Int32Array([1, 2, 3]);
    const result = ta.reduceRight((acc, val) => acc * 10 + val, 0);
    expect(result).toBe(321);
  });

  test("on empty without initial value throws TypeError", () => {
    const ta = new Int32Array(0);
    expect(() => ta.reduceRight((acc, val) => acc + val)).toThrow(TypeError);
  });

  test("with initial value on empty array returns initial", () => {
    const ta = new Int32Array(0);
    expect(ta.reduceRight((acc, val) => acc + val, 99)).toBe(99);
  });

  test("single element with no initial value", () => {
    const ta = new Int32Array([42]);
    expect(ta.reduceRight((acc, val) => acc + val)).toBe(42);
  });

  test("without callback throws TypeError", () => {
    const ta = new Int32Array([1, 2, 3]);
    expect(() => ta.reduceRight()).toThrow(TypeError);
  });
});
