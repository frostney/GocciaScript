describe("TypedArray.prototype.entries", () => {
  test("iterates over [index, value] pairs", () => {
    const ta = new Int32Array([10, 20, 30]);
    const result = [];
    for (const entry of ta.entries()) {
      result.push(entry);
    }
    expect(result.length).toBe(3);
    expect(result[0][0]).toBe(0);
    expect(result[0][1]).toBe(10);
    expect(result[2][0]).toBe(2);
    expect(result[2][1]).toBe(30);
  });

  test("on empty returns done immediately", () => {
    const ta = new Int32Array(0);
    const iter = ta.entries();
    const result = iter.next();
    expect(result.done).toBe(true);
  });
});
