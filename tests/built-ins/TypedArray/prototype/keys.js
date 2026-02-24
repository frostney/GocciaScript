describe("TypedArray.prototype.keys", () => {
  test("iterates over indices", () => {
    const ta = new Int32Array([10, 20, 30]);
    const result = [];
    for (const k of ta.keys()) {
      result.push(k);
    }
    expect(result.length).toBe(3);
    expect(result[0]).toBe(0);
    expect(result[1]).toBe(1);
    expect(result[2]).toBe(2);
  });

  test("on empty returns done immediately", () => {
    const ta = new Int32Array(0);
    const iter = ta.keys();
    const result = iter.next();
    expect(result.done).toBe(true);
  });
});
