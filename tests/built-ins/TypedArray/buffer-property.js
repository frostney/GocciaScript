describe("TypedArray buffer property", () => {
  test("returns the backing ArrayBuffer", () => {
    const buf = new ArrayBuffer(8);
    const ta = new Int32Array(buf);
    expect(ta.buffer).toBe(buf);
  });

  test("auto-created buffer is accessible", () => {
    const ta = new Int32Array(4);
    expect(ta.buffer).toBeDefined();
    expect(ta.buffer.byteLength).toBe(16);
  });

  test("subarray shares the same buffer", () => {
    const ta = new Int32Array([1, 2, 3, 4]);
    const sub = ta.subarray(1);
    expect(sub.buffer).toBe(ta.buffer);
  });
});
