describe("TypedArray.of", () => {
  test("Int32Array.of creates from arguments", () => {
    const ta = Int32Array.of(1, 2, 3);
    expect(ta.length).toBe(3);
    expect(ta[0]).toBe(1);
    expect(ta[1]).toBe(2);
    expect(ta[2]).toBe(3);
  });

  test("Float64Array.of with decimals", () => {
    const ta = Float64Array.of(1.5, 2.5);
    expect(ta.length).toBe(2);
    expect(ta[0]).toBe(1.5);
    expect(ta[1]).toBe(2.5);
  });

  test("Uint8Array.of with truncation", () => {
    const ta = Uint8Array.of(1, 256, 300);
    expect(ta[0]).toBe(1);
    expect(ta[1]).toBe(0);
    expect(ta[2]).toBe(44);
  });

  test("with no arguments", () => {
    const ta = Int32Array.of();
    expect(ta.length).toBe(0);
  });
});
