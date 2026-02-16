describe("bitwise operator edge cases", () => {
  test("bitwise OR with zero", () => {
    expect((5 | 0)).toBe(5);
    expect((0 | 0)).toBe(0);
  });

  test("bitwise AND mask", () => {
    expect((0xFF & 0x0F)).toBe(15);
  });

  test("bitwise XOR", () => {
    expect((5 ^ 3)).toBe(6);
  });

  test("bitwise NOT", () => {
    expect((~0)).toBe(-1);
    expect((~-1)).toBe(0);
  });

  test("left shift", () => {
    expect((1 << 3)).toBe(8);
  });

  test("right shift", () => {
    expect((8 >> 2)).toBe(2);
  });

  test("unsigned right shift", () => {
    expect((-1 >>> 0)).toBe(4294967295);
  });
});
