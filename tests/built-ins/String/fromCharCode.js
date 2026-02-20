describe("String.fromCharCode", () => {
  test("single ASCII character", () => {
    expect(String.fromCharCode(65)).toBe("A");
    expect(String.fromCharCode(97)).toBe("a");
    expect(String.fromCharCode(48)).toBe("0");
  });

  test("multiple characters", () => {
    expect(String.fromCharCode(72, 101, 108, 108, 111)).toBe("Hello");
  });

  test("no arguments returns empty string", () => {
    expect(String.fromCharCode()).toBe("");
  });

  test("null character", () => {
    expect(String.fromCharCode(0).length).toBe(1);
  });

  test("wraps values to 16-bit unsigned", () => {
    expect(String.fromCharCode(65536 + 65)).toBe("A");
    expect(String.fromCharCode(-1)).toBe(String.fromCharCode(65535));
  });
});
