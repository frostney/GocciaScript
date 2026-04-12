describe("TextEncoder constructor", () => {
  test("creates an instance with no arguments", () => {
    const enc = new TextEncoder();
    expect(enc instanceof TextEncoder).toBe(true);
  });

  test("encoding property is utf-8", () => {
    const enc = new TextEncoder();
    expect(enc.encoding).toBe("utf-8");
  });

  test("has encode method", () => {
    const enc = new TextEncoder();
    expect(typeof enc.encode).toBe("function");
  });

  test("has encodeInto method", () => {
    const enc = new TextEncoder();
    expect(typeof enc.encodeInto).toBe("function");
  });

  test("prototype has correct toStringTag", () => {
    const enc = new TextEncoder();
    expect(Object.prototype.toString.call(enc)).toBe("[object TextEncoder]");
  });
});
