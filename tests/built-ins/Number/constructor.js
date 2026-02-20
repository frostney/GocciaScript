describe("Number constructor", () => {
  test("new Number() creates wrapper object", () => {
    const n = new Number(42);
    expect(typeof n).toBe("object");
    expect(n.valueOf()).toBe(42);
  });

  test("new Number() has number methods", () => {
    const n = new Number(3.14159);
    expect(n.toFixed(2)).toBe("3.14");
  });

  test("Number() as function returns primitive", () => {
    const n = Number("42");
    expect(typeof n).toBe("number");
    expect(n).toBe(42);
  });

  test("new Number() instanceof Number", () => {
    const n = new Number(42);
    expect(n instanceof Number).toBe(true);
  });
});
