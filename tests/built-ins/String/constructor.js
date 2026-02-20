describe("String constructor", () => {
  test("new String() creates wrapper object", () => {
    const s = new String("hello");
    expect(typeof s).toBe("object");
    expect(s.valueOf()).toBe("hello");
  });

  test("new String() has string methods", () => {
    const s = new String("hello");
    expect(s.toUpperCase()).toBe("HELLO");
    expect(s.slice(1, 3)).toBe("el");
    expect(s.includes("ell")).toBe(true);
  });

  test("new String().length works", () => {
    const s = new String("hello");
    expect(s.length).toBe(5);
  });

  test("new String() index access works", () => {
    const s = new String("hello");
    expect(s[0]).toBe("h");
    expect(s[4]).toBe("o");
  });

  test("String() as function returns primitive", () => {
    const s = String(42);
    expect(typeof s).toBe("string");
    expect(s).toBe("42");
  });

  test("new String() instanceof String", () => {
    const s = new String("hello");
    expect(s instanceof String).toBe(true);
  });
});
