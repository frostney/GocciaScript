describe("Object.setPrototypeOf", () => {
  test("sets the prototype of an object", () => {
    const proto = { greet: () => "hello" };
    const obj = {};
    Object.setPrototypeOf(obj, proto);
    expect(obj.greet()).toBe("hello");
  });

  test("returns the object", () => {
    const obj = {};
    const result = Object.setPrototypeOf(obj, {});
    expect(result).toBe(obj);
  });

  test("setting prototype to null", () => {
    const obj = {};
    Object.setPrototypeOf(obj, null);
    expect(Object.getPrototypeOf(obj)).toBe(null);
  });

  test("returns primitive unchanged for non-null/undefined non-object", () => {
    expect(Object.setPrototypeOf(42, null)).toBe(42);
    expect(Object.setPrototypeOf(true, null)).toBe(true);
    expect(Object.setPrototypeOf("str", null)).toBe("str");
  });

  test("throws for null first argument", () => {
    expect(() => Object.setPrototypeOf(null, null)).toThrow(TypeError);
  });

  test("throws for undefined first argument", () => {
    expect(() => Object.setPrototypeOf(undefined, null)).toThrow(TypeError);
  });
});
