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
});
