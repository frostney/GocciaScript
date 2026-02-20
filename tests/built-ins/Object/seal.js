describe("Object.seal", () => {
  test("prevents adding new properties", () => {
    const obj = { a: 1 };
    Object.seal(obj);
    expect(() => { obj.b = 2; }).toThrow(TypeError);
  });

  test("allows modifying existing writable properties", () => {
    const obj = { a: 1 };
    Object.seal(obj);
    obj.a = 2;
    expect(obj.a).toBe(2);
  });

  test("returns the same object", () => {
    const obj = { a: 1 };
    const result = Object.seal(obj);
    expect(result).toBe(obj);
  });

  test("non-objects are returned as-is", () => {
    expect(Object.seal(42)).toBe(42);
    expect(Object.seal("hello")).toBe("hello");
  });

  test("sealed object is not extensible", () => {
    const obj = { a: 1 };
    Object.seal(obj);
    expect(Object.isExtensible(obj)).toBe(false);
  });
});
