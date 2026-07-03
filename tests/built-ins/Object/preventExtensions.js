describe("Object.preventExtensions", () => {
  test("prevents adding new properties", () => {
    const obj = { a: 1 };
    Object.preventExtensions(obj);
    expect(() => { obj.b = 2; }).toThrow(TypeError);
  });

  test("allows modifying existing properties", () => {
    const obj = { a: 1 };
    Object.preventExtensions(obj);
    obj.a = 2;
    expect(obj.a).toBe(2);
  });

  test("returns the same object", () => {
    const obj = {};
    const result = Object.preventExtensions(obj);
    expect(result).toBe(obj);
  });

  test("throws when the object internal method returns false", () => {
    const buffer = new ArrayBuffer(4, { maxByteLength: 8 });

    expect(() => Object.preventExtensions(new Uint8Array(buffer))).toThrow(TypeError);
  });

  test("non-objects are returned as-is", () => {
    expect(Object.preventExtensions(42)).toBe(42);
  });
});
