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

  test("sealing String objects preserves virtual indices and length", () => {
    const str = new String("abc");
    str.foo = 10;

    Object.seal(str);

    const index = Object.getOwnPropertyDescriptor(str, "0");
    const length = Object.getOwnPropertyDescriptor(str, "length");
    const foo = Object.getOwnPropertyDescriptor(str, "foo");

    expect(Object.isSealed(str)).toBe(true);
    expect(index.value).toBe("a");
    expect(index.writable).toBe(false);
    expect(index.enumerable).toBe(true);
    expect(index.configurable).toBe(false);
    expect(length.value).toBe(3);
    expect(length.writable).toBe(false);
    expect(length.enumerable).toBe(false);
    expect(length.configurable).toBe(false);
    expect(foo.value).toBe(10);
    expect(foo.writable).toBe(true);
    expect(foo.configurable).toBe(false);
  });
});
