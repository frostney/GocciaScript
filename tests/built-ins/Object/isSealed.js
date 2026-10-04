describe("Object.isSealed", () => {
  test("returns false for regular objects", () => {
    expect(Object.isSealed({ a: 1 })).toBe(false);
  });

  test("returns true for sealed objects", () => {
    const obj = { a: 1 };
    Object.seal(obj);
    expect(Object.isSealed(obj)).toBe(true);
  });

  test("frozen objects are also sealed", () => {
    const obj = { a: 1 };
    Object.freeze(obj);
    expect(Object.isSealed(obj)).toBe(true);
  });

  test("non-objects return true", () => {
    expect(Object.isSealed(42)).toBe(true);
    expect(Object.isSealed("hello")).toBe(true);
  });

  test("non-extensible object with non-configurable properties is structurally sealed", () => {
    const obj = {};
    Object.defineProperty(obj, "a", {
      value: 1,
      writable: true,
      configurable: false,
      enumerable: true,
    });
    Object.preventExtensions(obj);
    expect(Object.isSealed(obj)).toBe(true);
  });

  test("non-extensible object with configurable property is not sealed", () => {
    const obj = {};
    Object.defineProperty(obj, "a", {
      value: 1,
      writable: false,
      configurable: true,
      enumerable: true,
    });
    Object.preventExtensions(obj);
    expect(Object.isSealed(obj)).toBe(false);
  });

  test("empty non-extensible object is sealed", () => {
    const obj = {};
    Object.preventExtensions(obj);
    expect(Object.isSealed(obj)).toBe(true);
  });

  test("extensible object is never sealed", () => {
    const obj = {};
    Object.defineProperty(obj, "a", {
      value: 1,
      writable: false,
      configurable: false,
    });
    expect(Object.isSealed(obj)).toBe(false);
  });

  test("a class is sealed once it is non-extensible and its length and name are non-configurable", () => {
    class K {
      static level = 1;
    }
    expect(Object.isSealed(K)).toBe(false);

    Object.preventExtensions(K);
    expect(Object.isSealed(K)).toBe(false);

    Object.seal(K);
    expect(Object.isSealed(K)).toBe(true);

    class Frozen {}
    Object.freeze(Frozen);
    expect(Object.isSealed(Frozen)).toBe(true);
  });
});
