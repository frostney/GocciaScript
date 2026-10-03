describe("Object.isExtensible", () => {
  test("returns true for regular objects", () => {
    expect(Object.isExtensible({})).toBe(true);
  });

  test("returns false after preventExtensions", () => {
    const obj = {};
    Object.preventExtensions(obj);
    expect(Object.isExtensible(obj)).toBe(false);
  });

  test("returns false for frozen objects", () => {
    const obj = {};
    Object.freeze(obj);
    expect(Object.isExtensible(obj)).toBe(false);
  });

  test("returns false for sealed objects", () => {
    const obj = {};
    Object.seal(obj);
    expect(Object.isExtensible(obj)).toBe(false);
  });

  test("non-objects return false", () => {
    expect(Object.isExtensible(42)).toBe(false);
    expect(Object.isExtensible("hello")).toBe(false);
  });

  test("returns false for a class after preventExtensions, seal or freeze", () => {
    class Plain {}
    class NonExtensible {}
    class Sealed {}
    class Frozen {}
    Object.preventExtensions(NonExtensible);
    Object.seal(Sealed);
    Object.freeze(Frozen);

    expect(Object.isExtensible(Plain)).toBe(true);
    expect(Object.isExtensible(NonExtensible)).toBe(false);
    expect(Object.isExtensible(Sealed)).toBe(false);
    expect(Object.isExtensible(Frozen)).toBe(false);
  });
});
