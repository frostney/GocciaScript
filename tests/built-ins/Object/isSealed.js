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
});
