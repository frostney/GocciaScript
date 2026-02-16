describe("optional chaining edge cases", () => {
  test("chaining on null returns undefined", () => {
    const obj = null;
    expect(obj?.prop).toBe(undefined);
  });

  test("chaining on undefined returns undefined", () => {
    const obj = undefined;
    expect(obj?.prop).toBe(undefined);
  });

  test("chaining on existing object accesses property", () => {
    const obj = { a: { b: 42 } };
    expect(obj?.a?.b).toBe(42);
  });

  test("chaining stops at null in chain", () => {
    const obj = { a: null };
    expect(obj?.a?.b).toBe(undefined);
  });
});
