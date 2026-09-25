describe("toHaveLength", () => {
  test("reads array length", () => {
    expect([1, 2, 3]).toHaveLength(3);
    expect([]).toHaveLength(0);
    expect([1]).not.toHaveLength(2);
  });

  test("reads string length", () => {
    expect("abc").toHaveLength(3);
    expect("").toHaveLength(0);
  });

  test("reads size for a Set", () => {
    expect(new Set([1, 2])).toHaveLength(2);
    expect(new Set()).toHaveLength(0);
    expect(new Set([1, 2])).not.toHaveLength(3);
  });

  test("reads size for a Map", () => {
    expect(new Map([[1, 2]])).toHaveLength(1);
    expect(new Map()).toHaveLength(0);
    expect(new Map([[1, 2]])).not.toHaveLength(2);
  });
});
