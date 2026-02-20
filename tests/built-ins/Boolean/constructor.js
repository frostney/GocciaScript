describe("Boolean constructor", () => {
  test("new Boolean() creates wrapper object", () => {
    const b = new Boolean(true);
    expect(typeof b).toBe("object");
    expect(b.valueOf()).toBe(true);
  });

  test("new Boolean(false).valueOf() is false", () => {
    const b = new Boolean(false);
    expect(b.valueOf()).toBe(false);
  });

  test("Boolean() as function returns primitive", () => {
    const b = Boolean(1);
    expect(typeof b).toBe("boolean");
    expect(b).toBe(true);
  });

  test("new Boolean() instanceof Boolean", () => {
    const b = new Boolean(true);
    expect(b instanceof Boolean).toBe(true);
  });
});
