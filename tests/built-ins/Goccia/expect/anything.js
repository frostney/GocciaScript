describe("expect.anything", () => {
  test("matches every value except null and undefined", () => {
    expect(0).toEqual(expect.anything());
    expect(false).toStrictEqual(expect.anything());
    expect("").toEqual(expect.anything());
    expect(null).not.toEqual(expect.anything());
    expect(undefined).not.toEqual(expect.anything());
  });

  test("participates in nested and symmetric deep equality", () => {
    expect({ value: 42 }).toEqual({ value: expect.anything() });
    expect(expect.anything()).toEqual(42);
  });

  test("does not expose a negated factory", () => {
    expect(expect.not.anything).toBeUndefined();
  });
});
