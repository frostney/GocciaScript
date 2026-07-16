describe("expect.arrayContaining", () => {
  test("matches a subset in any order with nested matchers", () => {
    expect(["extra", 42, { active: true }]).toEqual(
      expect.arrayContaining([expect.any(Number), { active: true }]),
    );
    expect([1]).toEqual(expect.arrayContaining([]));
    expect("not an array").toEqual(expect.arrayContaining([]));
    expect([1]).toEqual(expect.arrayContaining([1, 1]));
    expect([]).toEqual(expect.arrayContaining([,]));
  });

  test("supports expect.not.arrayContaining", () => {
    expect([1, 2]).toEqual(expect.not.arrayContaining([3]));
    expect([1, 2]).not.toEqual(expect.not.arrayContaining([2]));
  });

  test("rejects a non-array sample when matched", () => {
    expect(() => expect([]).toEqual(expect.arrayContaining({}))).toThrow(
      "You must provide an array to ArrayContaining",
    );
  });
});
