describe("expect.any", () => {
  test("matches built-in primitive and object types", () => {
    expect("value").toEqual(expect.any(String));
    expect(42).toEqual(expect.any(Number));
    expect(true).toEqual(expect.any(Boolean));
    expect(1n).toEqual(expect.any(BigInt));
    expect(Symbol("value")).toEqual(expect.any(Symbol));
    expect(() => {}).toEqual(expect.any(Function));
    expect({}).toEqual(expect.any(Object));
    expect(null).toEqual(expect.any(Object));
  });

  test("matches class instances through instanceof semantics", () => {
    class Example {}

    expect(new Example()).toEqual(expect.any(Example));
    expect({}).not.toEqual(expect.any(Example));
  });

  test("works as a toHaveProperty expected value", () => {
    expect({ value: 42 }).toHaveProperty("value", expect.any(Number));
  });

  test("does not expose a negated factory", () => {
    expect(expect.not.any).toBeUndefined();
  });

  test("compares two matcher values by their stored sample", () => {
    expect(expect.any(Number)).toEqual(expect.any(Number));
    expect(expect.any(Number)).not.toEqual(expect.any(String));
  });

  test("requires a constructor argument", () => {
    expect(() => expect.any()).toThrow(
      "any() expects to be passed a constructor function",
    );
  });
});
