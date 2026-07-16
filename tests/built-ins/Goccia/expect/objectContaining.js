describe("expect.objectContaining", () => {
  test("matches inherited actual properties and nested matchers", () => {
    const symbol = Symbol("inherited");
    const prototype = { inherited: 42 };
    prototype[symbol] = 7;
    const actual = Object.create(prototype);
    actual.own = { enabled: true };

    expect(actual).toEqual(
      expect.objectContaining({
        inherited: expect.any(Number),
        own: expect.objectContaining({ enabled: true }),
        [symbol]: 7,
      }),
    );
  });

  test("supports expect.not.objectContaining", () => {
    expect({ value: 1 }).toEqual(expect.not.objectContaining({ value: 2 }));
    expect({ value: 1 }).not.toEqual(
      expect.not.objectContaining({ value: 1 }),
    );
  });

  test("works through other deep-equality matchers", () => {
    expect({ value: 1, extra: true }).toMatchObject({
      value: expect.closeTo(1.004, 2),
    });
    expect([{ id: 7 }]).toContainEqual({ id: expect.any(Number) });
  });

  test("uses Vitest primitive boxing semantics", () => {
    expect("abc").toEqual(expect.objectContaining({ 0: "a" }));
    expect("abc").toEqual(expect.objectContaining({ length: 3 }));
    expect(null).toEqual(expect.objectContaining({}));
    expect(undefined).toEqual(expect.objectContaining({}));
    expect(0).toEqual(expect.objectContaining({}));
    expect(1).not.toEqual(expect.objectContaining({ 0: 1 }));
    expect([undefined]).toEqual([,]);
    expect([undefined]).not.toStrictEqual([,]);
  });

  test("rejects a non-object sample when matched", () => {
    expect(() => expect({}).toEqual(expect.objectContaining(1))).toThrow(
      "You must provide an object to ObjectContaining",
    );
  });
});
