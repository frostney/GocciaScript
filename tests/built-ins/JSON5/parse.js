describe("JSON5.parse", () => {
  test("parses comments, trailing commas, and unquoted keys", () => {
    const value = JSON5.parse(`
{
  // comment
  unquoted: 'goccia',
  trailing: [1, 2, 3,],
  nested: {
    enabled: true,
  },
}
`);

    expect(value.unquoted).toBe("goccia");
    expect(value.trailing.length).toBe(3);
    expect(value.trailing[2]).toBe(3);
    expect(value.nested.enabled).toBe(true);
  });

  test("parses JSON5 numeric extensions", () => {
    const value = JSON5.parse(`
{
  hex: 0xdecaf,
  leadingDot: .5,
  trailingDot: 5.,
  positive: +1,
  infinite: Infinity,
  negativeInfinite: -Infinity,
  nanValue: NaN,
  negativeZero: -0,
}
`);

    expect(value.hex).toBe(0xdecaf);
    expect(value.leadingDot).toBe(0.5);
    expect(value.trailingDot).toBe(5);
    expect(value.positive).toBe(1);
    expect(value.infinite).toBe(Infinity);
    expect(value.negativeInfinite).toBe(-Infinity);
    expect(Number.isNaN(value.nanValue)).toBe(true);
    expect(Object.is(value.negativeZero, -0)).toBe(true);
  });

  test("parses escapes, line continuations, and identifier escapes", () => {
    const value = JSON5.parse(`
{
  lineBreaks: "Look, Mom! \\
No \\\\n's!",
  escapedKey\\u0031: '\\x41\\v\\0',
  ùńîċõďë: 9,
}
`);

    expect(value.lineBreaks).toBe("Look, Mom! No \\n's!");
    expect(value.escapedKey1).toBe(String.fromCodePoint(65, 11, 0));
    expect(value["ùńîċõďë"]).toBe(9);
  });

  test("preserves __proto__ as data", () => {
    const value = JSON5.parse('{__proto__: 1}');
    expect(value["__proto__"]).toBe(1);
  });

  test("supports reviver transforms", () => {
    const value = JSON5.parse('{answer: 21, nested: { count: 1 }}', (key, item) => {
      if (typeof item === "number") {
        return item * 2;
      }
      return item;
    });

    expect(value.answer).toBe(42);
    expect(value.nested.count).toBe(2);
  });

  test("reviver can remove object properties and array elements", () => {
    const objectValue = JSON5.parse('{keep: 1, drop: 2}', (key, item) => {
      if (key === "drop") {
        return undefined;
      }
      return item;
    });
    const arrayValue = JSON5.parse('[1, 2, 3]', (key, item) => {
      if (key === "1") {
        return undefined;
      }
      return item;
    });

    expect(objectValue.keep).toBe(1);
    expect(objectValue.drop).toBeUndefined();
    expect(arrayValue.length).toBe(3);
    expect(1 in arrayValue).toBe(false);
  });

  test("throws on malformed JSON5 syntax", () => {
    expect(() => JSON5.parse("{")).toThrow(SyntaxError);
    expect(() => JSON5.parse("{a:}")).toThrow(SyntaxError);
    expect(() => JSON5.parse("{a: 0x}")).toThrow(SyntaxError);
    expect(() => JSON5.parse("{a: '\\1'}")).toThrow(SyntaxError);
    expect(() => JSON5.parse("{\\u0021: 1}")).toThrow(SyntaxError);
    expect(() => JSON5.parse("{\\u00A0: 1}")).toThrow(SyntaxError);
    expect(() => JSON5.parse("/*")).toThrow(SyntaxError);
  });
});
