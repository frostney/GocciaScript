describe("JSON5.stringify", () => {
  test("stringifies objects with JSON5 key and string syntax", () => {
    expect(JSON5.stringify({ a: 1 })).toBe("{a:1}");
    expect(JSON5.stringify({ "a-b": 1 })).toBe("{'a-b':1}");
    expect(JSON5.stringify({ "a'": 1 })).toBe('{"a\'":1}');
    expect(JSON5.stringify({ "": 1 })).toBe("{'':1}");
    expect(JSON5.stringify({ $_: 1, _$: 2, "a\u200C": 3 })).toBe(
      "{$_:1,_$:2,a\u200C:3}",
    );
  });

  test("stringifies arrays and nested structures", () => {
    expect(JSON5.stringify([1, 2])).toBe("[1,2]");
    expect(JSON5.stringify({ a: { b: 2 } })).toBe("{a:{b:2}}");
    expect(JSON5.stringify([1, [2, 3]])).toBe("[1,[2,3]]");
  });

  test("preserves special numeric values", () => {
    expect(JSON5.stringify([Infinity, -Infinity, NaN])).toBe(
      "[Infinity,-Infinity,NaN]",
    );
    expect(JSON5.stringify({ inf: Infinity, ninf: -Infinity, nan: NaN })).toBe(
      "{inf:Infinity,ninf:-Infinity,nan:NaN}",
    );
    expect(JSON5.stringify(new Number(-1.2))).toBe("-1.2");
  });

  test("stringifies strings with JSON5 quoting and escapes", () => {
    expect(JSON5.stringify("abc")).toBe("'abc'");
    expect(JSON5.stringify("abc'")).toBe('"abc\'"');
    expect(
      JSON5.stringify(
        String.fromCodePoint(92, 8, 12, 10, 13, 9, 11, 0, 15),
      ),
    ).toBe(
      String.fromCodePoint(
        39, 92, 92, 92, 98, 92, 102, 92, 110, 92, 114, 92, 116, 92, 118,
        92, 48, 92, 120, 48, 70, 39,
      ),
    );
    expect(JSON5.stringify("\0\x001")).toBe("'\\0\\x001'");
    expect(JSON5.stringify("\u2028\u2029")).toBe("'\\u2028\\u2029'");
  });

  test("uses toJSON5 before toJSON", () => {
    class Example {
      toJSON() {
        return { a: 1 };
      }

      toJSON5() {
        return { a: 2 };
      }
    }

    expect(JSON5.stringify(new Example())).toBe("{a:2}");
  });

  test("supports pretty printing with trailing commas", () => {
    expect(JSON5.stringify([1], null, 2)).toBe("[\n  1,\n]");
    expect(JSON5.stringify({ a: 1 }, null, 2)).toBe("{\n  a: 1,\n}");
    expect(JSON5.stringify({ a: { b: 2 } }, null, 2)).toBe(
      "{\n  a: {\n    b: 2,\n  },\n}",
    );
    expect(JSON5.stringify([1], null, "\t")).toBe("[\n\t1,\n]");
  });

  test("supports replacer arrays and functions", () => {
    expect(JSON5.stringify({ a: 1, b: 2, 3: 3 }, ["a", 3])).toBe("{a:1,'3':3}");
    expect(
      JSON5.stringify({ a: 1, b: 2, 3: 3 }, [new String("a"), new Number(3)]),
    ).toBe("{a:1,'3':3}");
    expect(
      JSON5.stringify({ a: 1, b: 2 }, (key, value) => {
        if (key === "a") {
          return 2;
        }
        return value;
      }),
    ).toBe("{a:2,b:2}");
    expect(
      JSON5.stringify(
        { a: 1 },
        (key, value) => {
          JSON5.stringify({}, null, 4);
          return value;
        },
        2,
      ),
    ).toBe("{\n  a: 1,\n}");
  });

  test("supports options objects and quote overrides", () => {
    expect(
      JSON5.stringify({ a: 1, b: 2, 3: 3 }, { replacer: ["a", 3] }),
    ).toBe("{a:1,'3':3}");
    expect(JSON5.stringify([1], { space: 2 })).toBe("[\n  1,\n]");
    expect(JSON5.stringify([1], { space: new Number(2) })).toBe("[\n  1,\n]");
    expect(
      JSON5.stringify({ 'a"': '1"' }, { quote: '"' }),
    ).toBe("{\"a\\\"\":\"1\\\"\"}");
    expect(
      JSON5.stringify({ "a'": "1'" }, { quote: "'" }),
    ).toBe("{'a\\'':'1\\''}");
  });

  test("matches JSON semantics for omitted and nullified values", () => {
    expect(JSON5.stringify(undefined)).toBeUndefined();
    expect(JSON5.stringify(() => {})).toBeUndefined();
    expect(JSON5.stringify(new String("abc"))).toBe("'abc'");
    expect(JSON5.stringify(new Boolean(true))).toBe("true");
    expect(JSON5.stringify({ keep: 1, fn() {} })).toBe("{keep:1}");
    expect(JSON5.stringify([() => {}])).toBe("[null]");
    expect(JSON5.stringify({ keep: 1, missing: undefined })).toBe("{keep:1}");
  });

  test("throws on circular structures", () => {
    const objectValue = {};
    objectValue.self = objectValue;

    const arrayValue = [];
    arrayValue[0] = arrayValue;

    expect(() => JSON5.stringify(objectValue)).toThrow(TypeError);
    expect(() => JSON5.stringify(arrayValue)).toThrow(TypeError);
  });
});
