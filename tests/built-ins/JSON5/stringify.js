const hasJSON5 = typeof JSON5 !== "undefined";

describe.runIf(hasJSON5)("JSON5.stringify", () => {
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
      JSON5.stringify(String.fromCodePoint(92, 8, 12, 10, 13, 9, 11, 0, 15)),
    ).toBe(
      String.fromCodePoint(
        39,
        92,
        92,
        92,
        98,
        92,
        102,
        92,
        110,
        92,
        114,
        92,
        116,
        92,
        118,
        92,
        48,
        92,
        120,
        48,
        70,
        39,
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

  test("pretty-prints deeply nested objects with a trailing comma at each level", () => {
    const makeNested = (depth) =>
      Array.from({ length: depth }).reduce((acc) => ({ child: acc }), {
        leaf: true,
      });
    const expected = [
      "{",
      "  child: {",
      "    child: {",
      "      child: {",
      "        leaf: true,",
      "      },",
      "    },",
      "  },",
      "}",
    ].join("\n");
    expect(JSON5.stringify(makeNested(3), null, 2)).toBe(expected);
  });

  test("supports replacer arrays and functions", () => {
    expect(JSON5.stringify({ a: 1, b: 2, 3: 3 }, ["a", 3])).toBe("{a:1,'3':3}");
    expect(JSON5.stringify({ a: { a: 1, b: 2 }, b: { a: 3 } }, ["a"])).toBe(
      "{a:{a:1}}",
    );
    expect(
      JSON5.stringify({ a: 1, b: 2, 3: 3 }, [new String("a"), new Number(3)]),
    ).toBe("{a:1,'3':3}");
    expect(
      JSON5.stringify(
        {
          toJSON5() {
            return { a: 1, b: 2 };
          },
        },
        ["a"],
      ),
    ).toBe("{a:1}");
    expect(
      JSON5.stringify(
        {
          a: {
            toJSON5() {
              return { a: 1, b: 2 };
            },
          },
        },
        ["a"],
      ),
    ).toBe("{a:{a:1}}");
    expect(
      JSON5.stringify({ a: 1, b: 2 }, (key, value) => {
        if (key === "a") {
          return 2;
        }
        return value;
      }),
    ).toBe("{a:2,b:2}");
    expect(
      JSON5.stringify({ a: 1, b: 2 }, (key, value) =>
        key === "a" ? () => {} : value),
    ).toBe("{b:2}");
    expect(
      JSON5.stringify([1, 2], (key, value) =>
        key === "0" ? Symbol("a") : value),
    ).toBe("[null,2]");
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
    const emojiIndent = "🙂".repeat(12);

    expect(JSON5.stringify({ a: 1, b: 2, 3: 3 }, { replacer: ["a", 3] })).toBe(
      "{a:1,'3':3}",
    );
    expect(JSON5.stringify([1], { space: 2 })).toBe("[\n  1,\n]");
    expect(JSON5.stringify([1], { space: new Number(2) })).toBe("[\n  1,\n]");
    expect(JSON5.stringify([1], { space: NaN })).toBe("[1]");
    expect(JSON5.stringify([1], { space: Infinity })).toBe(
      "[\n          1,\n]",
    );
    expect(JSON5.stringify([1], { space: emojiIndent })).toBe(
      `[\n${"🙂".repeat(10)}1,\n]`,
    );
    expect(JSON5.stringify({ 'a"': '1"' }, { quote: '"' })).toBe(
      '{"a\\"":"1\\""}',
    );
    expect(JSON5.stringify({ "a'": "1'" }, { quote: "'" })).toBe(
      "{'a\\'':'1\\''}",
    );
  });

  test("matches JSON semantics for omitted and nullified values", () => {
    const nbsp = String.fromCodePoint(160);

    expect(JSON5.stringify(undefined)).toBeUndefined();
    expect(JSON5.stringify(() => {})).toBeUndefined();
    expect(JSON5.stringify(new String("abc"))).toBe("'abc'");
    expect(JSON5.stringify(new Boolean(true))).toBe("true");
    expect(JSON5.stringify({ toJSON5() { return undefined; } })).toBeUndefined();
    expect(JSON5.stringify({ keep: 1, fn() {} })).toBe("{keep:1}");
    expect(JSON5.stringify([() => {}])).toBe("[null]");
    expect(JSON5.stringify(new Array(2))).toBe("[null,null]");
    expect(JSON5.stringify({ keep: 1, missing: undefined })).toBe("{keep:1}");
    expect(JSON5.stringify({ ["a" + nbsp + "b"]: 1 })).toBe(`{'a${nbsp}b':1}`);
  });

  test("uses an overridden valueOf on boxed Number values", () => {
    const boxed = new Number(1);
    boxed.valueOf = () => 4;

    expect(JSON5.stringify({ a: boxed })).toBe("{a:4}");
    expect(JSON5.stringify(boxed)).toBe("4");
  });

  test("root boxed Number calls valueOf once", () => {
    let calls = 0;
    const boxed = new Number(1);
    boxed.valueOf = () => {
      calls += 1;
      return 4;
    };

    expect(JSON5.stringify(boxed)).toBe("4");
    expect(calls).toBe(1);
  });

  test("uses an overridden valueOf on boxed Number values with a replacer function", () => {
    const boxed = new Number(1);
    boxed.valueOf = () => 4;

    expect(JSON5.stringify({ a: boxed }, (key, value) => value)).toBe("{a:4}");
  });

  test("uses an overridden valueOf on boxed Number values with an array replacer", () => {
    const boxed = new Number(1);
    boxed.valueOf = () => 4;

    expect(JSON5.stringify({ a: boxed, b: 2 }, ["a"])).toBe("{a:4}");
  });

  test("uses an overridden toString on boxed String values without calling valueOf", () => {
    const boxed = new String("ab");
    boxed.toString = () => "zz";
    boxed.valueOf = () => {
      throw new TypeError("valueOf must not be called");
    };

    expect(JSON5.stringify([boxed])).toBe("['zz']");
  });

  test("reads boxed Boolean values directly, ignoring valueOf and toString", () => {
    const boxed = new Boolean(false);
    boxed.valueOf = () => true;
    boxed.toString = () => "true";

    expect(JSON5.stringify([boxed])).toBe("[false]");
  });

  test("throws on BigInt and boxed BigInt values", () => {
    expect(() => JSON5.stringify(1n)).toThrow(TypeError);
    expect(() => JSON5.stringify(Object(1n))).toThrow(TypeError);
    expect(() => JSON5.stringify({ a: Object(1n) })).toThrow(TypeError);
    expect(() => JSON5.stringify([Object(1n)])).toThrow(TypeError);
  });

  test("propagates valueOf exceptions from boxed Number values", () => {
    const boxed = new Number(1);
    boxed.valueOf = () => {
      throw new RangeError("abrupt valueOf");
    };

    expect(() => JSON5.stringify({ a: boxed })).toThrow(RangeError);
  });

  test("propagates toString exceptions from boxed String values", () => {
    const boxed = new String("ab");
    boxed.toString = () => {
      throw new RangeError("abrupt toString");
    };

    expect(() => JSON5.stringify([boxed])).toThrow(RangeError);
  });

  test("space as boxed Number indents like the primitive", () => {
    const obj = { a: 1, b: [1, 2] };

    expect(JSON5.stringify(obj, null, new Number(2))).toBe(
      JSON5.stringify(obj, null, 2),
    );
  });

  test("space as boxed Number uses an overridden valueOf", () => {
    const space = new Number(1);
    space.valueOf = () => 4;

    expect(JSON5.stringify({ a: 1 }, null, space)).toBe(
      JSON5.stringify({ a: 1 }, null, 4),
    );
  });

  test("space as boxed Number propagates valueOf exceptions", () => {
    const space = new Number(4);
    space.valueOf = () => {
      throw new RangeError("abrupt valueOf");
    };

    expect(() => JSON5.stringify({ a: 1 }, null, space)).toThrow(RangeError);
  });

  test("space as boxed String indents like the primitive", () => {
    const obj = { a: 1, b: [1, 2] };

    expect(JSON5.stringify(obj, null, new String("xx"))).toBe(
      JSON5.stringify(obj, null, "xx"),
    );
  });

  test("space as boxed String uses an overridden toString without calling valueOf", () => {
    const space = new String("xx");
    space.toString = () => "--";
    space.valueOf = () => {
      throw new RangeError("valueOf must not be called");
    };

    expect(JSON5.stringify({ a: 1 }, null, space)).toBe(
      JSON5.stringify({ a: 1 }, null, "--"),
    );
  });

  test("space as boxed String propagates toString exceptions", () => {
    const space = new String("xx");
    space.toString = () => {
      throw new RangeError("abrupt toString");
    };

    expect(() => JSON5.stringify({ a: 1 }, null, space)).toThrow(RangeError);
  });

  test("truncates a fractional space", () => {
    const obj = { a: 1, b: [1, 2] };

    expect(JSON5.stringify(obj, null, 5.99999)).toBe(
      JSON5.stringify(obj, null, 5),
    );
  });

  test("treats non-positive space as no indentation", () => {
    expect(JSON5.stringify({ a: 1 }, null, 0)).toBe("{a:1}");
    expect(JSON5.stringify({ a: 1 }, null, -4)).toBe("{a:1}");
    expect(JSON5.stringify({ a: 1 }, null, -Infinity)).toBe("{a:1}");
  });

  test("clamps huge finite space to 10 spaces", () => {
    const obj = { a: 1, b: [1, 2] };

    expect(JSON5.stringify(obj, null, 2147483648)).toBe(
      JSON5.stringify(obj, null, 10),
    );
    expect(JSON5.stringify(obj, null, 1e15)).toBe(
      JSON5.stringify(obj, null, 10),
    );
  });

  test("clamps boxed Number Infinity space to 10 spaces", () => {
    const obj = { a: 1, b: [1, 2] };

    expect(JSON5.stringify(obj, null, new Number(Infinity))).toBe(
      JSON5.stringify(obj, null, 10),
    );
  });

  test("quote as boxed String behaves like the primitive", () => {
    expect(JSON5.stringify({ "a'": "1'" }, { quote: new String('"') })).toBe(
      JSON5.stringify({ "a'": "1'" }, { quote: '"' }),
    );
  });

  test("quote as boxed String uses an overridden toString", () => {
    const quote = new String('"');
    quote.toString = () => "'";

    expect(JSON5.stringify({ "a'": "1'" }, { quote })).toBe(
      JSON5.stringify({ "a'": "1'" }, { quote: "'" }),
    );
  });

  test("quote as boxed String propagates toString exceptions", () => {
    const quote = new String('"');
    quote.toString = () => {
      throw new RangeError("abrupt quote toString");
    };

    expect(() => JSON5.stringify({ a: 1 }, { quote })).toThrow(RangeError);
  });

  test("allow-list boxed Number uses an overridden toString", () => {
    const key = new Number(3);
    key.toString = () => "a";

    expect(JSON5.stringify({ a: 1, 3: 3 }, [key])).toBe("{a:1}");
  });

  test("allow-list boxed String uses an overridden toString", () => {
    const key = new String("a");
    key.toString = () => "b";

    expect(JSON5.stringify({ a: 1, b: 2 }, [key])).toBe("{b:2}");
  });

  test("allow-list boxed Number propagates toString exceptions", () => {
    const key = new Number(3);
    key.toString = () => {
      throw new RangeError("abrupt allow-list toString");
    };

    expect(() => JSON5.stringify({ a: 1, 3: 3 }, [key])).toThrow(RangeError);
  });

  test("allow-list boxed String propagates toString exceptions", () => {
    const key = new String("a");
    key.toString = () => {
      throw new RangeError("abrupt allow-list toString");
    };

    expect(() => JSON5.stringify({ a: 1, b: 2 }, [key])).toThrow(RangeError);
  });

  test("throws on circular structures", () => {
    const objectValue = {};
    objectValue.self = objectValue;

    const arrayValue = [];
    arrayValue[0] = arrayValue;

    expect(() => JSON5.stringify(objectValue)).toThrow(TypeError);
    expect(() => JSON5.stringify(arrayValue)).toThrow(TypeError);
  });

  test("array replacer converts String and Number wrapper elements via toString", () => {
    const num = new Number(10);
    num.toString = () => "toString";
    num.valueOf = () => {
      throw new Error("valueOf should not be called");
    };
    expect(JSON5.stringify({ 10: 1, toString: 2 }, [num])).toBe(
      "{toString:2}",
    );

    const str = new String("str");
    str.toString = () => "toString";
    str.valueOf = () => {
      throw new Error("valueOf should not be called");
    };
    expect(JSON5.stringify({ str: 1, toString: 2 }, [str])).toBe(
      "{toString:2}",
    );
  });

  test("array replacer extracts each element key once even for nested objects", () => {
    let calls = 0;
    const key = new String("a");
    key.toString = () => {
      calls += 1;
      return "a";
    };
    expect(JSON5.stringify({ a: { a: 1, b: 2 }, b: 3 }, [key])).toBe(
      "{a:{a:1}}",
    );
    expect(calls).toBe(1);
  });

  test("array replacer de-duplicates keys and reads each property once", () => {
    let getCalls = 0;
    const value = {
      get key() {
        getCalls += 1;
        return true;
      },
    };
    expect(JSON5.stringify(value, ["key", "key"])).toBe("{key:true}");
    expect(getCalls).toBe(1);
  });

  test("array replacer reads elements through property access", () => {
    const replacer = new Array(1);
    Object.defineProperty(replacer, "0", {
      get() {
        return "a";
      },
    });

    expect(JSON5.stringify({ a: 1, b: 2 }, replacer)).toBe("{a:1}");
  });

  test("arrays read elements through property access", () => {
    const arr = new Array(1);
    Object.defineProperty(arr, "0", {
      get() {
        return 7;
      },
    });

    expect(JSON5.stringify(arr)).toBe("[7]");
  });
});
