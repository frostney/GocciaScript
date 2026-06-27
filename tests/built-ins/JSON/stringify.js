/*---
description: JSON.stringify converts JavaScript values to JSON strings
features: [JSON.stringify]
---*/

test("JSON.stringify basic values", () => {
  expect(JSON.stringify(42)).toBe("42");
  expect(JSON.stringify("hello")).toBe('"hello"');
  expect(JSON.stringify(true)).toBe("true");
  expect(JSON.stringify(false)).toBe("false");
  expect(JSON.stringify(null)).toBe("null");
  expect(JSON.stringify(undefined)).toBeUndefined();
});

test("JSON.stringify omits root function and symbol values", () => {
  expect(JSON.stringify(() => {})).toBeUndefined();
  expect(JSON.stringify(Symbol("root"))).toBeUndefined();
});

test("JSON.stringify replacer can transform otherwise omitted root values", () => {
  const replaceRoot = (replacement) => (key, value) =>
    key === "" ? replacement : value;

  expect(JSON.stringify(undefined, replaceRoot(1))).toBe("1");
  expect(JSON.stringify(() => {}, replaceRoot("fn"))).toBe('"fn"');
  expect(JSON.stringify(Symbol("root"), replaceRoot(true))).toBe("true");
  expect(JSON.stringify(1, replaceRoot(() => {}))).toBeUndefined();
  expect(JSON.stringify(1, replaceRoot(Symbol("root")))).toBeUndefined();
});

test("JSON.stringify omits root values after toJSON", () => {
  expect(JSON.stringify({ toJSON() { return undefined; } })).toBeUndefined();
  expect(JSON.stringify({ toJSON() { return () => {}; } })).toBeUndefined();
  expect(JSON.stringify({ toJSON() { return Symbol("root"); } })).toBeUndefined();
});

test("JSON.stringify objects", () => {
  const obj = { name: "Alice", age: 30, active: true };
  const json = JSON.stringify(obj);
  expect(json).toBe('{"name":"Alice","age":30,"active":true}');
  expect(json).toContain('"name":"Alice"');
  expect(json).toContain('"age":30');
  expect(json).toContain('"active":true');
});

test("JSON.stringify skips undefined values", () => {
  const obj = {
    name: "Alice",
    age: 30,
    active: true,
    undefinedValue: undefined,
  };
  const json = JSON.stringify(obj);
  expect(json).toBe('{"name":"Alice","age":30,"active":true}');
  expect(json).not.toContain('"undefinedValue":undefined');
});

test("JSON.stringify arrays", () => {
  const arr = [1, "hello", true, null];
  const json = JSON.stringify(arr);
  expect(json).toBe('[1,"hello",true,null]');
});

test("JSON.stringify undefined in array becomes null", () => {
  expect(JSON.stringify([1, undefined, 3])).toBe("[1,null,3]");
});

test("JSON.stringify nested objects", () => {
  const obj = { a: { b: { c: 1 } } };
  expect(JSON.stringify(obj)).toBe('{"a":{"b":{"c":1}}}');
});

test("JSON.stringify empty structures", () => {
  expect(JSON.stringify({})).toBe("{}");
  expect(JSON.stringify([])).toBe("[]");
});

test("JSON.stringify NaN and Infinity become null", () => {
  expect(JSON.stringify(NaN)).toBe("null");
  expect(JSON.stringify(Infinity)).toBe("null");
  expect(JSON.stringify(-Infinity)).toBe("null");
});

test("JSON.stringify preserves round-trip precision for large fractional floating-point numbers", () => {
  const value = 1775026448797.2498;

  expect(JSON.parse(JSON.stringify(value))).toBe(value);
});

test("JSON.stringify strings with special characters", () => {
  expect(JSON.stringify("hello\nworld")).toBe('"hello\\nworld"');
  expect(JSON.stringify("tab\there")).toBe('"tab\\there"');
});

test("JSON.stringify with space number", () => {
  const obj = { a: 1 };
  const result = JSON.stringify(obj, null, 2);
  expect(result).toContain("\n");
  expect(result).toContain("  ");
  expect(result).toContain('"a"');
});

test("JSON.stringify with space string", () => {
  const obj = { a: 1 };
  const result = JSON.stringify(obj, null, "\t");
  expect(result).toContain("\n");
  expect(result).toContain("\t");
});

test("JSON.stringify pretty-prints nested objects with per-level indentation", () => {
  const makeNested = (depth) =>
    Array.from({ length: depth }).reduce((acc) => ({ child: acc }), { leaf: true });
  const expected = [
    "{",
    '  "child": {',
    '    "child": {',
    '      "child": {',
    '        "child": {',
    '          "leaf": true',
    "        }",
    "      }",
    "    }",
    "  }",
    "}",
  ].join("\n");
  expect(JSON.stringify(makeNested(4), null, 2)).toBe(expected);
});

test("JSON.stringify pretty-prints nested arrays with per-level indentation", () => {
  const nestArr = (depth) =>
    Array.from({ length: depth }).reduce((acc) => [acc], [1]);
  const expected = [
    "[",
    "  [",
    "    [",
    "      [",
    "        1",
    "      ]",
    "    ]",
    "  ]",
    "]",
  ].join("\n");
  expect(JSON.stringify(nestArr(3), null, 2)).toBe(expected);
});

test("JSON.stringify repeats a multi-character gap once per nesting level", () => {
  const expected = ['{', 'ab"a": {', 'abab"b": 1', 'ab}', '}'].join("\n");
  expect(JSON.stringify({ a: { b: 1 } }, null, "ab")).toBe(expected);
});

test("JSON.stringify indents each deeper nesting level by exactly one more gap", () => {
  const makeNested = (depth) =>
    Array.from({ length: depth }).reduce((acc) => ({ child: acc }), { leaf: true });
  const out = JSON.stringify(makeNested(30), null, 2);
  // Each keyed line starts with its indentation followed by a quoted key, so the
  // index of the first quote equals the number of leading spaces for that line.
  const indentOf = (line) => line.indexOf('"');
  const keyedLines = out
    .split("\n")
    .filter((line) => line.includes('"child"') || line.includes('"leaf"'));
  // 30 wrappers plus the innermost leaf object => 31 keyed lines, each one gap deeper.
  expect(keyedLines.length).toBe(31);
  keyedLines.forEach((line, i) => {
    expect(indentOf(line)).toBe((i + 1) * 2);
  });
});

test("JSON.stringify serializes deeply nested objects without a gap", () => {
  const makeNested = (depth) =>
    Array.from({ length: depth }).reduce((acc) => ({ child: acc }), { leaf: true });
  expect(JSON.stringify(makeNested(4))).toBe(
    '{"child":{"child":{"child":{"child":{"leaf":true}}}}}',
  );
});

test("JSON.stringify serializes deeply nested arrays without a gap", () => {
  const nestArr = (depth) =>
    Array.from({ length: depth }).reduce((acc) => [acc], [1]);
  expect(JSON.stringify(nestArr(3))).toBe("[[[[1]]]]");
});

test("JSON.stringify pretty-prints interleaved objects and arrays", () => {
  const value = { a: [{ b: [1] }] };
  const expected = [
    "{",
    '  "a": [',
    "    {",
    '      "b": [',
    "        1",
    "      ]",
    "    }",
    "  ]",
    "}",
  ].join("\n");
  expect(JSON.stringify(value, null, 2)).toBe(expected);
});

test("JSON.stringify with replacer function", () => {
  const obj = { a: 1, b: "hello", c: true };
  const result = JSON.stringify(obj, (key, value) => {
    if (typeof value === "number") {
      return value * 2;
    }
    return value;
  });
  const parsed = JSON.parse(result);
  expect(parsed.a).toBe(2);
  expect(parsed.b).toBe("hello");
});

test("JSON.stringify replacer function can exclude properties", () => {
  const obj = { a: 1, b: 2, c: 3 };
  const result = JSON.stringify(obj, (key, value) => {
    if (key === "b") {
      return undefined;
    }
    return value;
  });
  const parsed = JSON.parse(result);
  expect(parsed.a).toBe(1);
  expect(parsed.c).toBe(3);
  expect(parsed.b).toBeUndefined();
});

test("JSON.stringify replacer function omits function and symbol object properties", () => {
  expect(JSON.stringify({ a: 1, b: 2 }, (key, value) =>
    key === "a" ? () => {} : value)).toBe('{"b":2}');
  expect(JSON.stringify({ a: 1, b: 2 }, (key, value) =>
    key === "a" ? Symbol("a") : value)).toBe('{"b":2}');
});

test("JSON.stringify replacer function nullifies function and symbol array elements", () => {
  expect(JSON.stringify([1, 2], (key, value) =>
    key === "0" ? () => {} : value)).toBe("[null,2]");
  expect(JSON.stringify([1, 2], (key, value) =>
    key === "0" ? Symbol("a") : value)).toBe("[null,2]");
});

test("JSON.stringify with array replacer", () => {
  const obj = { a: 1, b: 2, c: 3 };
  const result = JSON.stringify(obj, ["a", "c"]);
  const parsed = JSON.parse(result);
  expect(parsed.a).toBe(1);
  expect(parsed.c).toBe(3);
  expect(parsed.b).toBeUndefined();
});

test("JSON.stringify space is capped at 10", () => {
  const obj = { a: 1 };
  const result = JSON.stringify(obj, null, 20);
  const lines = result.split("\n");
  expect(lines.length).toBe(3);
});

test("JSON.stringify throws on circular references", () => {
  const obj = {};
  obj.self = obj;

  const arr = [];
  arr.push(arr);

  const nested = { child: {} };
  nested.child.parent = nested;

  expect(() => JSON.stringify(obj)).toThrow(TypeError);
  expect(() => JSON.stringify(arr)).toThrow(TypeError);
  expect(() => JSON.stringify(nested)).toThrow(TypeError);
});

test("JSON.stringify calls toJSON during serialization", () => {
  let rootKey = "unset";
  let nestedKey = "unset";

  const payload = {
    value: 5,
    nested: {
      toJSON(key) {
        nestedKey = key;
        return { ok: true };
      },
    },
    toJSON(key) {
      rootKey = key;
      return {
        doubled: this.value * 2,
        nested: this.nested,
      };
    },
  };

  const parsed = JSON.parse(JSON.stringify(payload));
  expect(rootKey).toBe("");
  expect(nestedKey).toBe("nested");
  expect(parsed.doubled).toBe(10);
  expect(parsed.nested.ok).toBeTruthy();
});

test("JSON.stringify propagates toJSON errors", () => {
  const value = {
    toJSON() {
      throw new RangeError("bad toJSON");
    },
  };

  expect(() => JSON.stringify(value)).toThrow(RangeError);
});

test("JSON.stringify propagates replacer errors", () => {
  expect(() =>
    JSON.stringify({ a: 1 }, () => {
      throw new SyntaxError("bad replacer");
    }),
  ).toThrow(SyntaxError);
});

test("JSON.stringify ignores invalid replacer types", () => {
  const obj = { a: 1, b: 2 };
  const expected = '{"a":1,"b":2}';

  expect(JSON.stringify(obj, 123)).toBe(expected);
  expect(JSON.stringify(obj, "ignored")).toBe(expected);
  expect(JSON.stringify(obj, { only: ["a"] })).toBe(expected);
});

test("JSON.stringify omits function values in objects and serializes them as null in arrays", () => {
  const obj = { keep: 1, fn: () => 1 };
  const arr = [1, () => 1, 3];

  expect(JSON.stringify(obj)).toBe('{"keep":1}');
  expect(JSON.stringify(arr)).toBe("[1,null,3]");
});

test("JSON.stringify omits symbol values in objects and serializes them as null in arrays", () => {
  const sym = Symbol("x");

  expect(JSON.stringify({ keep: 1, sym })).toBe('{"keep":1}');
  expect(JSON.stringify([1, sym, 3])).toBe("[1,null,3]");
});

test("JSON.stringify handles deeply nested objects", () => {
  const makeNested = (depth) =>
    depth === 0 ? { leaf: true } : { child: makeNested(depth - 1) };
  const countDepth = (node) => (node.leaf ? 0 : 1 + countDepth(node.child));

  const parsed = JSON.parse(JSON.stringify(makeNested(25)));
  expect(countDepth(parsed)).toBe(25);
  expect(parsed.child.child.child.child.child).toBeTruthy();
});

test("JSON.stringify ignores invalid space parameters", () => {
  const obj = { a: 1 };

  expect(JSON.stringify(obj, null, {})).toBe('{"a":1}');
  expect(JSON.stringify(obj, null, true)).toBe('{"a":1}');
});

test("JSON.stringify treats non-positive space as no indentation", () => {
  const obj = { a: 1 };

  expect(JSON.stringify(obj, null, 0)).toBe('{"a":1}');
  expect(JSON.stringify(obj, null, -4)).toBe('{"a":1}');
});

test("JSON.stringify space as boxed Number indents like the primitive", () => {
  const obj = { a: 1, b: [1, 2] };

  expect(JSON.stringify(obj, null, new Number(2))).toBe(
    JSON.stringify(obj, null, 2),
  );
});

test("JSON.stringify space as boxed Number truncates fractional values", () => {
  const obj = { a: 1, b: [1, 2] };

  expect(JSON.stringify(obj, null, new Number(5.11111))).toBe(
    JSON.stringify(obj, null, 5),
  );
});

test("JSON.stringify space as boxed Number uses an overridden valueOf", () => {
  const obj = { a: 1 };
  const space = new Number(1);
  space.valueOf = () => 3;

  expect(JSON.stringify(obj, null, space)).toBe(JSON.stringify(obj, null, 3));
});

test("JSON.stringify space as boxed Number propagates valueOf exceptions", () => {
  const space = new Number(4);
  space.valueOf = () => {
    throw new TypeError("abrupt valueOf");
  };

  expect(() => JSON.stringify({ a: 1 }, null, space)).toThrow(TypeError);
});

test("JSON.stringify space as boxed String indents like the primitive", () => {
  const obj = { a: 1, b: [1, 2] };

  expect(JSON.stringify(obj, null, new String("xx"))).toBe(
    JSON.stringify(obj, null, "xx"),
  );
});

test("JSON.stringify space as boxed String uses an overridden toString without calling valueOf", () => {
  const obj = { a: 1 };
  const space = new String("xx");
  space.toString = () => "--";
  space.valueOf = () => {
    throw new TypeError("valueOf must not be called");
  };

  expect(JSON.stringify(obj, null, space)).toBe(
    JSON.stringify(obj, null, "--"),
  );
});

test("JSON.stringify space as boxed String propagates toString exceptions", () => {
  const space = new String("xx");
  space.toString = () => {
    throw new TypeError("abrupt toString");
  };

  expect(() => JSON.stringify({ a: 1 }, null, space)).toThrow(TypeError);
});

test("JSON.stringify truncates a fractional primitive space", () => {
  const obj = { a: 1, b: [1, 2] };

  expect(JSON.stringify(obj, null, 5.99999)).toBe(JSON.stringify(obj, null, 5));
});

test("JSON.stringify treats NaN space as no indentation", () => {
  expect(JSON.stringify({ a: 1 }, null, NaN)).toBe('{"a":1}');
});

test("JSON.stringify clamps Infinity space to 10 spaces", () => {
  const obj = { a: 1, b: [1, 2] };

  expect(JSON.stringify(obj, null, Infinity)).toBe(
    JSON.stringify(obj, null, 10),
  );
});

test("JSON.stringify treats -Infinity space as no indentation", () => {
  expect(JSON.stringify({ a: 1 }, null, -Infinity)).toBe('{"a":1}');
});

test("JSON.stringify clamps huge finite space to 10 spaces", () => {
  const obj = { a: 1, b: [1, 2] };

  expect(JSON.stringify(obj, null, 2147483648)).toBe(
    JSON.stringify(obj, null, 10),
  );
  expect(JSON.stringify(obj, null, 1e15)).toBe(JSON.stringify(obj, null, 10));
});

test("JSON.stringify clamps boxed Number Infinity space to 10 spaces", () => {
  const obj = { a: 1, b: [1, 2] };

  expect(JSON.stringify(obj, null, new Number(Infinity))).toBe(
    JSON.stringify(obj, null, 10),
  );
});

test("JSON.stringify serializes sparse array holes as null", () => {
  expect(JSON.stringify([1, , 3])).toBe("[1,null,3]");
  expect(JSON.stringify(new Array(3))).toBe("[null,null,null]");
});

test("JSON.stringify arrays read elements through property access", () => {
  const arr = new Array(1);
  Object.defineProperty(arr, "0", {
    get() {
      return 7;
    },
  });

  expect(JSON.stringify(arr)).toBe("[7]");
});

test("JSON.stringify arrays propagate element accessor errors", () => {
  const arr = new Array(1);
  Object.defineProperty(arr, "0", {
    get() {
      throw new RangeError("abrupt element get");
    },
  });

  expect(() => JSON.stringify(arr)).toThrow(RangeError);
});

test("JSON.stringify calls replacer with the holder as this", () => {
  const holders = [];
  const helper = {
    replacer(key, value) {
      if (key === "a" || key === "0") {
        holders.push(this);
      }
      return value;
    },
  };
  const obj = { a: 1 };
  const arr = [2];

  JSON.stringify(obj, helper.replacer);
  JSON.stringify(arr, helper.replacer);

  expect(holders[0]).toBe(obj);
  expect(holders[1]).toBe(arr);
});

test("JSON.stringify array replacer coerces keys and preserves replacer order", () => {
  const obj = { a: 1, b: 2, 1: "one", c: 3 };

  expect(JSON.stringify(obj, ["b", 1, "a", "b"])).toBe(
    '{"b":2,"1":"one","a":1}',
  );
});

test("JSON.stringify preserves enumerable property order", () => {
  const obj = {};
  obj.beta = 2;
  obj.alpha = 1;
  obj.gamma = 3;

  expect(JSON.stringify(obj)).toBe('{"beta":2,"alpha":1,"gamma":3}');
});

test("JSON.stringify array replacer ignores boolean, null, undefined, and plain object elements", () => {
  expect(JSON.stringify({ true: 1, a: 2 }, [true])).toBe("{}");
  expect(JSON.stringify({ false: 1, a: 2 }, [false])).toBe("{}");
  expect(JSON.stringify({ null: 1, a: 2 }, [null])).toBe("{}");
  expect(JSON.stringify({ undefined: 1, a: 2 }, [undefined])).toBe("{}");
  expect(JSON.stringify({ "[object Object]": 1, a: 2 }, [{}])).toBe("{}");
});

test("JSON.stringify array replacer does not call toString on plain object elements", () => {
  const element = {
    toString() {
      return "toString";
    },
  };
  expect(JSON.stringify({ toString: 1, a: 2 }, [element])).toBe("{}");
});

test("JSON.stringify array replacer filters nested objects at every depth", () => {
  expect(JSON.stringify({ a: { a: 1, b: 2 }, b: 3 }, ["a"])).toBe(
    '{"a":{"a":1}}',
  );
  expect(JSON.stringify({ a: { b: { a: 1, c: 2 }, a: 3 }, c: 4 }, ["a", "b"])).toBe(
    '{"a":{"a":3,"b":{"a":1}}}',
  );
});

test("JSON.stringify array replacer filters objects inside arrays", () => {
  expect(JSON.stringify([{ a: 1, b: 2 }, { b: 3 }], ["a"])).toBe(
    '[{"a":1},{}]',
  );
});

test("JSON.stringify array replacer determines key order at every depth", () => {
  expect(JSON.stringify({ a: { b: 2, c: 3 } }, ["c", "b", "a"])).toBe(
    '{"a":{"c":3,"b":2}}',
  );
});

test("JSON.stringify array replacer de-duplicates keys and reads each property once", () => {
  let getCalls = 0;
  const value = {
    get key() {
      getCalls += 1;
      return true;
    },
  };
  expect(JSON.stringify(value, ["key", "key"])).toBe('{"key":true}');
  expect(getCalls).toBe(1);
});

test("JSON.stringify array replacer reads elements through property access", () => {
  const replacer = new Array(1);
  Object.defineProperty(replacer, "0", {
    get() {
      return "a";
    },
  });

  expect(JSON.stringify({ a: 1, b: 2 }, replacer)).toBe('{"a":1}');
});

test("JSON.stringify array replacer propagates element accessor errors", () => {
  const replacer = new Array(1);
  Object.defineProperty(replacer, "0", {
    get() {
      throw new RangeError("abrupt replacer get");
    },
  });

  expect(() => JSON.stringify({ a: 1 }, replacer)).toThrow(RangeError);
});

test("JSON.stringify array replacer converts number elements with number-to-string semantics", () => {
  const obj = { 0: 0, 1: 1, "-4": 2, 0.3: 3, "-Infinity": 4, NaN: 5 };
  expect(JSON.stringify(obj, [-0, 1, -4, 0.3, -Infinity, NaN])).toBe(
    '{"0":0,"1":1,"-4":2,"0.3":3,"-Infinity":4,"NaN":5}',
  );
});

test("JSON.stringify array replacer converts String and Number wrapper elements via toString", () => {
  const num = new Number(10);
  num.toString = () => "toString";
  num.valueOf = () => {
    throw new Error("valueOf should not be called");
  };
  expect(JSON.stringify({ 10: 1, toString: 2 }, [num])).toBe('{"toString":2}');

  const str = new String("str");
  str.toString = () => "toString";
  str.valueOf = () => {
    throw new Error("valueOf should not be called");
  };
  expect(JSON.stringify({ str: 1, toString: 2 }, [str])).toBe('{"toString":2}');
});

test("JSON.stringify array replacer treats keys case-sensitively", () => {
  expect(JSON.stringify({ a: 1, A: 2 }, ["A"])).toBe('{"A":2}');
  expect(JSON.stringify({ a: 1, A: 2 }, ["a", "A"])).toBe('{"a":1,"A":2}');
});

test("JSON.stringify empty array replacer serializes objects as empty", () => {
  expect(JSON.stringify({ a: 1 }, [])).toBe("{}");
  expect(JSON.stringify({ a: { b: 1 } }, [])).toBe("{}");
});

test("JSON.stringify array replacer composes with indentation", () => {
  expect(JSON.stringify({ a: { a: 1, b: 2 }, b: 3 }, ["a"], 2)).toBe(
    '{\n  "a": {\n    "a": 1\n  }\n}',
  );
});

test("JSON.stringify value as boxed Number uses an overridden valueOf", () => {
  const boxed = new Number(1);
  boxed.valueOf = () => 4;

  expect(JSON.stringify({ a: boxed })).toBe('{"a":4}');
  expect(JSON.stringify(boxed)).toBe("4");
});

test("JSON.stringify root boxed Number calls valueOf once", () => {
  let calls = 0;
  const boxed = new Number(1);
  boxed.valueOf = () => {
    calls += 1;
    return 4;
  };

  expect(JSON.stringify(boxed)).toBe("4");
  expect(calls).toBe(1);
});

test("JSON.stringify value as boxed Number uses an overridden valueOf with a replacer function", () => {
  const boxed = new Number(1);
  boxed.valueOf = () => 4;

  expect(JSON.stringify({ a: boxed }, (key, value) => value)).toBe('{"a":4}');
});

test("JSON.stringify value as boxed Number uses an overridden valueOf with an array replacer", () => {
  const boxed = new Number(1);
  boxed.valueOf = () => 4;

  expect(JSON.stringify({ a: boxed, b: 2 }, ["a"])).toBe('{"a":4}');
});

test("JSON.stringify value as boxed String uses an overridden toString without calling valueOf", () => {
  const boxed = new String("ab");
  boxed.toString = () => "zz";
  boxed.valueOf = () => {
    throw new TypeError("valueOf must not be called");
  };

  expect(JSON.stringify([boxed])).toBe('["zz"]');
});

test("JSON.stringify value as boxed Boolean ignores valueOf and toString", () => {
  const boxed = new Boolean(false);
  boxed.valueOf = () => true;
  boxed.toString = () => "true";

  expect(JSON.stringify([boxed])).toBe("[false]");
});

test("JSON.stringify throws on BigInt and boxed BigInt values", () => {
  expect(() => JSON.stringify(1n)).toThrow(TypeError);
  expect(() => JSON.stringify(Object(1n))).toThrow(TypeError);
  expect(() => JSON.stringify({ a: Object(1n) })).toThrow(TypeError);
  expect(() => JSON.stringify([Object(1n)])).toThrow(TypeError);
  expect(() => JSON.stringify({ a: 1 }, () => Object(1n))).toThrow(TypeError);
});

test("JSON.stringify value as boxed Number propagates valueOf exceptions", () => {
  const boxed = new Number(1);
  boxed.valueOf = () => {
    throw new RangeError("abrupt valueOf");
  };

  expect(() => JSON.stringify({ a: boxed })).toThrow(RangeError);
});

test("JSON.stringify value as boxed String propagates toString exceptions", () => {
  const boxed = new String("ab");
  boxed.toString = () => {
    throw new RangeError("abrupt toString");
  };

  expect(() => JSON.stringify([boxed])).toThrow(RangeError);
});

test("JSON.stringify keeps a short multibyte string space intact", () => {
  const obj = { a: 1 };

  expect(JSON.stringify(obj, null, "ααααααα")).toBe(
    '{\n' + "ααααααα" + '"a": 1\n}',
  );
});

test("JSON.stringify truncates a long multibyte string space to 10 characters", () => {
  const obj = { a: 1 };

  expect(JSON.stringify(obj, null, "あ".repeat(12))).toBe(
    JSON.stringify(obj, null, "あ".repeat(10)),
  );
});
