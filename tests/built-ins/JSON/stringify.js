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

test("JSON.stringify serializes sparse array holes as null", () => {
  expect(JSON.stringify([1, , 3])).toBe("[1,null,3]");
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
