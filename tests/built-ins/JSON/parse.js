/*---
description: JSON.parse converts JSON strings to JavaScript values
features: [JSON.parse]
---*/

test("JSON.parse basic values", () => {
  expect(JSON.parse("42")).toBe(42);
  expect(JSON.parse('"hello"')).toBe("hello");
  expect(JSON.parse("true")).toBeTruthy();
  expect(JSON.parse("false")).toBeFalsy();
  expect(JSON.parse("null")).toBeNull();
});

test("JSON.parse objects", () => {
  const json = '{"name":"Alice","age":30,"active":true}';
  const obj = JSON.parse(json);
  expect(obj.name).toBe("Alice");
  expect(obj.age).toBe(30);
  expect(obj.active).toBeTruthy();
});

test("JSON.parse arrays", () => {
  const json = '[1,"hello",true,null]';
  const arr = JSON.parse(json);
  expect(arr.length).toBe(4);
  expect(arr[0]).toBe(1);
  expect(arr[1]).toBe("hello");
  expect(arr[2]).toBeTruthy();
  expect(arr[3]).toBeNull();
});

test("JSON.parse nested objects", () => {
  const obj = JSON.parse('{"a":{"b":1}}');
  expect(obj.a.b).toBe(1);
});

test("JSON.parse empty structures", () => {
  const obj = JSON.parse("{}");
  expect(Object.keys(obj).length).toBe(0);
  const arr = JSON.parse("[]");
  expect(arr.length).toBe(0);
});

test("JSON.parse with escape sequences", () => {
  expect(JSON.parse('"hello\\nworld"')).toBe("hello\nworld");
  expect(JSON.parse('"tab\\there"')).toBe("tab\there");
});

test("JSON.parse unicode escape sequences", () => {
  expect(JSON.parse('"\\u0041"')).toBe("A");
  expect(JSON.parse('"\\u00e9"')).toBe("\u00e9");
  expect(JSON.parse('"caf\\u00e9"')).toBe("caf\u00e9");
  expect(JSON.parse('"\\u4e16\\u754c"')).toBe("\u4e16\u754c");
});

test("JSON.parse surrogate pairs", () => {
  const result = JSON.parse('"\\uD83D\\uDE00"');
  expect(result.length).toBeGreaterThan(0);
  expect(result).toBe("\uD83D\uDE00");
});

test("JSON.parse high surrogate without low surrogate", () => {
  const result = JSON.parse('"\\uD83Dabc"');
  expect(result.length).toBeGreaterThan(0);
});

test("JSON.parse throws on invalid JSON", () => {
  expect(() => JSON.parse("undefined")).toThrow(SyntaxError);
  expect(() => JSON.parse("{invalid}")).toThrow(SyntaxError);
  expect(() => JSON.parse("")).toThrow(SyntaxError);
});

test("JSON.parse throws on unterminated object", () => {
  expect(() => JSON.parse('{"a": 1')).toThrow(SyntaxError);
  expect(() => JSON.parse('{"a": 1, "b": 2')).toThrow(SyntaxError);
});

test("JSON.parse throws on unterminated array", () => {
  expect(() => JSON.parse("[1, 2")).toThrow(SyntaxError);
  expect(() => JSON.parse("[1, [2, 3")).toThrow(SyntaxError);
});

test("JSON.parse throws on unterminated string", () => {
  expect(() => JSON.parse('"hello')).toThrow(SyntaxError);
  expect(() => JSON.parse('{"key": "value')).toThrow(SyntaxError);
});

test("JSON.parse throws on trailing comma", () => {
  expect(() => JSON.parse("[1, 2,]")).toThrow(SyntaxError);
  expect(() => JSON.parse('{"a": 1,}')).toThrow(SyntaxError);
});

test("JSON.parse throws on invalid number formats", () => {
  expect(() => JSON.parse("+1")).toThrow(SyntaxError);
  expect(() => JSON.parse(".5")).toThrow(SyntaxError);
  expect(() => JSON.parse("01")).toThrow(SyntaxError);
});

test("JSON.parse throws on single quotes", () => {
  expect(() => JSON.parse("'hello'")).toThrow(SyntaxError);
  expect(() => JSON.parse("{'a': 1}")).toThrow(SyntaxError);
});

test("JSON.parse remains strict about JSON5-only syntax", () => {
  expect(() => JSON.parse("{unquoted: 1}")).toThrow(SyntaxError);
  expect(() => JSON.parse("{\"a\": 1,}")).toThrow(SyntaxError);
  expect(() => JSON.parse("// comment\n{\"a\":1}")).toThrow(SyntaxError);
  expect(() => JSON.parse("0x10")).toThrow(SyntaxError);
  expect(() => JSON.parse("Infinity")).toThrow(SyntaxError);
});

test("JSON.parse with reviver transforms values", () => {
  const result = JSON.parse('{"a":1,"b":2}', (key, value) => {
    if (typeof value === "number") {
      return value * 2;
    }
    return value;
  });
  expect(result.a).toBe(2);
  expect(result.b).toBe(4);
});

test("JSON.parse reviver can remove properties", () => {
  const result = JSON.parse('{"a":1,"b":2,"c":3}', (key, value) => {
    if (key === "b") {
      return undefined;
    }
    return value;
  });
  expect(result.a).toBe(1);
  expect(result.c).toBe(3);
  expect(result.b).toBeUndefined();
});

test("JSON.parse reviver receives key and value", () => {
  const keys = [];
  JSON.parse('{"x":10}', (key, value) => {
    keys.push(key);
    return value;
  });
  expect(keys.length).toBe(2);
  expect(keys[0]).toBe("x");
  expect(keys[1]).toBe("");
});

test("JSON.parse reviver works with arrays", () => {
  const result = JSON.parse("[1,2,3]", (key, value) => {
    if (typeof value === "number") {
      return value + 10;
    }
    return value;
  });
  expect(result[0]).toBe(11);
  expect(result[1]).toBe(12);
  expect(result[2]).toBe(13);
});

test("JSON.parse reviver returning undefined deletes array elements", () => {
  const result = JSON.parse("[1,2,3]", (key, value) => {
    if (key === "1") {
      return undefined;
    }
    return value;
  });

  expect(result.length).toBe(3);
  expect(0 in result).toBe(true);
  expect(1 in result).toBe(false);
  expect(2 in result).toBe(true);
  expect(result[1]).toBe(undefined);
});

test("JSON.parse reviver is called with the holder as this", () => {
  const holders = [];
  const helper = {
    reviver(key, value) {
      if (key === "a" || key === "0") {
        holders.push(this);
      }
      return value;
    },
  };
  const obj = JSON.parse('{"a":1}', helper.reviver);
  const arr = JSON.parse("[1]", helper.reviver);

  expect(holders[0]).toBe(obj);
  expect(holders[1]).toBe(arr);
});

test("JSON.parse reviver visits nested properties before parents", () => {
  const keys = [];

  JSON.parse('{"outer":{"inner":1},"list":[2]}', (key, value) => {
    keys.push(key);
    return value;
  });

  expect(keys).toEqual(["inner", "outer", "0", "list", ""]);
});
