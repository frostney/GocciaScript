/*---
description: JSON.rawJSON creates frozen raw JSON value objects
features: [JSON.rawJSON, JSON.isRawJSON]
---*/

test("JSON.rawJSON returns a frozen object with null prototype", () => {
  const raw = JSON.rawJSON("123");

  expect(typeof raw).toBe("object");
  expect(Object.isFrozen(raw)).toBe(true);
  expect(Object.getPrototypeOf(raw)).toBeNull();
});

test("JSON.rawJSON sets the rawJSON property to the input string", () => {
  expect(JSON.rawJSON("123").rawJSON).toBe("123");
  expect(JSON.rawJSON('"hello"').rawJSON).toBe('"hello"');
  expect(JSON.rawJSON("true").rawJSON).toBe("true");
  expect(JSON.rawJSON("false").rawJSON).toBe("false");
  expect(JSON.rawJSON("null").rawJSON).toBe("null");
});

test("JSON.rawJSON accepts number values", () => {
  expect(JSON.rawJSON("0").rawJSON).toBe("0");
  expect(JSON.rawJSON("-1").rawJSON).toBe("-1");
  expect(JSON.rawJSON("3.14").rawJSON).toBe("3.14");
  expect(JSON.rawJSON("1e100").rawJSON).toBe("1e100");
  expect(JSON.rawJSON("9007199254740993").rawJSON).toBe("9007199254740993");
});

test("JSON.rawJSON accepts string values", () => {
  expect(JSON.rawJSON('"abc"').rawJSON).toBe('"abc"');
  expect(JSON.rawJSON('""').rawJSON).toBe('""');
  expect(JSON.rawJSON('"hello\\nworld"').rawJSON).toBe('"hello\\nworld"');
});

test("JSON.rawJSON accepts boolean and null values", () => {
  expect(JSON.rawJSON("true").rawJSON).toBe("true");
  expect(JSON.rawJSON("false").rawJSON).toBe("false");
  expect(JSON.rawJSON("null").rawJSON).toBe("null");
});

test("JSON.rawJSON throws SyntaxError for empty string", () => {
  expect(() => JSON.rawJSON("")).toThrow(SyntaxError);
});

test("JSON.rawJSON throws SyntaxError for leading whitespace", () => {
  expect(() => JSON.rawJSON(" 123")).toThrow(SyntaxError);
  expect(() => JSON.rawJSON("\t123")).toThrow(SyntaxError);
  expect(() => JSON.rawJSON("\n123")).toThrow(SyntaxError);
  expect(() => JSON.rawJSON("\r123")).toThrow(SyntaxError);
});

test("JSON.rawJSON throws SyntaxError for trailing whitespace", () => {
  expect(() => JSON.rawJSON("123 ")).toThrow(SyntaxError);
  expect(() => JSON.rawJSON("123\t")).toThrow(SyntaxError);
  expect(() => JSON.rawJSON("123\n")).toThrow(SyntaxError);
  expect(() => JSON.rawJSON("123\r")).toThrow(SyntaxError);
});

test("JSON.rawJSON throws SyntaxError for invalid JSON", () => {
  expect(() => JSON.rawJSON("abc")).toThrow(SyntaxError);
  expect(() => JSON.rawJSON("undefined")).toThrow(SyntaxError);
  expect(() => JSON.rawJSON("{invalid}")).toThrow(SyntaxError);
});

test("JSON.rawJSON throws SyntaxError for object values", () => {
  expect(() => JSON.rawJSON("{}")).toThrow(SyntaxError);
  expect(() => JSON.rawJSON('{"a":1}')).toThrow(SyntaxError);
});

test("JSON.rawJSON throws SyntaxError for array values", () => {
  expect(() => JSON.rawJSON("[]")).toThrow(SyntaxError);
  expect(() => JSON.rawJSON("[1,2,3]")).toThrow(SyntaxError);
});

test("JSON.stringify emits raw JSON text verbatim", () => {
  const raw = JSON.rawJSON("9007199254740993");
  expect(JSON.stringify(raw)).toBe("9007199254740993");
});

test("JSON.stringify emits raw JSON inside objects", () => {
  const obj = { bigNum: JSON.rawJSON("9007199254740993") };
  expect(JSON.stringify(obj)).toBe('{"bigNum":9007199254740993}');
});

test("JSON.stringify emits raw JSON inside arrays", () => {
  const arr = [1, JSON.rawJSON("9007199254740993"), 3];
  expect(JSON.stringify(arr)).toBe("[1,9007199254740993,3]");
});

test("JSON.stringify emits raw JSON string values without double-quoting", () => {
  const obj = { greeting: JSON.rawJSON('"hello"') };
  expect(JSON.stringify(obj)).toBe('{"greeting":"hello"}');
});

test("JSON.stringify emits raw JSON boolean and null values", () => {
  const obj = {
    a: JSON.rawJSON("true"),
    b: JSON.rawJSON("false"),
    c: JSON.rawJSON("null"),
  };
  expect(JSON.stringify(obj)).toBe('{"a":true,"b":false,"c":null}');
});

test("JSON.stringify with replacer function returning raw JSON", () => {
  const result = JSON.stringify({ n: 42 }, (key, value) => {
    if (key === "n") {
      return JSON.rawJSON("9007199254740993");
    }
    return value;
  });
  expect(result).toBe('{"n":9007199254740993}');
});

test("JSON.stringify with indentation and raw JSON", () => {
  const obj = { a: JSON.rawJSON("123"), b: "text" };
  const result = JSON.stringify(obj, null, 2);
  expect(result).toContain("123");
  expect(result).toContain('"text"');
});

test("JSON.rawJSON objects are frozen and cannot be modified", () => {
  const raw = JSON.rawJSON("123");
  expect(() => { raw.rawJSON = "456"; }).toThrow(TypeError);
  expect(() => { raw.extra = true; }).toThrow(TypeError);
  expect(raw.rawJSON).toBe("123");
});

test("JSON.rawJSON preserves exact numeric representation", () => {
  const values = [
    "1e100",
    "-0",
    "1.0000000000000001",
    "9007199254740993",
    "0.1",
  ];
  values.forEach((v) => {
    const obj = { v: JSON.rawJSON(v) };
    const result = JSON.stringify(obj);
    expect(result).toBe('{"v":' + v + "}");
  });
});
