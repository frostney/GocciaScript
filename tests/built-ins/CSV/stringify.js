/*---
description: CSV.stringify converts JavaScript arrays/objects to CSV text
features: [CSV.stringify]
---*/

const hasCSV = typeof CSV !== "undefined";

describe.runIf(hasCSV)("CSV.stringify", () => {
  test("stringifies array of objects with headers", () => {
    const data = [
      { name: "Alice", age: "30" },
      { name: "Bob", age: "25" },
    ];
    const result = CSV.stringify(data);

    expect(result).toBe("name,age\nAlice,30\nBob,25");
  });

  test("stringifies array of arrays without headers", () => {
    const data = [
      ["Alice", "30"],
      ["Bob", "25"],
    ];
    const result = CSV.stringify(data, { headers: false });

    expect(result).toBe("Alice,30\nBob,25");
  });

  test("escapes fields containing commas", () => {
    const data = [{ v: "hello, world" }];
    const result = CSV.stringify(data);

    expect(result).toBe('v\n"hello, world"');
  });

  test("escapes fields containing quotes", () => {
    const data = [{ v: 'He said "hi"' }];
    const result = CSV.stringify(data);

    expect(result).toBe('v\n"He said ""hi"""');
  });

  test("escapes fields containing newlines", () => {
    const data = [{ v: "line1\nline2" }];
    const result = CSV.stringify(data);

    expect(result).toBe('v\n"line1\nline2"');
  });

  test("escapes fields containing CR", () => {
    const data = [{ v: "a\rb" }];
    const result = CSV.stringify(data);

    expect(result).toBe('v\n"a\rb"');
  });

  test("does not escape fields without special characters", () => {
    const data = [{ v: "plain text" }];
    const result = CSV.stringify(data);

    expect(result).toBe("v\nplain text");
  });

  test("uses custom delimiter", () => {
    const data = [{ a: "1", b: "2" }];
    const result = CSV.stringify(data, { delimiter: ";" });

    expect(result).toBe("a;b\n1;2");
  });

  test("escapes the custom delimiter in values", () => {
    const data = [{ v: "a;b" }];
    const result = CSV.stringify(data, { delimiter: ";" });

    expect(result).toBe('v\n"a;b"');
  });

  test("returns empty string for empty array", () => {
    const result = CSV.stringify([]);

    expect(result).toBe("");
  });

  test("returns empty string for non-array", () => {
    const result = CSV.stringify("not an array");

    expect(result).toBe("");
  });

  test("returns empty string for number", () => {
    const result = CSV.stringify(42);

    expect(result).toBe("");
  });

  test("handles single-row data", () => {
    const data = [{ x: "1" }];
    const result = CSV.stringify(data);

    expect(result).toBe("x\n1");
  });

  test("handles empty string values", () => {
    const data = [{ a: "", b: "x" }];
    const result = CSV.stringify(data);

    expect(result).toBe("a,b\n,x");
  });

  test("round-trip parse then stringify preserves data", () => {
    const csv = "name,city\nAlice,NYC\nBob,LA";
    const parsed = CSV.parse(csv);
    const result = CSV.stringify(parsed);

    expect(result).toBe(csv);
  });

  test("round-trip with quoted fields", () => {
    const csv = 'a,b\n"x,y","He said ""hi"""';
    const parsed = CSV.parse(csv);
    const result = CSV.stringify(parsed);

    expect(result).toBe(csv);
  });
});

describe.runIf(hasCSV)("CSV.stringify with replacer", () => {
  test("replacer transforms values", () => {
    const data = [{ name: "alice", age: "30" }];
    const result = CSV.stringify(data, {}, (key, value) => {
      if (key === "name") return value.toUpperCase();
      return value;
    });

    expect(result).toBe("name,age\nALICE,30");
  });

  test("replacer receives key and value", () => {
    const calls = [];
    const data = [{ a: "1", b: "2" }];
    CSV.stringify(data, {}, (key, value) => {
      calls.push({ key, value });
      return value;
    });

    expect(calls.length).toBe(2);
    expect(calls[0].key).toBe("a");
    expect(calls[0].value).toBe("1");
  });

  test("replacer with array-of-arrays data", () => {
    const data = [["a", "b"]];
    const result = CSV.stringify(data, { headers: false }, (key, value) => {
      return value.toUpperCase();
    });

    expect(result).toBe("A,B");
  });

  test("non-callable third arg is ignored", () => {
    const data = [{ a: "1" }];
    const result = CSV.stringify(data, {}, "not a function");

    expect(result).toBe("a\n1");
  });
});

describe.runIf(hasCSV)("CSV.stringify error handling", () => {
  test("throws when called with no arguments", () => {
    expect(() => CSV.stringify()).toThrow();
  });
});
