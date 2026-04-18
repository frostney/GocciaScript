/*---
description: TSV.stringify converts JavaScript arrays/objects to tab-separated text
features: [TSV.stringify]
---*/

const hasTSV = typeof TSV !== "undefined";

describe.runIf(hasTSV)("TSV.stringify", () => {
  test("stringifies array of objects with headers", () => {
    const data = [
      { name: "Alice", age: "30" },
      { name: "Bob", age: "25" },
    ];
    const result = TSV.stringify(data);

    expect(result).toBe("name\tage\nAlice\t30\nBob\t25");
  });

  test("stringifies array of arrays without headers", () => {
    const data = [
      ["Alice", "30"],
      ["Bob", "25"],
    ];
    const result = TSV.stringify(data, { headers: false });

    expect(result).toBe("Alice\t30\nBob\t25");
  });

  test("escapes tab characters in values", () => {
    const data = [{ v: "col1\tcol2" }];
    const result = TSV.stringify(data);

    expect(result).toBe("v\ncol1\\tcol2");
  });

  test("escapes newline characters in values", () => {
    const data = [{ v: "line1\nline2" }];
    const result = TSV.stringify(data);

    expect(result).toBe("v\nline1\\nline2");
  });

  test("escapes CR characters in values", () => {
    const data = [{ v: "a\rb" }];
    const result = TSV.stringify(data);

    expect(result).toBe("v\na\\rb");
  });

  test("escapes backslash characters in values", () => {
    const data = [{ v: "path\\file" }];
    const result = TSV.stringify(data);

    expect(result).toBe("v\npath\\\\file");
  });

  test("escapes multiple special characters in one value", () => {
    const data = [{ v: "a\tb\nc\\d" }];
    const result = TSV.stringify(data);

    expect(result).toBe("v\na\\tb\\nc\\\\d");
  });

  test("does not escape commas or quotes (TSV semantics)", () => {
    const data = [{ v: 'hello, "world"' }];
    const result = TSV.stringify(data);

    expect(result).toBe('v\nhello, "world"');
  });

  test("returns empty string for empty array", () => {
    const result = TSV.stringify([]);

    expect(result).toBe("");
  });

  test("returns empty string for non-array", () => {
    const result = TSV.stringify("not an array");

    expect(result).toBe("");
  });

  test("handles single-row data", () => {
    const data = [{ x: "1" }];
    const result = TSV.stringify(data);

    expect(result).toBe("x\n1");
  });

  test("handles empty string values", () => {
    const data = [{ a: "", b: "x" }];
    const result = TSV.stringify(data);

    expect(result).toBe("a\tb\n\tx");
  });

  test("round-trip parse then stringify preserves data", () => {
    const tsv = "name\tcity\nAlice\tNYC\nBob\tLA";
    const parsed = TSV.parse(tsv);
    const result = TSV.stringify(parsed);

    expect(result).toBe(tsv);
  });

  test("round-trip with backslash escaping", () => {
    const data = [{ v: "has\ttab\nand\nnewline" }];
    const stringified = TSV.stringify(data);
    const parsed = TSV.parse(stringified);

    expect(parsed[0].v).toBe("has\ttab\nand\nnewline");
  });

  test("round-trip with backslash in value", () => {
    const data = [{ v: "C:\\Users\\test" }];
    const stringified = TSV.stringify(data);
    const parsed = TSV.parse(stringified);

    expect(parsed[0].v).toBe("C:\\Users\\test");
  });
});

describe.runIf(hasTSV)("TSV.stringify with replacer", () => {
  test("replacer transforms values", () => {
    const data = [{ name: "alice", age: "30" }];
    const result = TSV.stringify(data, {}, (key, value) => {
      if (key === "name") return value.toUpperCase();
      return value;
    });

    expect(result).toBe("name\tage\nALICE\t30");
  });

  test("replacer with array-of-arrays data", () => {
    const data = [["a", "b"]];
    const result = TSV.stringify(data, { headers: false }, (key, value) => {
      return value.toUpperCase();
    });

    expect(result).toBe("A\tB");
  });
});

describe.runIf(hasTSV)("TSV.stringify error handling", () => {
  test("throws when called with no arguments", () => {
    expect(() => TSV.stringify()).toThrow();
  });
});
