/*---
description: TSV.parse converts tab-separated text to JavaScript arrays/objects
features: [TSV.parse]
---*/

const hasTSV = typeof TSV !== "undefined";

describe.runIf(hasTSV)("TSV.parse", () => {
  test("parses TSV with headers (default)", () => {
    const result = TSV.parse("name\tage\nAlice\t30\nBob\t25");

    expect(result.length).toBe(2);
    expect(result[0].name).toBe("Alice");
    expect(result[0].age).toBe("30");
    expect(result[1].name).toBe("Bob");
    expect(result[1].age).toBe("25");
  });

  test("parses TSV without headers", () => {
    const result = TSV.parse("Alice\t30\nBob\t25", { headers: false });

    expect(result.length).toBe(2);
    expect(result[0][0]).toBe("Alice");
    expect(result[0][1]).toBe("30");
  });

  test("parses single-column TSV", () => {
    const result = TSV.parse("v\n1\n2\n3");

    expect(result.length).toBe(3);
    expect(result[2].v).toBe("3");
  });

  test("parses header-only TSV as empty array", () => {
    const result = TSV.parse("a\tb\tc");

    expect(result.length).toBe(0);
  });
});

describe.runIf(hasTSV)("TSV.parse backslash escaping", () => {
  test("unescapes backslash-t to tab", () => {
    const result = TSV.parse("col1\\tcol2", { headers: false });

    expect(result[0][0]).toBe("col1\tcol2");
  });

  test("unescapes backslash-n to newline", () => {
    const result = TSV.parse("line1\\nline2", { headers: false });

    expect(result[0][0]).toBe("line1\nline2");
  });

  test("unescapes backslash-r to carriage return", () => {
    const result = TSV.parse("a\\rb", { headers: false });

    expect(result[0][0]).toBe("a\rb");
  });

  test("unescapes backslash-backslash to literal backslash", () => {
    const result = TSV.parse("path\\\\file", { headers: false });

    expect(result[0][0]).toBe("path\\file");
  });

  test("preserves unrecognized escape sequences", () => {
    const result = TSV.parse("\\x41", { headers: false });

    expect(result[0][0]).toBe("\\x41");
  });

  test("handles multiple escape sequences in one field", () => {
    const result = TSV.parse("a\\tb\\nc\\\\d", { headers: false });

    expect(result[0][0]).toBe("a\tb\nc\\d");
  });

  test("handles escape at end of field", () => {
    const result = TSV.parse("trailing\\\\", { headers: false });

    expect(result[0][0]).toBe("trailing\\");
  });

  test("unescapes in header mode too", () => {
    const result = TSV.parse("key\nval\\tue");

    expect(result[0].key).toBe("val\tue");
  });
});

describe.runIf(hasTSV)("TSV.parse edge cases", () => {
  test("handles empty fields", () => {
    const result = TSV.parse("a\t\tc", { headers: false });

    expect(result[0][0]).toBe("a");
    expect(result[0][1]).toBe("");
    expect(result[0][2]).toBe("c");
  });

  test("handles leading empty field", () => {
    const result = TSV.parse("\tb\tc", { headers: false });

    expect(result[0][0]).toBe("");
    expect(result[0][1]).toBe("b");
  });

  test("handles trailing tab", () => {
    const result = TSV.parse("a\tb\t", { headers: false });

    expect(result[0].length).toBe(3);
    expect(result[0][2]).toBe("");
  });

  test("handles CRLF line endings", () => {
    const result = TSV.parse("name\tage\r\nAlice\t30\r\nBob\t25");

    expect(result.length).toBe(2);
    expect(result[0].name).toBe("Alice");
    expect(result[1].name).toBe("Bob");
  });

  test("handles CR-only line endings", () => {
    const result = TSV.parse("1\t2\r3\t4", { headers: false });

    expect(result.length).toBe(2);
  });

  test("preserves empty rows by default", () => {
    const result = TSV.parse("1\n\n2", { headers: false });

    expect(result.length).toBe(3);
    expect(result[1][0]).toBe("");
  });

  test("skips empty rows with skipEmptyLines", () => {
    const result = TSV.parse("1\n\n2", {
      headers: false,
      skipEmptyLines: true,
    });

    expect(result.length).toBe(2);
    expect(result[0][0]).toBe("1");
    expect(result[1][0]).toBe("2");
  });

  test("pads ragged rows with empty strings", () => {
    const result = TSV.parse("a\tb\tc\n1");

    expect(result[0].a).toBe("1");
    expect(result[0].b).toBe("");
    expect(result[0].c).toBe("");
  });

  test("all values are strings — no type coercion", () => {
    const result = TSV.parse("v\n42\ntrue\nnull\n0");

    expect(typeof result[0].v).toBe("string");
    expect(result[0].v).toBe("42");
    expect(result[1].v).toBe("true");
    expect(result[2].v).toBe("null");
    expect(result[3].v).toBe("0");
  });

  test("returns empty array for empty input", () => {
    const result = TSV.parse("");

    expect(result.length).toBe(0);
  });

  test("handles many columns", () => {
    const headers = Array.from({ length: 20 }, (_, i) => "c" + i).join("\t");
    const values = Array.from({ length: 20 }, (_, i) => String(i)).join("\t");
    const result = TSV.parse(headers + "\n" + values);

    expect(result.length).toBe(1);
    expect(result[0].c0).toBe("0");
    expect(result[0].c19).toBe("19");
  });

  test("whitespace is preserved (no trimming)", () => {
    const result = TSV.parse("  a  \t b ", { headers: false });

    expect(result[0][0]).toBe("  a  ");
    expect(result[0][1]).toBe(" b ");
  });
});

describe.runIf(hasTSV)("TSV.parse error handling", () => {
  test("throws TypeError when first argument is not a string", () => {
    expect(() => TSV.parse(42)).toThrow(TypeError);
  });

  test("throws TypeError when first argument is null", () => {
    expect(() => TSV.parse(null)).toThrow(TypeError);
  });

  test("throws when called with no arguments", () => {
    expect(() => TSV.parse()).toThrow();
  });
});

describe.runIf(hasTSV)("TSV metadata", () => {
  test("Symbol.toStringTag is TSV", () => {
    expect(TSV[Symbol.toStringTag]).toBe("TSV");
  });
});
