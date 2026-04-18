/*---
description: CSV.parse converts CSV text to JavaScript arrays/objects
features: [CSV.parse]
---*/

const hasCSV = typeof CSV !== "undefined";

describe.runIf(hasCSV)("CSV.parse", () => {
  test("parses CSV with headers (default)", () => {
    const result = CSV.parse("name,age\nAlice,30\nBob,25");

    expect(result.length).toBe(2);
    expect(result[0].name).toBe("Alice");
    expect(result[0].age).toBe("30");
    expect(result[1].name).toBe("Bob");
    expect(result[1].age).toBe("25");
  });

  test("parses CSV without headers", () => {
    const result = CSV.parse("Alice,30\nBob,25", { headers: false });

    expect(result.length).toBe(2);
    expect(result[0][0]).toBe("Alice");
    expect(result[0][1]).toBe("30");
    expect(result[1][0]).toBe("Bob");
    expect(result[1][1]).toBe("25");
  });

  test("parses single-column CSV", () => {
    const result = CSV.parse("v\n1\n2\n3");

    expect(result.length).toBe(3);
    expect(result[0].v).toBe("1");
    expect(result[2].v).toBe("3");
  });

  test("parses single-row CSV with headers", () => {
    const result = CSV.parse("a,b\n1,2");

    expect(result.length).toBe(1);
    expect(result[0].a).toBe("1");
    expect(result[0].b).toBe("2");
  });

  test("parses header-only CSV as empty array", () => {
    const result = CSV.parse("a,b,c");

    expect(result.length).toBe(0);
  });

  test("handles quoted fields with commas", () => {
    const result = CSV.parse('name,address\nAlice,"123 Main St, Apt 4"');

    expect(result.length).toBe(1);
    expect(result[0].address).toBe("123 Main St, Apt 4");
  });

  test("handles quoted fields with escaped quotes", () => {
    const result = CSV.parse('value\n"He said ""hello"""');

    expect(result.length).toBe(1);
    expect(result[0].value).toBe('He said "hello"');
  });

  test("handles double-quoted field that is entirely escaped quotes", () => {
    const result = CSV.parse('""""""', { headers: false });

    expect(result[0][0]).toBe('""');
  });

  test("handles newlines within quoted fields", () => {
    const result = CSV.parse('msg\n"line 1\nline 2"');

    expect(result.length).toBe(1);
    expect(result[0].msg).toBe("line 1\nline 2");
  });

  test("handles CRLF within quoted fields", () => {
    const result = CSV.parse('msg\n"line 1\r\nline 2"');

    expect(result.length).toBe(1);
    expect(result[0].msg).toBe("line 1\r\nline 2");
  });

  test("handles empty fields", () => {
    const result = CSV.parse("1,,3", { headers: false });

    expect(result[0][0]).toBe("1");
    expect(result[0][1]).toBe("");
    expect(result[0][2]).toBe("3");
  });

  test("handles leading empty field", () => {
    const result = CSV.parse(",b,c", { headers: false });

    expect(result[0][0]).toBe("");
    expect(result[0][1]).toBe("b");
    expect(result[0][2]).toBe("c");
  });

  test("handles trailing delimiter", () => {
    const result = CSV.parse("a,b,", { headers: false });

    expect(result[0].length).toBe(3);
    expect(result[0][2]).toBe("");
  });

  test("handles multiple trailing delimiters", () => {
    const result = CSV.parse("a,,", { headers: false });

    expect(result[0].length).toBe(3);
    expect(result[0][0]).toBe("a");
    expect(result[0][1]).toBe("");
    expect(result[0][2]).toBe("");
  });

  test("handles quoted empty fields", () => {
    const result = CSV.parse('"",b', { headers: false });

    expect(result[0][0]).toBe("");
    expect(result[0][1]).toBe("b");
  });

  test("preserves empty rows by default", () => {
    const result = CSV.parse("1\n\n2", { headers: false });

    expect(result.length).toBe(3);
    expect(result[0][0]).toBe("1");
    expect(result[1][0]).toBe("");
    expect(result[2][0]).toBe("2");
  });

  test("skips empty rows with skipEmptyLines option", () => {
    const result = CSV.parse("1\n\n2", {
      headers: false,
      skipEmptyLines: true,
    });

    expect(result.length).toBe(2);
    expect(result[0][0]).toBe("1");
    expect(result[1][0]).toBe("2");
  });

  test("skipEmptyLines preserves rows that have delimiters", () => {
    const result = CSV.parse("1,2\n,\n3,4", {
      headers: false,
      skipEmptyLines: true,
    });

    expect(result.length).toBe(3);
  });

  test("supports semicolon delimiter", () => {
    const result = CSV.parse("name;age\nAlice;30", { delimiter: ";" });

    expect(result[0].name).toBe("Alice");
    expect(result[0].age).toBe("30");
  });

  test("supports pipe delimiter", () => {
    const result = CSV.parse("a|b\n1|2", { delimiter: "|" });

    expect(result[0].a).toBe("1");
    expect(result[0].b).toBe("2");
  });

  test("quoting still works with non-comma delimiters", () => {
    const result = CSV.parse('a;b\n"x;y";z', { delimiter: ";" });

    expect(result[0].a).toBe("x;y");
    expect(result[0].b).toBe("z");
  });

  test("handles CRLF line endings", () => {
    const result = CSV.parse("name,age\r\nAlice,30\r\nBob,25");

    expect(result.length).toBe(2);
    expect(result[0].name).toBe("Alice");
    expect(result[1].name).toBe("Bob");
  });

  test("handles CR-only line endings", () => {
    const result = CSV.parse("1,2\r3,4\r5,6", { headers: false });

    expect(result.length).toBe(3);
    expect(result[2][0]).toBe("5");
  });

  test("handles mixed line endings", () => {
    const result = CSV.parse("a\nb\r\nc\rd", { headers: false });

    expect(result.length).toBe(4);
  });

  test("pads ragged rows with empty strings", () => {
    const result = CSV.parse("a,b,c\n1");

    expect(result[0].a).toBe("1");
    expect(result[0].b).toBe("");
    expect(result[0].c).toBe("");
  });

  test("extra columns beyond headers are ignored", () => {
    const result = CSV.parse("a,b\n1,2,3,4");

    expect(result[0].a).toBe("1");
    expect(result[0].b).toBe("2");
  });

  test("all values are strings — no type coercion", () => {
    const result = CSV.parse("v\n42\ntrue\n\nnull\nundefined\n0\nfalse");

    expect(typeof result[0].v).toBe("string");
    expect(result[0].v).toBe("42");
    expect(result[1].v).toBe("true");
    expect(result[2].v).toBe("");
    expect(result[3].v).toBe("null");
    expect(result[4].v).toBe("undefined");
    expect(result[5].v).toBe("0");
    expect(result[6].v).toBe("false");
  });

  test("returns empty array for empty input", () => {
    const result = CSV.parse("");

    expect(result.length).toBe(0);
  });

  test("handles whitespace-only fields without trimming", () => {
    const result = CSV.parse("  a  , b ", { headers: false });

    expect(result[0][0]).toBe("  a  ");
    expect(result[0][1]).toBe(" b ");
  });

  test("handles fields with only whitespace in header mode", () => {
    const result = CSV.parse("a,b\n , ");

    expect(result[0].a).toBe(" ");
    expect(result[0].b).toBe(" ");
  });

  test("handles many columns", () => {
    const headers = Array.from({ length: 20 }, (_, i) => "c" + i).join(",");
    const values = Array.from({ length: 20 }, (_, i) => String(i)).join(",");
    const result = CSV.parse(headers + "\n" + values);

    expect(result.length).toBe(1);
    expect(result[0].c0).toBe("0");
    expect(result[0].c19).toBe("19");
  });

  test("handles many rows", () => {
    const rows = Array.from({ length: 100 }, (_, i) => String(i)).join("\n");
    const result = CSV.parse("v\n" + rows);

    expect(result.length).toBe(100);
    expect(result[99].v).toBe("99");
  });
});

describe.runIf(hasCSV)("CSV.parse error handling", () => {
  test("throws SyntaxError on unterminated quoted field", () => {
    expect(() => CSV.parse('"unterminated')).toThrow(SyntaxError);
  });

  test("throws SyntaxError on unterminated quoted field mid-row", () => {
    expect(() => CSV.parse('a,b\n"ok","not closed')).toThrow(SyntaxError);
  });

  test("throws SyntaxError on unterminated quote in header", () => {
    expect(() => CSV.parse('"name\nAlice')).toThrow(SyntaxError);
  });

  test("throws TypeError when first argument is not a string", () => {
    expect(() => CSV.parse(42)).toThrow(TypeError);
  });

  test("throws TypeError when first argument is null", () => {
    expect(() => CSV.parse(null)).toThrow(TypeError);
  });

  test("throws TypeError when first argument is undefined", () => {
    expect(() => CSV.parse(undefined)).toThrow(TypeError);
  });

  test("throws TypeError when called with no arguments", () => {
    expect(() => CSV.parse()).toThrow();
  });
});

describe.runIf(hasCSV)("CSV.parse with reviver", () => {
  test("reviver receives key, value, and context", () => {
    const calls = [];
    CSV.parse("a,b\n1,2", {}, (key, value, ctx) => {
      calls.push({ key, value, quoted: ctx.quoted, row: ctx.row, column: ctx.column });
      return value;
    });

    expect(calls.length).toBe(2);
    expect(calls[0].key).toBe("a");
    expect(calls[0].value).toBe("1");
    expect(calls[0].quoted).toBe(false);
    expect(calls[0].row).toBe(0);
    expect(calls[0].column).toBe(0);
    expect(calls[1].key).toBe("b");
    expect(calls[1].value).toBe("2");
    expect(calls[1].column).toBe(1);
  });

  test("reviver can transform values to numbers", () => {
    const result = CSV.parse("n\n42\nhello", {}, (key, value, ctx) => {
      const n = Number(value);
      return Number.isNaN(n) ? value : n;
    });

    expect(result[0].n).toBe(42);
    expect(result[1].n).toBe("hello");
  });

  test("reviver can return null", () => {
    const result = CSV.parse("a\nfoo\n", {}, (key, value, ctx) => {
      if (value === "foo") return null;
      return value;
    });

    expect(result[0].a).toBe(null);
  });

  test("reviver distinguishes quoted from unquoted empty fields", () => {
    const result = CSV.parse('a,b\n"",,', {}, (key, value, ctx) => {
      if (!ctx.quoted && value === "") return null;
      return value;
    });

    expect(result[0].a).toBe("");
    expect(result[0].b).toBe(null);
  });

  test("reviver quoted flag is true for quoted fields", () => {
    const quoted = [];
    CSV.parse('a,b\n"x",y', {}, (key, value, ctx) => {
      quoted.push(ctx.quoted);
      return value;
    });

    expect(quoted[0]).toBe(true);
    expect(quoted[1]).toBe(false);
  });

  test("reviver row index increments per data row", () => {
    const rows = [];
    CSV.parse("h\na\nb\nc", {}, (key, value, ctx) => {
      rows.push(ctx.row);
      return value;
    });

    expect(rows[0]).toBe(0);
    expect(rows[1]).toBe(1);
    expect(rows[2]).toBe(2);
  });

  test("reviver works with headers: false", () => {
    const result = CSV.parse("1,2\n3,4", { headers: false }, (key, value, ctx) => {
      return Number(value) * 10;
    });

    expect(result[0][0]).toBe(10);
    expect(result[0][1]).toBe(20);
    expect(result[1][0]).toBe(30);
  });

  test("non-callable second arg is treated as options, not reviver", () => {
    const result = CSV.parse("a\n1", { headers: true });

    expect(result[0].a).toBe("1");
  });
});

describe.runIf(hasCSV)("CSV metadata", () => {
  test("Symbol.toStringTag is CSV", () => {
    expect(CSV[Symbol.toStringTag]).toBe("CSV");
  });
});
