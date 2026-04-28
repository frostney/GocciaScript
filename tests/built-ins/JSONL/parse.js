const hasJSONL = typeof JSONL !== "undefined";

describe.runIf(hasJSONL)("JSONL.parse", () => {
  test("parses mixed JSON values and ignores blank lines", () => {
    const records = JSONL.parse(
      '{"id":1,"name":"alpha"}\n\n42\n true \n["x","y"]',
    );

    expect(records.length).toBe(4);
    expect(records[0].id).toBe(1);
    expect(records[0].name).toBe("alpha");
    expect(records[1]).toBe(42);
    expect(records[2]).toBe(true);
    expect(records[3][0]).toBe("x");
    expect(records[3][1]).toBe("y");
  });

  test("parses a final line without a trailing newline", () => {
    const records = JSONL.parse('{"id":1}\n{"id":2}');

    expect(records.length).toBe(2);
    expect(records[1].id).toBe(2);
  });

  test("accepts Uint8Array input and skips a UTF-8 BOM", () => {
    const bytes = new Uint8Array([
      0xef, 0xbb, 0xbf, 0x7b, 0x22, 0x61, 0x22, 0x3a, 0x31, 0x7d, 0x0a, 0x7b,
      0x22, 0x62, 0x22, 0x3a, 0x32, 0x7d,
    ]);
    const records = JSONL.parse(bytes);

    expect(records.length).toBe(2);
    expect(records[0].a).toBe(1);
    expect(records[1].b).toBe(2);
  });

  test("decodes UTF-8 text from Uint8Array input", () => {
    const bytes = new Uint8Array([
      0x7b, 0x22, 0x6e, 0x61, 0x6d, 0x65, 0x22, 0x3a, 0x22, 0x4a, 0x6f,
      0x73, 0xc3, 0xa9, 0x22, 0x2c, 0x22, 0x63, 0x69, 0x74, 0x79, 0x22,
      0x3a, 0x22, 0x5a, 0xc3, 0xbc, 0x72, 0x69, 0x63, 0x68, 0x22, 0x7d,
    ]);
    const records = JSONL.parse(bytes);

    expect(records.length).toBe(1);
    expect(records[0].name).toBe("Jos\u00e9");
    expect(records[0].city).toBe("Z\u00fcrich");
  });

  test("throws a SyntaxError with the JSONL source line number", () => {
    let error = null;

    try {
      JSONL.parse('{"ok":true}\n{invalid}\n');
    } catch (caught) {
      error = caught;
    }

    expect(error instanceof SyntaxError).toBe(true);
    expect(error.message.includes("JSONL line 2")).toBe(true);
  });

  test("throws TypeError for unsupported typed array input", () => {
    expect(() => JSONL.parse(new Int8Array([123]))).toThrow(TypeError);
  });
});
