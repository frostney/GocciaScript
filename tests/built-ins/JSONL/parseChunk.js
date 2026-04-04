describe("JSONL.parseChunk", () => {
  test("parses complete records and leaves an incomplete record unread", () => {
    const input = '{"id":1}\n{"id":2}\n{"id":';
    const secondNewline = input.indexOf("\n", input.indexOf("\n") + 1);
    const result = JSONL.parseChunk(input);

    expect(result.values.length).toBe(2);
    expect(result.values[0].id).toBe(1);
    expect(result.values[1].id).toBe(2);
    expect(result.read).toBe(secondNewline);
    expect(result.done).toBe(false);
    expect(result.error).toBe(null);
  });

  test("marks a fully consumed chunk as done", () => {
    const input = '{"id":1}\n{"id":2}';
    const result = JSONL.parseChunk(input);

    expect(result.values.length).toBe(2);
    expect(result.read).toBe(input.length);
    expect(result.done).toBe(true);
    expect(result.error).toBe(null);
  });

  test("returns a SyntaxError object for invalid data lines", () => {
    const input = '{"id":1}\n{invalid}\n{"id":2}\n';
    const firstNewline = input.indexOf("\n");
    const result = JSONL.parseChunk(input);

    expect(result.values.length).toBe(1);
    expect(result.values[0].id).toBe(1);
    expect(result.read).toBe(firstNewline);
    expect(result.done).toBe(false);
    expect(result.error instanceof SyntaxError).toBe(true);
    expect(result.error.message.includes("JSONL line 2")).toBe(true);
  });

  test("supports Uint8Array ranges", () => {
    const bytes = new Uint8Array([
      0x7b, 0x22, 0x61, 0x22, 0x3a, 0x31, 0x7d, 0x0a,
      0x7b, 0x22, 0x62, 0x22, 0x3a, 0x32, 0x7d, 0x0a,
      0x7b, 0x22, 0x63, 0x22, 0x3a, 0x33, 0x7d, 0x0a,
    ]);
    const result = JSONL.parseChunk(bytes, 8, 16);

    expect(result.values.length).toBe(1);
    expect(result.values[0].b).toBe(2);
    expect(result.read).toBe(16);
    expect(result.done).toBe(true);
    expect(result.error).toBe(null);
  });

  test("throws TypeError for unsupported typed array input", () => {
    expect(() => JSONL.parseChunk(new Int8Array([123]))).toThrow(TypeError);
  });
});
