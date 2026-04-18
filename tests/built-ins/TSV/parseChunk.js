/*---
description: TSV.parseChunk parses tab-separated text incrementally for streaming
features: [TSV.parseChunk]
---*/

const hasTSV = typeof TSV !== "undefined";

describe.runIf(hasTSV)("TSV.parseChunk", () => {
  test("parses a complete chunk with headers", () => {
    const result = TSV.parseChunk("name\tage\nAlice\t30\nBob\t25\n", {});

    expect(result.values.length).toBe(2);
    expect(result.done).toBe(true);
    expect(result.error).toBe(null);
    expect(result.values[0].name).toBe("Alice");
    expect(result.values[1].name).toBe("Bob");
  });

  test("parses chunk without headers", () => {
    const result = TSV.parseChunk("a\tb\n1\t2\n", { headers: false });

    expect(result.values.length).toBe(2);
    expect(result.values[0][0]).toBe("a");
    expect(result.values[1][0]).toBe("1");
  });

  test("returns read offset for resume", () => {
    const result = TSV.parseChunk("name\nAlice\nBob\n", {});

    expect(typeof result.read).toBe("number");
    expect(result.read).toBeGreaterThan(0);
  });

  test("parses empty input", () => {
    const result = TSV.parseChunk("", {});

    expect(result.values.length).toBe(0);
    expect(result.done).toBe(true);
  });

  test("throws TypeError when first argument is not a string", () => {
    expect(() => TSV.parseChunk(42, {})).toThrow(TypeError);
  });

  test("throws when called with no arguments", () => {
    expect(() => TSV.parseChunk()).toThrow();
  });
});
