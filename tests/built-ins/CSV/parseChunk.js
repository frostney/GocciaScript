/*---
description: CSV.parseChunk parses CSV text incrementally for streaming
features: [CSV.parseChunk]
---*/

const hasCSV = typeof CSV !== "undefined";

describe.runIf(hasCSV)("CSV.parseChunk", () => {
  test("parses a complete chunk with headers", () => {
    const result = CSV.parseChunk("name,age\nAlice,30\nBob,25\n", {});

    expect(result.values.length).toBe(2);
    expect(result.done).toBe(true);
    expect(result.error).toBe(null);
    expect(result.values[0].name).toBe("Alice");
    expect(result.values[1].name).toBe("Bob");
  });

  test("returns done false for incomplete quoted field", () => {
    const result = CSV.parseChunk('name,age\nAlice,"30', {});

    expect(result.done).toBe(false);
  });

  test("parses chunk without headers", () => {
    const result = CSV.parseChunk("a,b\n1,2\n", { headers: false });

    expect(result.values.length).toBe(2);
    expect(result.values[0][0]).toBe("a");
    expect(result.values[1][0]).toBe("1");
  });

  test("returns read offset for resume", () => {
    const result = CSV.parseChunk("name\nAlice\nBob\n", {});

    expect(typeof result.read).toBe("number");
    expect(result.read).toBeGreaterThan(0);
  });

  test("parses empty input", () => {
    const result = CSV.parseChunk("", {});

    expect(result.values.length).toBe(0);
    expect(result.done).toBe(true);
  });

  test("throws TypeError when first argument is not a string", () => {
    expect(() => CSV.parseChunk(42, {})).toThrow(TypeError);
  });

  test("throws when called with no arguments", () => {
    expect(() => CSV.parseChunk()).toThrow();
  });
});
