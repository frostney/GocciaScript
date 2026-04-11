/*---
description: String.raw static method
features: [tagged-templates, String.raw]
---*/

describe("String.raw", () => {
  test("basic usage as tag function", () => {
    expect(String.raw`Hello\nWorld`).toBe("Hello\\nWorld");
  });

  test("preserves raw escape sequences", () => {
    expect(String.raw`\t\r\n\\`).toBe("\\t\\r\\n\\\\");
  });

  test("with interpolations", () => {
    const name = "world";
    expect(String.raw`Hello\n${name}`).toBe("Hello\\n" + name);
  });

  test("called as a function with raw object", () => {
    expect(String.raw({ raw: ["a", "b"] }, 1)).toBe("a1b");
  });

  test("called as a function with multiple substitutions", () => {
    expect(String.raw({ raw: ["a", "b", "c"] }, 1, 2)).toBe("a1b2c");
  });

  test("called as a function with no substitutions", () => {
    expect(String.raw({ raw: ["hello"] })).toBe("hello");
  });

  test("called as a function with empty raw array", () => {
    expect(String.raw({ raw: [] })).toBe("");
  });

  test("fewer substitutions than gaps are treated as empty", () => {
    expect(String.raw({ raw: ["a", "b", "c"] }, 1)).toBe("a1bc");
  });

  test("extra substitutions are ignored", () => {
    expect(String.raw({ raw: ["a", "b"] }, 1, 2, 3)).toBe("a1b");
  });

  test("substitution values are coerced to strings", () => {
    expect(String.raw({ raw: ["a", "b", "c"] }, 42, true)).toBe("a42btruec");
  });

  test("empty template", () => {
    expect(String.raw``).toBe("");
  });

  test("template with only interpolation", () => {
    expect(String.raw`${42}`).toBe("42");
  });

  test("backtick escape preserved in raw", () => {
    expect(String.raw`\``).toBe("\\`");
  });

  test("dollar escape preserved in raw", () => {
    expect(String.raw`\$`).toBe("\\$");
  });

  test("throws TypeError for undefined template", () => {
    expect(() => String.raw(undefined)).toThrow(TypeError);
  });

  test("throws TypeError for null template", () => {
    expect(() => String.raw(null)).toThrow(TypeError);
  });
});
