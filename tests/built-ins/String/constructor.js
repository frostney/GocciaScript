/*---
description: String() constructor for type conversion
features: [String]
---*/

describe("String() constructor", () => {
  test("converts numbers to strings", () => {
    expect(String(123)).toBe("123");
    expect(String(0)).toBe("0");
    expect(String(-42)).toBe("-42");
    expect(String(3.14)).toBe("3.14");
  });

  test("converts boolean to string", () => {
    expect(String(true)).toBe("true");
    expect(String(false)).toBe("false");
  });

  test("converts null to string", () => {
    expect(String(null)).toBe("null");
  });

  test("converts undefined to string", () => {
    expect(String(undefined)).toBe("undefined");
  });

  test("converts NaN to string", () => {
    expect(String(NaN)).toBe("NaN");
  });

  test("converts Infinity to string", () => {
    expect(String(Infinity)).toBe("Infinity");
    expect(String(-Infinity)).toBe("-Infinity");
  });

  test("string passes through unchanged", () => {
    expect(String("hello")).toBe("hello");
    expect(String("")).toBe("");
  });

  test("template literal converts boolean to string", () => {
    expect(`${true}`).toBe("true");
    expect(`${false}`).toBe("false");
  });

  test("template literal converts null to string", () => {
    expect(`${null}`).toBe("null");
  });

  test("template literal converts undefined to string", () => {
    expect(`${undefined}`).toBe("undefined");
  });
});
