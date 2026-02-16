/*---
description: Number.prototype.toString correctly converts numbers to string representation
features: [Number.prototype.toString]
---*/

describe("Number.prototype.toString", () => {
  test("toString with no radix returns decimal string", () => {
    expect((42).toString()).toBe("42");
    expect((3.14).toString()).toBe("3.14");
    expect((0).toString()).toBe("0");
    expect((-7).toString()).toBe("-7");
  });

  test("toString with radix 16 (hex)", () => {
    expect((255).toString(16)).toBe("ff");
    expect((16).toString(16)).toBe("10");
    expect((0).toString(16)).toBe("0");
  });

  test("toString with radix 10", () => {
    expect((42).toString(10)).toBe("42");
    expect((0).toString(10)).toBe("0");
  });

  test("toString on NaN", () => {
    expect(NaN.toString()).toBe("NaN");
  });

  test("toString on Infinity", () => {
    expect(Infinity.toString()).toBe("Infinity");
    expect((-Infinity).toString()).toBe("-Infinity");
  });

  test("toString on negative zero", () => {
    expect((-0).toString()).toBe("0");
  });
});
