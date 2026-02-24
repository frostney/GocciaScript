describe("Array.prototype.toString", () => {
  test("exists on Array.prototype", () => {
    expect(typeof Array.prototype.toString).toBe("function");
  });

  test("returns comma-separated string for simple array", () => {
    expect([1, 2, 3].toString()).toBe("1,2,3");
  });

  test("returns empty string for empty array", () => {
    expect([].toString()).toBe("");
  });

  test("single element array", () => {
    expect([42].toString()).toBe("42");
  });

  test("mixed types", () => {
    expect([1, "a", true, null, undefined].toString()).toBe("1,a,true,,");
  });

  test("nested arrays", () => {
    expect([1, [2, 3], 4].toString()).toBe("1,2,3,4");
  });

  test("string elements", () => {
    expect(["hello", "world"].toString()).toBe("hello,world");
  });

  test("boolean elements", () => {
    expect([true, false].toString()).toBe("true,false");
  });

  test("toString is used in string concatenation", () => {
    expect([1, 2] + [3, 4]).toBe("1,23,4");
    expect("" + [1, 2, 3]).toBe("1,2,3");
  });

  test("toString is used in template literals", () => {
    const arr = [1, 2, 3];
    expect(`${arr}`).toBe("1,2,3");
  });

  test("array with plain objects", () => {
    expect([{}, 1].toString()).toBe("[object Object],1");
  });

  test("array with custom toString objects", () => {
    const obj = { toString() { return "custom"; } };
    expect([obj, 1, 2].toString()).toBe("custom,1,2");
  });

  test("array with custom valueOf (string hint prefers toString)", () => {
    const obj = { valueOf() { return 10; }, toString() { return "str"; } };
    expect([obj].toString()).toBe("str");
  });

  test("array with toString returning non-primitive falls back to valueOf", () => {
    const obj = { valueOf() { return "val"; }, toString() { return {}; } };
    expect([obj].toString()).toBe("val");
  });
});

describe("Array.prototype.join edge cases", () => {
  test("join with empty string separator", () => {
    expect([1, 2, 3].join("")).toBe("123");
  });

  test("join with multi-character separator", () => {
    expect([1, 2, 3].join(" - ")).toBe("1 - 2 - 3");
  });

  test("join with undefined separator defaults to comma", () => {
    expect([1, 2, 3].join(undefined)).toBe("1,2,3");
  });

  test("join with null elements", () => {
    expect([null, null].join(",")).toBe(",");
  });

  test("join with undefined elements", () => {
    expect([undefined, undefined].join(",")).toBe(",");
  });

  test("join with mixed null/undefined/values", () => {
    expect([1, null, 2, undefined, 3].join(",")).toBe("1,,2,,3");
  });

  test("join on single-element array", () => {
    expect([42].join(",")).toBe("42");
  });

  test("join on empty array", () => {
    expect([].join(",")).toBe("");
  });

  test("join with boolean separator", () => {
    expect([1, 2].join(true)).toBe("1true2");
  });

  test("join with number separator", () => {
    expect([1, 2].join(0)).toBe("102");
  });

  test("join with Symbol.toStringTag objects", () => {
    const obj = { [Symbol.toStringTag]: "Custom", toString() { return "tagged"; } };
    expect([obj].join(",")).toBe("tagged");
  });
});

describe("Array.prototype.toString with special values", () => {
  test("array of NaN values", () => {
    expect([NaN, NaN].toString()).toBe("NaN,NaN");
  });

  test("array of Infinity values", () => {
    expect([Infinity, -Infinity].toString()).toBe("Infinity,-Infinity");
  });

  test("array with boolean values", () => {
    expect([true, false, true].toString()).toBe("true,false,true");
  });

  test("deeply nested arrays", () => {
    expect([1, [2, [3, [4]]]].toString()).toBe("1,2,3,4");
  });
});
