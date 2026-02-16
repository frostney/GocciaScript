/*---
description: Type coercion in comparison and equality operators
features: [type-coercion, comparison]
---*/

describe("strict equality (===) - no coercion", () => {
  test("different types are never strictly equal", () => {
    expect(1 === "1").toBe(false);
    expect(0 === false).toBe(false);
    expect("" === false).toBe(false);
    expect(null === undefined).toBe(false);
    expect(0 === null).toBe(false);
    expect("" === null).toBe(false);
  });

  test("same type same value are strictly equal", () => {
    expect(1 === 1).toBe(true);
    expect("abc" === "abc").toBe(true);
    expect(true === true).toBe(true);
    expect(null === null).toBe(true);
    expect(undefined === undefined).toBe(true);
  });

  test("NaN is not strictly equal to itself", () => {
    expect(NaN === NaN).toBe(false);
  });

  test("positive and negative zero are strictly equal", () => {
    expect(0 === -0).toBe(true);
  });
});

describe("strict inequality (!==)", () => {
  test("different types are always strictly inequal", () => {
    expect(1 !== "1").toBe(true);
    expect(0 !== false).toBe(true);
  });

  test("same type same value", () => {
    expect(1 !== 1).toBe(false);
    expect("abc" !== "abc").toBe(false);
  });

  test("NaN is strictly inequal to itself", () => {
    expect(NaN !== NaN).toBe(true);
  });
});

describe("relational operators coerce to number", () => {
  test("string comparison with numbers", () => {
    expect("5" > 3).toBe(true);
    expect("2" < 10).toBe(true);
    expect("10" >= 10).toBe(true);
    expect("5" <= 5).toBe(true);
  });

  test("boolean comparison", () => {
    expect(true > 0).toBe(true);
    expect(true >= 1).toBe(true);
    expect(false < 1).toBe(true);
    expect(false <= 0).toBe(true);
  });

  test("null in comparisons", () => {
    expect(null < 1).toBe(true);
    expect(null >= 0).toBe(true);
  });

  test("string vs string uses lexicographic order", () => {
    expect("b" > "a").toBe(true);
    expect("apple" < "banana").toBe(true);
    expect("abc" < "abd").toBe(true);
  });

  test("comparisons with NaN always return false", () => {
    expect(NaN > 0).toBe(false);
    expect(NaN < 0).toBe(false);
    expect(NaN >= 0).toBe(false);
    expect(NaN <= 0).toBe(false);
  });
});
