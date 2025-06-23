/*---
description: Conditional (ternary) operator works correctly including nested forms
features: [ternary-operator]
---*/

test("basic ternary operator", () => {
  let a = 5;
  let b = 10;
  let max = a > b ? a : b;
  expect(max).toBe(10);
});

test("nested ternary operators", () => {
  const categorize = (score) => {
    return score >= 90
      ? "A"
      : score >= 80
      ? "B"
      : score >= 70
      ? "C"
      : score >= 60
      ? "D"
      : "F";
  };

  expect(categorize(95)).toBe("A");
  expect(categorize(85)).toBe("B");
  expect(categorize(75)).toBe("C");
  expect(categorize(65)).toBe("D");
  expect(categorize(55)).toBe("F");
});

test("ternary with side effects", () => {
  let count = 0;
  let result = true ? ++count : --count;
  expect(result).toBe(1);
  expect(count).toBe(1);
});
