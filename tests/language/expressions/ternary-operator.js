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

test("alternate can read the destination's previous value", () => {
  const fallback = () => "fallback";
  const replacement = () => "replacement";
  let current = () => "current";

  current = current === fallback ? replacement : current;

  expect(current()).toBe("current");
});

test("parenthesized consequent does not make later division look like regex", () => {
  const condition = true;
  const chosen = condition ? (2) : 3;
  const start = 1;
  const end = 3;
  const step = 1;
  const quotient = (end - start) / (step || 1);

  expect(chosen).toBe(2);
  expect(quotient).toBe(2);
});

test("parenthesized calls before the alternate are not parsed as arrow parameters", () => {
  const first = () => 1;
  const second = () => 2;
  const selected = true ? (first(), second()) : 3;
  const laterArrow = () => 4;

  expect(selected).toBe(2);
  expect(laterArrow()).toBe(4);
});
