/*---
description: If-else statements work correctly including nested and chained forms
features: [if-statement, else-statement]
---*/

test("if statement with false condition", () => {
  let result = "not changed";
  if (false) {
    result = "changed";
  }
  expect(result).toBe("not changed");
});

test("if-else statement", () => {
  let result1, result2;

  if (true) {
    result1 = "if branch";
  } else {
    result1 = "else branch";
  }

  if (false) {
    result2 = "if branch";
  } else {
    result2 = "else branch";
  }

  expect(result1).toBe("if branch");
  expect(result2).toBe("else branch");
});

test("if-else-if chain", () => {
  function checkValue(x) {
    if (x > 10) {
      return "greater than 10";
    } else if (x > 5) {
      return "greater than 5";
    } else if (x > 0) {
      return "greater than 0";
    } else {
      return "zero or negative";
    }
  }

  expect(checkValue(15)).toBe("greater than 10");
  expect(checkValue(8)).toBe("greater than 5");
  expect(checkValue(3)).toBe("greater than 0");
  expect(checkValue(-1)).toBe("zero or negative");
});

test("nested if statements", () => {
  function nestedTest(a, b) {
    if (a > 0) {
      if (b > 0) {
        return "both positive";
      } else {
        return "a positive, b non-positive";
      }
    } else {
      return "a non-positive";
    }
  }

  expect(nestedTest(1, 1)).toBe("both positive");
  expect(nestedTest(1, -1)).toBe("a positive, b non-positive");
  expect(nestedTest(-1, 1)).toBe("a non-positive");
});
