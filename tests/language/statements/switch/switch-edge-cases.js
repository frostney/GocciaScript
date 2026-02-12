/*---
description: Switch statement edge cases and advanced patterns
features: [switch-statement]
---*/

test("switch with expression as discriminant", () => {
  const x = 5;
  let result;
  switch (x + 5) {
    case 10:
      result = "ten";
      break;
    case 20:
      result = "twenty";
      break;
    default:
      result = "other";
  }
  expect(result).toBe("ten");
});

test("switch uses strict equality", () => {
  let result;
  switch (1) {
    case "1":
      result = "string match";
      break;
    case 1:
      result = "number match";
      break;
    default:
      result = "no match";
  }
  expect(result).toBe("number match");
});

test("switch with only default case", () => {
  let result;
  switch ("anything") {
    default:
      result = "default";
  }
  expect(result).toBe("default");
});

test("switch with empty case fall-through to default", () => {
  let result;
  switch (42) {
    case 1:
    case 2:
    case 3:
    default:
      result = "reached default";
  }
  expect(result).toBe("reached default");
});

test("switch with return in arrow function", () => {
  const classify = (n) => {
    switch (true) {
      case n < 0:
        return "negative";
      case n === 0:
        return "zero";
      case n > 0:
        return "positive";
      default:
        return "unknown";
    }
  };

  expect(classify(-5)).toBe("negative");
  expect(classify(0)).toBe("zero");
  expect(classify(10)).toBe("positive");
});

test("switch with block scoping in cases", () => {
  let result;
  switch (2) {
    case 1: {
      const msg = "one";
      result = msg;
      break;
    }
    case 2: {
      const msg = "two";
      result = msg;
      break;
    }
    case 3: {
      const msg = "three";
      result = msg;
      break;
    }
  }
  expect(result).toBe("two");
});

test("switch with null discriminant", () => {
  let result;
  switch (null) {
    case undefined:
      result = "undefined";
      break;
    case null:
      result = "null";
      break;
    default:
      result = "default";
  }
  expect(result).toBe("null");
});

test("switch with boolean discriminant", () => {
  let result;
  switch (true) {
    case false:
      result = "false";
      break;
    case true:
      result = "true";
      break;
    default:
      result = "default";
  }
  expect(result).toBe("true");
});
