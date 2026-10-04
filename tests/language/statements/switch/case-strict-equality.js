/*---
description: |
  A switch statement selects its case with strict equality (ES2026 §14.12.4
  CaseClauseIsSelected), whichever way the discriminant was produced: a
  literal, the result of arithmetic, or a value read back from a property.
features: [switch-statement, strict-equality-operator]
---*/

const subtract = (left, right) => left - right;
const divide = (left, right) => left / right;
const negate = (operand) => !operand;

const holder = { nil: null };
const firstObject = { name: "first" };
const secondObject = { name: "first" };

test("a switch statement selects its case by strict equality", () => {
  const classify = (value) => {
    switch (value) {
      case 0:
        return "zero";
      case 0.5:
        return "half";
      case NaN:
        return "never";
      case null:
        return "null";
      case undefined:
        return "undefined";
      case true:
        return "true";
      case "7":
        return "text seven";
      case 7:
        return "seven";
      case firstObject:
        return "first object";
      default:
        return "default";
    }
  };
  expect(classify(subtract(5, 5))).toBe("zero");
  expect(classify(-0)).toBe("zero");
  expect(classify(divide(1, 2))).toBe("half");
  expect(classify(divide(0, 0))).toBe("default");
  expect(classify(holder.nil)).toBe("null");
  expect(classify(holder.absent)).toBe("undefined");
  expect(classify(negate(false))).toBe("true");
  expect(classify(false)).toBe("default");
  expect(classify(divide(14, 2))).toBe("seven");
  expect(classify("7")).toBe("text seven");
  expect(classify(firstObject)).toBe("first object");
  expect(classify(secondObject)).toBe("default");
  expect(classify(1)).toBe("default");
});
