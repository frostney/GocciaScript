/*---
description: |
  Strict equality (ES2026 §7.2.14 IsStrictlyEqual) gives the same answer
  whichever way a value reached the operator: as a literal, as the result of
  arithmetic, read back from a property, returned from a call, or absent.

  The bytecode VM keeps numbers, booleans, null and undefined in registers
  without allocating a value for them, and one number can be held in more than
  one form (an int32, a double, or an allocated value for NaN, the infinities
  and negative zero, depending on where it came from). Every pair below is
  compared through function parameters, so nothing is folded at compile time.
features: [strict-equality, strict-inequality]
---*/

const equal = (left, right) => left === right;
const unequal = (left, right) => left !== right;

const add = (left, right) => left + right;
const subtract = (left, right) => left - right;
const divide = (left, right) => left / right;
const negate = (operand) => !operand;
const nothing = () => {};

const holder = {
  nan: NaN,
  infinity: Infinity,
  negativeInfinity: -Infinity,
  negativeZero: -0,
  zero: 0,
  seven: 7,
  half: 0.5,
  beyondInt32: 2147483648,
  nil: null,
  missing: undefined,
  yes: true,
  no: false,
  text: "a",
};

const firstObject = { name: "first" };
const secondObject = { name: "first" };
const list = [1, 2, 3];
const callable = () => 1;
const firstSymbol = Symbol("shared description");
const secondSymbol = Symbol("shared description");
const sparse = [, 1];

// Each entry is [label, value, group]. Two entries are strictly equal exactly
// when they carry the same group; a group of null marks NaN, which equals
// nothing, itself included.
const entries = [
  ["0", 0, "zero"],
  ["5 - 5", subtract(5, 5), "zero"],
  ["holder.zero", holder.zero, "zero"],
  ["-0", -0, "zero"],
  ["0 / -1", divide(0, -1), "zero"],
  ["holder.negativeZero", holder.negativeZero, "zero"],
  ["1", 1, "one"],
  ["3 - 2", subtract(3, 2), "one"],
  ["7", 7, "seven"],
  ["14 / 2", divide(14, 2), "seven"],
  ["holder.seven", holder.seven, "seven"],
  ["-7", -7, "minus seven"],
  ["0.5", 0.5, "half"],
  ["1 / 2", divide(1, 2), "half"],
  ["holder.half", holder.half, "half"],
  ["2147483648", 2147483648, "beyond int32"],
  ["2147483647 + 1", add(2147483647, 1), "beyond int32"],
  ["holder.beyondInt32", holder.beyondInt32, "beyond int32"],
  ["NaN", NaN, null],
  ["0 / 0", divide(0, 0), null],
  ["holder.nan", holder.nan, null],
  ["Infinity", Infinity, "infinity"],
  ["1 / 0", divide(1, 0), "infinity"],
  ["holder.infinity", holder.infinity, "infinity"],
  ["-Infinity", -Infinity, "minus infinity"],
  ["-1 / 0", divide(-1, 0), "minus infinity"],
  ["holder.negativeInfinity", holder.negativeInfinity, "minus infinity"],
  ["undefined", undefined, "undefined"],
  ["void 0", void 0, "undefined"],
  ["holder.missing", holder.missing, "undefined"],
  ["holder.absent", holder.absent, "undefined"],
  ["nothing()", nothing(), "undefined"],
  ["sparse[0]", sparse[0], "undefined"],
  ["null", null, "null"],
  ["holder.nil", holder.nil, "null"],
  ["true", true, "true"],
  ["!false", negate(false), "true"],
  ["holder.yes", holder.yes, "true"],
  ["false", false, "false"],
  ["!true", negate(true), "false"],
  ["holder.no", holder.no, "false"],
  ['"a"', "a", "text a"],
  ['"" + "a"', add("", "a"), "text a"],
  ["holder.text", holder.text, "text a"],
  ['"b"', "b", "text b"],
  ['""', "", "empty text"],
  ['"0"', "0", "text zero"],
  ['"7"', "7", "text seven"],
  ["1n", 1n, "bigint one"],
  ["1n + 0n", add(1n, 0n), "bigint one"],
  ["7n", 7n, "bigint seven"],
  ["0n", 0n, "bigint zero"],
  ["firstSymbol", firstSymbol, "first symbol"],
  ["secondSymbol", secondSymbol, "second symbol"],
  ["firstObject", firstObject, "first object"],
  ["secondObject", secondObject, "second object"],
  ["list", list, "list"],
  ["callable", callable, "callable"],
  ["Object(7)", Object(7), "wrapped seven"],
  ['Object("a")', Object("a"), "wrapped text"],
  ["Object(true)", Object(true), "wrapped true"],
  ["Object(1n)", Object(1n), "wrapped bigint"],
];

const expected = (left, right) => left[2] !== null && left[2] === right[2];

test("=== agrees with the group of every pair of operands", () => {
  const wrong = [];
  for (const left of entries) {
    for (const right of entries) {
      if (equal(left[1], right[1]) !== expected(left, right)) {
        wrong.push(`${left[0]} === ${right[0]}`);
      }
    }
  }
  expect(wrong).toEqual([]);
});

test("!== is the negation of === for every pair of operands", () => {
  const wrong = [];
  for (const left of entries) {
    for (const right of entries) {
      if (unequal(left[1], right[1]) === expected(left, right)) {
        wrong.push(`${left[0]} !== ${right[0]}`);
      }
    }
  }
  expect(wrong).toEqual([]);
});

test("the expectation table itself is not decided by the operators under test", () => {
  // Spot checks written out, so a fault that broke the table's own group
  // comparison cannot hide a wrong answer above.
  expect(equal(subtract(5, 5), -0)).toBe(true);
  expect(equal(divide(0, 0), divide(0, 0))).toBe(false);
  expect(equal(holder.nan, holder.nan)).toBe(false);
  expect(unequal(holder.nan, holder.nan)).toBe(true);
  expect(equal(divide(1, 0), holder.infinity)).toBe(true);
  expect(equal(divide(1, 0), divide(-1, 0))).toBe(false);
  expect(equal(add(2147483647, 1), holder.beyondInt32)).toBe(true);
  expect(equal(divide(14, 2), 7)).toBe(true);
  expect(equal(7, "7")).toBe(false);
  expect(equal(0, "")).toBe(false);
  expect(equal(0, null)).toBe(false);
  expect(equal(0, false)).toBe(false);
  expect(equal(1, true)).toBe(false);
  expect(equal(null, undefined)).toBe(false);
  expect(equal(holder.absent, undefined)).toBe(true);
  expect(equal(sparse[0], undefined)).toBe(true);
  expect(equal(1n, 1)).toBe(false);
  expect(equal(add(1n, 0n), 1n)).toBe(true);
  expect(equal(Object(7), 7)).toBe(false);
  expect(equal(firstObject, secondObject)).toBe(false);
  expect(equal(firstObject, firstObject)).toBe(true);
  expect(unequal(firstObject, null)).toBe(true);
  expect(unequal(firstObject, undefined)).toBe(true);
  expect(equal(firstSymbol, secondSymbol)).toBe(false);
  expect(equal(firstSymbol, firstSymbol)).toBe(true);
  expect(equal(add("", "a"), "a")).toBe(true);
});

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

test("collections that search by strict equality or SameValueZero agree with the operator", () => {
  const values = [NaN, 0, -0, "a", firstObject, null, undefined, 1n, 7];
  expect(values.indexOf(NaN)).toBe(-1);
  expect(values.includes(NaN)).toBe(true);
  expect(values.indexOf(-0)).toBe(1);
  expect(values.indexOf(add("", "a"))).toBe(3);
  expect(values.indexOf(firstObject)).toBe(4);
  expect(values.indexOf(secondObject)).toBe(-1);
  expect(values.indexOf(null)).toBe(5);
  expect(values.indexOf(undefined)).toBe(6);
  expect(values.indexOf(add(1n, 0n))).toBe(7);
  expect(values.indexOf(divide(14, 2))).toBe(8);
  expect(values.lastIndexOf(0)).toBe(2);
  expect(Object.is(divide(0, 0), NaN)).toBe(true);
  expect(Object.is(subtract(5, 5), -0)).toBe(false);
  expect(Object.is(divide(0, -1), -0)).toBe(true);
  expect(new Set([NaN, divide(0, 0), 0, -0, "a", add("", "a"), 1n, add(1n, 0n)]).size).toBe(4);
});
