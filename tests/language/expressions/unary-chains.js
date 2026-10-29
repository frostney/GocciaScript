/*---
description: A chain of unary operators applies each operator in turn to the value of the one inside it
features: [let, const, arrow-functions, unary-operators]
---*/

// ES2026 §13.5 Unary Operators: each operator evaluates its operand, the
// next operator of the chain, first. Each helper takes its input as an
// argument, so that only the parts the tests name can be folded at compile
// time.

const opaque = (value) => value;

test("of negations negates once per operator", () => {
  const odd = (p) => - - - - - p;
  const even = (p) => - - - - p;
  expect(odd(opaque(2))).toBe(-2);
  expect(even(opaque(2))).toBe(2);
  expect(odd(opaque("3"))).toBe(-3);
  expect(Object.is(even(opaque(-0)), -0)).toBe(true);
});

test("of logical nots converts to a Boolean once", () => {
  const odd = (p) => !!!!!p;
  const even = (p) => !!!!p;
  expect(odd(opaque(0))).toBe(true);
  expect(even(opaque("a"))).toBe(true);
  expect(even(opaque(""))).toBe(false);
});

test("of bitwise nots converts to an int32", () => {
  const even = (p) => ~~~~p;
  expect(even(opaque(2.7))).toBe(2);
  expect(even(opaque(2 ** 32 + 5))).toBe(5);
  expect(even(opaque(10n))).toBe(10n);
});

test("of typeof, void and unary plus", () => {
  const types = (p) => typeof typeof typeof p;
  const voids = (p) => void void void p;
  const pluses = (p) => + + + p;
  expect(types(opaque(1))).toBe("string");
  expect(voids(opaque(1))).toBeUndefined();
  expect(pluses(opaque("4"))).toBe(4);
});

test("folds a constant operand inside it", () => {
  const inner = (p) => -(p + -(2 + 3));
  const mixed = (p) => !(1 < !(2 < !p));
  expect(inner(opaque(1))).toBe(4);
  expect(mixed(opaque(0))).toBe(true);
});

test("reads a binding an operand inside it assigns", () => {
  const run = (p) => {
    let x = 2;
    return - - -((x = p) + - - x);
  };
  expect(run(opaque(3))).toBe(-6);
});

test("alternating binary and unary operands preserve Number and BigInt results", () => {
  const run = (p) => p + -(p + -(p + -p));
  expect(run(opaque(3))).toBe(0);
  expect(run(opaque(3n))).toBe(0n);
});

test("a unary operand keeps its value when a later operand changes its binding", () => {
  const run = (start) => {
    let x = start;
    return -(x + 1) + (x = "s", 2) + x;
  };
  expect(run(opaque(1))).toBe("0s");
});
