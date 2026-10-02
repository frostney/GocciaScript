/*---
description: |
  The bitwise and shift operators convert a number operand that is not a
  32-bit integer with ToInt32 / ToUint32 (ES2026 §7.1.7 / §7.1.8): values at
  or beyond 2^31, fractions, values beyond 2^53 and 2^63, and denormals.

  Every operand reaches its operator as a function parameter, so that constant
  folding cannot decide the result and the bytecode VM applies the operator to
  a floating-point register rather than to an int32 one. A value produced by
  integer arithmetic that leaves the int32 range takes the same path.
features: [bitwise-and, bitwise-or, bitwise-xor, bitwise-not, left-shift, right-shift, unsigned-right-shift]
---*/

const and = (left, right) => left & right;
const or = (left, right) => left | right;
const xor = (left, right) => left ^ right;
const shl = (left, right) => left << right;
const shr = (left, right) => left >> right;
const ushr = (left, right) => left >>> right;
const not = (operand) => ~operand;

test("&, | and ^ reduce an operand at or beyond 2^31 modulo 2^32", () => {
  expect(and(2147483648, 7)).toBe(0);
  expect(and(2147483655, 7)).toBe(7);
  expect(and(7, 2147483655)).toBe(7);
  expect(and(4294967301, 4294967303)).toBe(5);
  expect(or(2147483648, 7)).toBe(-2147483641);
  expect(or(4294967301, 2)).toBe(7);
  expect(or(2, 4294967301)).toBe(7);
  expect(or(-2147483649, 0)).toBe(2147483647);
  expect(xor(2147483648, 7)).toBe(-2147483641);
  expect(xor(4294967295, 1)).toBe(-2);
  expect(xor(1, 4294967295)).toBe(-2);
  expect(xor(-2147483649, -2147483649)).toBe(0);
});

test("&, | and ^ truncate a fractional operand toward zero", () => {
  expect(and(7.9, 5)).toBe(5);
  expect(and(-7.9, 5)).toBe(1);
  expect(and(5, 7.9)).toBe(5);
  expect(or(0.9, 0)).toBe(0);
  expect(or(-0.9, 0)).toBe(0);
  expect(or(1.5, 2.5)).toBe(3);
  expect(xor(1.5, 3)).toBe(2);
  expect(xor(-1.5, 3)).toBe(-4);
  expect(xor(3, -1.5)).toBe(-4);
  expect(or(4294967295.9, 0)).toBe(-1);
  expect(or(-4294967297.5, 0)).toBe(-1);
});

test("operands beyond 2^53 and 2^63 keep only their low 32 bits", () => {
  expect(or(9007199254740994, 0)).toBe(2);
  expect(and(9007199254740994, 3)).toBe(2);
  expect(or(1e+21, 0)).toBe(-559939584);
  expect(or(-1e+21, 0)).toBe(559939584);
  expect(xor(1e+21, 1)).toBe(-559939583);
  expect(or(9223372036854776000, 0)).toBe(0);
  expect(or(-9223372036854776000, 0)).toBe(0);
  expect(or(18446744073709556000, 0)).toBe(4096);
  expect(and(18446744073709556000, 4096)).toBe(4096);
  expect(or(1.7976931348623157e+308, 0)).toBe(0);
  expect(or(5e-324, 0)).toBe(0);
  expect(and(5e-324, 1)).toBe(0);
});

test("<< converts the left operand with ToInt32 and masks the count to five bits", () => {
  expect(shl(2147483648, 1)).toBe(0);
  expect(shl(4294967297, 4)).toBe(16);
  expect(shl(1.9, 31)).toBe(-2147483648);
  expect(shl(-1.9, 31)).toBe(-2147483648);
  expect(shl(3, 30.9)).toBe(-1073741824);
  expect(shl(5, 33.7)).toBe(10);
  expect(shl(5, 4294967297)).toBe(10);
  expect(shl(5, -1.5)).toBe(-2147483648);
  expect(shl(1e+21, 1)).toBe(-1119879168);
  expect(shl(0.5, 3)).toBe(0);
});

test(">> converts the left operand with ToInt32 and keeps its sign", () => {
  expect(shr(2147483648, 1)).toBe(-1073741824);
  expect(shr(4294967295, 1)).toBe(-1);
  expect(shr(-2147483649, 1)).toBe(1073741823);
  expect(shr(4294967301.5, 1)).toBe(2);
  expect(shr(-8.9, 1)).toBe(-4);
  expect(shr(8, 1.9)).toBe(4);
  expect(shr(8, 33.7)).toBe(4);
  expect(shr(8, 4294967297)).toBe(4);
  expect(shr(-8, -1.5)).toBe(-1);
  expect(shr(1e+21, 4)).toBe(-34996224);
});

test(">>> converts the left operand with ToUint32 and can exceed the int32 range", () => {
  expect(ushr(2147483648, 0)).toBe(2147483648);
  expect(ushr(4294967295, 0)).toBe(4294967295);
  expect(ushr(-1.5, 0)).toBe(4294967295);
  expect(ushr(-2147483649, 0)).toBe(2147483647);
  expect(ushr(4294967301.5, 1)).toBe(2);
  expect(ushr(-8.9, 28)).toBe(15);
  expect(ushr(8, 33.7)).toBe(4);
  expect(ushr(-1, 4294967297)).toBe(2147483647);
  expect(ushr(-1, 31.9)).toBe(1);
  expect(ushr(1e+21, 4)).toBe(233439232);
  expect(ushr(0.5, 0)).toBe(0);
});

test("~ converts its operand with ToInt32", () => {
  expect(not(2147483648)).toBe(2147483647);
  expect(not(4294967295)).toBe(0);
  expect(not(4294967301.5)).toBe(-6);
  expect(not(-2147483649)).toBe(-2147483648);
  expect(not(1.9)).toBe(-2);
  expect(not(-1.9)).toBe(0);
  expect(not(0.5)).toBe(-1);
  expect(not(1e+21)).toBe(559939583);
  expect(not(9007199254740994)).toBe(-3);
  expect(not(9223372036854776000)).toBe(-1);
  expect(not(5e-324)).toBe(-1);
});

test("an integer sum that leaves the int32 range is still a valid operand", () => {
  // 2147483647 + 1 no longer fits an int32, so the sum is held as a double.
  const sum = (left, right) => left + right;
  const product = (left, right) => left * right;
  expect(sum(2147483647, 1) & 7).toBe(0);
  expect(sum(2147483647, 1) | 0).toBe(-2147483648);
  expect(sum(2147483647, 9) & 15).toBe(8);
  expect(sum(-2147483648, -1) | 0).toBe(2147483647);
  expect(product(65536, 65537) ^ 3).toBe(65539);
  expect(product(65536, 65537) >>> 1).toBe(32768);
  expect(~sum(4294967295, 1)).toBe(-1);
  expect(sum(2147483647, 1) << 1).toBe(0);
  expect(sum(2147483647, 1) >> 31).toBe(-1);
});

test("a running total keeps its low bits after it outgrows int32", () => {
  let total = 2147483000;
  let low = 0;
  for (const step of Array.from({ length: 2000 }, (unused, index) => index)) {
    total = total + step;
    low = low ^ (total & 255);
  }
  expect(total).toBe(2149482000);
  expect(low).toBe(48);
});

test("a mixed int32 and non-int32 pair gives the same result in either order", () => {
  expect(and(4294967303, 5)).toBe(and(5, 4294967303));
  expect(or(2147483648.5, 1)).toBe(or(1, 2147483648.5));
  expect(xor(-2147483649.5, 6)).toBe(xor(6, -2147483649.5));
});

test("NaN, the infinities and negative zero convert to 0", () => {
  expect(or(NaN, 0)).toBe(0);
  expect(or(Infinity, 0)).toBe(0);
  expect(or(-Infinity, 0)).toBe(0);
  expect(or(-0, 0)).toBe(0);
  expect(Object.is(or(-0, -0), 0)).toBe(true);
  expect(and(NaN, 7)).toBe(0);
  expect(xor(Infinity, 7)).toBe(7);
  expect(shl(NaN, 3)).toBe(0);
  expect(shl(3, NaN)).toBe(3);
  expect(shr(-Infinity, 1)).toBe(0);
  expect(ushr(-0, 0)).toBe(0);
  expect(not(NaN)).toBe(-1);
  expect(not(-0)).toBe(-1);
  // A division produces these at run time instead of loading a constant.
  const divide = (left, right) => left / right;
  expect(or(divide(0, 0), 0)).toBe(0);
  expect(or(divide(1, 0), 0)).toBe(0);
  expect(and(divide(-1, 0), 7)).toBe(0);
  expect(not(divide(0, 0))).toBe(-1);
  expect(Object.is(or(divide(-0.5, 1e308) * 0, 0), 0)).toBe(true);
});
