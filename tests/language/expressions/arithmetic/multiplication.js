/*---
description: Multiplication operator works correctly
features: [multiplication-operator]
---*/

test("multiplication operator", () => {
  expect(4 * 5).toBe(20);
  expect(2.5 * 4).toBe(10);
  expect(-3 * 7).toBe(-21);
  expect(0 * 100).toBe(0);
});

describe("sign of a zero product", () => {
  // Operands pass through a function so the compiler cannot fold the product.
  const id = (value) => value;

  test("an integer zero times a negative integer is -0", () => {
    const zero = id(0);
    const minusOne = id(-1);
    expect(Object.is(zero * minusOne, -0)).toBe(true);
    expect(Object.is(minusOne * zero, -0)).toBe(true);
    expect(Object.is(zero * id(-2147483648), -0)).toBe(true);
    expect(1 / (zero * minusOne)).toBe(-Infinity);
  });

  test("a zero product of two non-negative or two zero integers is +0", () => {
    const zero = id(0);
    expect(Object.is(zero * id(1), 0)).toBe(true);
    expect(Object.is(zero * zero, 0)).toBe(true);
    expect(Object.is(id(7) * zero, 0)).toBe(true);
  });

  test("a non-zero integer product keeps its value and sign", () => {
    expect(id(-3) * id(7)).toBe(-21);
    expect(id(-3) * id(-7)).toBe(21);
    expect(id(65536) * id(-65536)).toBe(-4294967296);
  });

  test("locals assigned from integer arithmetic give -0 as well", () => {
    let left = 0;
    let right = 1;
    right = right - 2;
    expect(Object.is(left * right, -0)).toBe(true);
    expect(Object.is(right * left, -0)).toBe(true);
    left = left + 3;
    expect(left * right).toBe(-3);
  });

  test("compound multiplication gives -0", () => {
    let value = id(0);
    value *= id(-1);
    expect(Object.is(value, -0)).toBe(true);
  });

  test("a constant expression gives -0", () => {
    expect(Object.is(0 * -1, -0)).toBe(true);
    expect(Object.is(-1 * 0, -0)).toBe(true);
  });

  test("mixed integer and fractional operands give -0", () => {
    expect(Object.is(id(0) * id(-1.5), -0)).toBe(true);
  });
});
