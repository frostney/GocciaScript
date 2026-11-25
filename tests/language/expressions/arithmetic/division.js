/*---
description: Division operator works correctly
features: [division-operator]
---*/

describe("division basics", () => {
  test("division operator", () => {
    expect(15 / 3).toBe(5);
    expect(7 / 2).toBe(3.5);
    expect(-10 / 2).toBe(-5);
    expect(0 / 5).toBe(0);
  });

  test("object and class expression closing braces are division left operands", () => {
    expect(Number.isNaN(({} / 2))).toBe(true);
    expect(Number.isNaN((class {} / 2))).toBe(true);
  });

  test("keyword property names are division left operands after member access", () => {
    const obj = { if: 8, while: 10 };

    expect(obj.if / 2).toBe(4);
    expect(obj?.while / 2).toBe(5);
  });

  test("parenthesized expressions remain division left operands after arrow lookahead", () => {
    let x = 0;

    expect((x = 1) / x).toBe(1);
    x = 0;
    expect(x / (x = 1)).toBe(0);
    x = 0;
    expect(((x = 1) / x)).toBe(1);
    x = 0;
    expect("value: " + ((x = 1) / x)).toBe("value: 1");
  });
});

describe("division by zero", () => {
  test("positive divided by zero is Infinity", () => {
    expect(1 / 0).toBe(Infinity);
    expect(42 / 0).toBe(Infinity);
  });

  test("negative divided by zero is -Infinity", () => {
    expect(-1 / 0).toBe(-Infinity);
    expect(-42 / 0).toBe(-Infinity);
  });

  test("zero divided by zero is NaN", () => {
    expect(Number.isNaN(0 / 0)).toBe(true);
  });
});

describe("division with Infinity", () => {
  test("Infinity divided by Infinity is NaN", () => {
    expect(Number.isNaN(Infinity / Infinity)).toBe(true);
    expect(Number.isNaN(-Infinity / -Infinity)).toBe(true);
    expect(Number.isNaN(Infinity / -Infinity)).toBe(true);
  });

  test("Infinity divided by positive is Infinity", () => {
    expect(Infinity / 1).toBe(Infinity);
    expect(Infinity / 100).toBe(Infinity);
  });

  test("Infinity divided by negative is -Infinity", () => {
    expect(Infinity / -1).toBe(-Infinity);
    expect(-Infinity / 1).toBe(-Infinity);
  });

  test("-Infinity divided by negative is Infinity", () => {
    expect(-Infinity / -1).toBe(Infinity);
    expect(-Infinity / -100).toBe(Infinity);
  });

  test("finite divided by Infinity is zero", () => {
    expect(1 / Infinity).toBe(0);
    expect(100 / Infinity).toBe(0);
  });

  test("finite divided by -Infinity respects sign", () => {
    const posOverNegInf = 1 / -Infinity;
    expect(posOverNegInf === 0).toBe(true);
    expect(1 / posOverNegInf).toBe(-Infinity);

    const negOverNegInf = -1 / -Infinity;
    expect(negOverNegInf === 0).toBe(true);
    expect(1 / negOverNegInf).toBe(Infinity);
  });
});

describe("division with NaN", () => {
  test("NaN in division produces NaN", () => {
    expect(Number.isNaN(NaN / 1)).toBe(true);
    expect(Number.isNaN(1 / NaN)).toBe(true);
    expect(Number.isNaN(NaN / NaN)).toBe(true);
  });
});

describe("a subnormal quotient", () => {
  // x87 rounded each of these quotients twice and came out one bit off.
  const id = (value) => value;
  const quotients = [
    [2.0996648512956365e-308, 1.1, 1.9087862284505787e-308],
    [1.134375790418231e-308, 0.7, 1.6205368434546157e-308],
    [1.2349373183628205e-308, 3.3, 3.742234298069156e-309],
  ];

  test.each(quotients)("%s / %s rounds once to %s", (a, b, expected) => {
    expect(id(a) / id(b)).toBe(expected);
  });

  test("constant operands fold to the quotient rounded once", () => {
    expect(1.2349373183628205e-308 / 3.3).toBe(3.742234298069156e-309);
  });
});
