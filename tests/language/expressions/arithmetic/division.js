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
