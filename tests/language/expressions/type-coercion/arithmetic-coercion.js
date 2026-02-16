/*---
description: Type coercion in arithmetic operators
features: [type-coercion, arithmetic]
---*/

describe("addition coercion", () => {
  test("string + number concatenates", () => {
    expect("hello" + 1).toBe("hello1");
    expect(1 + "hello").toBe("1hello");
  });

  test("string + boolean concatenates", () => {
    expect("val:" + true).toBe("val:true");
    expect("val:" + false).toBe("val:false");
  });

  test("string + null concatenates", () => {
    expect("val:" + null).toBe("val:null");
  });

  test("string + undefined concatenates", () => {
    expect("val:" + undefined).toBe("val:undefined");
  });

  test("number + boolean coerces boolean to number", () => {
    expect(1 + true).toBe(2);
    expect(1 + false).toBe(1);
    expect(0 + true).toBe(1);
  });

  test("number + null coerces null to 0", () => {
    expect(1 + null).toBe(1);
    expect(0 + null).toBe(0);
  });

  test("number + undefined is NaN", () => {
    expect(Number.isNaN(1 + undefined)).toBe(true);
  });

  test("boolean + boolean coerces to numbers", () => {
    expect(true + true).toBe(2);
    expect(true + false).toBe(1);
    expect(false + false).toBe(0);
  });
});

describe("subtraction coercion", () => {
  test("string - number coerces string", () => {
    expect("5" - 2).toBe(3);
    expect("10" - "3").toBe(7);
  });

  test("non-numeric string - number is NaN", () => {
    expect(Number.isNaN("abc" - 1)).toBe(true);
  });

  test("boolean subtraction", () => {
    expect(true - false).toBe(1);
    expect(true - true).toBe(0);
  });

  test("null in subtraction", () => {
    expect(5 - null).toBe(5);
    expect(null - 5).toBe(-5);
  });
});

describe("multiplication coercion", () => {
  test("string * number coerces string", () => {
    expect("3" * 4).toBe(12);
    expect("2" * "5").toBe(10);
  });

  test("boolean multiplication", () => {
    expect(true * 5).toBe(5);
    expect(false * 5).toBe(0);
  });

  test("null multiplication", () => {
    expect(null * 5).toBe(0);
  });

  test("undefined multiplication is NaN", () => {
    expect(Number.isNaN(undefined * 5)).toBe(true);
  });
});

describe("division coercion", () => {
  test("string / number coerces string", () => {
    expect("10" / 2).toBe(5);
    expect("9" / "3").toBe(3);
  });

  test("boolean division", () => {
    expect(true / 2).toBe(0.5);
  });

  test("division by zero", () => {
    expect(1 / 0).toBe(Infinity);
    expect(-1 / 0).toBe(-Infinity);
  });

  test("zero divided by zero is NaN", () => {
    expect(Number.isNaN(0 / 0)).toBe(true);
  });
});

describe("modulo coercion", () => {
  test("string modulo coerces", () => {
    expect("10" % 3).toBe(1);
    expect("7" % "2").toBe(1);
  });

  test("boolean modulo", () => {
    expect(true % 2).toBe(1);
  });
});
