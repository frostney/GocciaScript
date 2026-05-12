/*---
description: Loose equality operators are available with --compat-loose-equality
features: [compat-loose-equality]
---*/

describe("loose equality", () => {
  test("same-type comparisons match strict equality", () => {
    expect(1 == 1).toBe(true);
    expect(1 != 1).toBe(false);
    expect("a" == "a").toBe(true);
    expect(NaN == NaN).toBe(false);
    expect(0 == -0).toBe(true);
  });

  test("null and undefined compare equal only to each other", () => {
    expect(null == undefined).toBe(true);
    expect(undefined == null).toBe(true);
    expect(null != undefined).toBe(false);
    expect(null == 0).toBe(false);
    expect(undefined == 0).toBe(false);
  });

  test("strings and booleans coerce through numbers", () => {
    expect("1" == 1).toBe(true);
    expect(1 == "1").toBe(true);
    expect("" == 0).toBe(true);
    expect(false == "").toBe(true);
    expect(true == "1").toBe(true);
    expect(true != "1").toBe(false);
  });

  test("BigInt compares loosely with strings and finite integral numbers", () => {
    expect(1n == "1").toBe(true);
    expect("1" == 1n).toBe(true);
    expect(1n == 1).toBe(true);
    expect(1n == 1.1).toBe(false);
    expect(1n == Infinity).toBe(false);
    expect(10n != "11").toBe(true);
  });

  test("objects use ToPrimitive with the default hint", () => {
    const hints = [];
    const obj = {
      [Symbol.toPrimitive](hint) {
        hints.push(hint);
        return "7";
      },
    };

    expect(7 == obj).toBe(true);
    expect(obj != 7).toBe(false);
    expect(hints).toEqual(["default", "default"]);
  });

  test("String.prototype compares as the empty string", () => {
    expect(String.prototype == "").toBe(true);
    expect(String.prototype != "").toBe(false);
  });

  test("arrays and valueOf results compare through ToPrimitive", () => {
    const boxed = { valueOf() { return 42; } };

    expect([1] == "1").toBe(true);
    expect(boxed == 42).toBe(true);
    expect({} == "[object Object]").toBe(true);
  });

  test("template interpolation inherits the compatibility flag", () => {
    expect(`${"1" == 1}`).toBe("true");
    expect(`${"1" != 1}`).toBe("false");
  });
});
