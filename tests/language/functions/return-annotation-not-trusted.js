/*---
description: A return-type annotation does not decide the type of a call's result, so arithmetic on it follows the value the function returns
features: [types-as-comments]
---*/

// Annotations have no runtime effect (TC39 types-as-comments), and return
// types are not enforced even under --strict-types (#1276). ES2026 §13.15.3
// ApplyStringOrNumericBinaryOperator then decides each result from the
// values the calls actually return.

describe("call to an arrow annotated : number that returns something else", () => {
  const text = (): number => "s";
  const digits = (): number => "7";
  const boxed = (): number => ({ valueOf() { return 10; } });
  const nothing = (): number => undefined;
  const big = (): number => 5n;

  test("adding a small integer concatenates a string result", () => {
    expect(text() + 1).toBe("s1");
    expect(1 + text()).toBe("1s");
  });

  test("subtracting a small integer converts a numeric string", () => {
    expect(digits() - 1).toBe(6);
  });

  test("an object result goes through valueOf", () => {
    expect(boxed() + 1).toBe(11);
  });

  test("an undefined result gives NaN", () => {
    expect(nothing() + 1).toBeNaN();
  });

  test("a BigInt result mixed with a Number throws TypeError", () => {
    expect(() => big() + 1).toThrow(TypeError);
  });

  test("adding two results or a fraction concatenates", () => {
    expect(text() + text()).toBe("ss");
    expect(text() + 0.5).toBe("s0.5");
  });

  test("a call to an arrow declared in the same function", () => {
    const local = (): number => "s";
    expect(local() + 1).toBe("s1");
    expect(local() + local()).toBe("ss");
  });

  test("a call through a captured binding", () => {
    const inner = () => text() + 1;
    expect(inner()).toBe("s1");
  });

  test("a call in a conditional, a sequence or a template", () => {
    const pick = (flag) => (flag ? text() : 1) + 1;
    expect(pick(true)).toBe("s1");
    expect((0, text()) + 1).toBe("s1");
    expect(`${text() + 1}`).toBe("s1");
  });

  test("a const or let initialized from the call", () => {
    const viaConst = () => {
      const result = text();
      return result + 1;
    };
    const viaLet = () => {
      let result = text();
      return result + 1;
    };
    const viaClosure = () => {
      const result = text();
      const read = () => result + 1;
      return read();
    };
    expect(viaConst()).toBe("s1");
    expect(viaLet()).toBe("s1");
    expect(viaClosure()).toBe("s1");
  });

  test("compound and plain assignment from the call", () => {
    const addTo = () => {
      let result = text();
      result += 1;
      return result;
    };
    const addFrom = () => {
      let result = 1;
      result += text();
      return result;
    };
    const assign = () => {
      let result = 0;
      result = text();
      return result + 1;
    };
    expect(addTo()).toBe("s1");
    expect(addFrom()).toBe("1s");
    expect(assign()).toBe("s1");
  });

  test("a recursive arrow whose base case returns a string", () => {
    const join = (n: number): number => n <= 1 ? "s" : join(n - 1) + join(n - 2);
    expect(join(3)).toBe("sss");
  });

  test("an async arrow returns a promise whatever its annotation", () => {
    const later = async (): number => 1;
    expect(typeof (later() + 1)).toBe("string");
  });
});

describe("call to an arrow annotated with another primitive type", () => {
  test(": string with a number result keeps the string conversion", () => {
    const five = (): string => 5;
    expect(typeof (five() + "")).toBe("string");
    expect(five() + "").toBe("5");
  });

  test(": boolean with a number result keeps the boolean conversion", () => {
    const one = (): boolean => 1;
    expect(!!one()).toBe(true);
  });

  test(": number with a string result keeps the exponent conversion", () => {
    const three = (): number => "3";
    expect(three() ** 1).toBe(3);
  });
});
