/*---
description: Under strict types a return-type annotation is still not enforced, so a call's result is typed by its value and checked where it reaches an enforced binding
features: [types-as-comments, strict-type-enforcement]
---*/

// Return-type annotations are not enforced in either mode (#1276). A binding
// initialized from a call takes no inferred type, and an enforced binding
// checks the value the call actually returns.

describe("call result under strict types", () => {
  const text = (): number => "s";

  test("arithmetic on the result follows the returned value", () => {
    const local = (): number => "s";
    expect(text() + 1).toBe("s1");
    expect(local() + 1).toBe("s1");
    expect(text() + text()).toBe("ss");
    expect(text() + 0.5).toBe("s0.5");
  });

  test("a const or let initialized from the call is not typed by the annotation", () => {
    const viaConst = () => {
      const result = text();
      return result + 1;
    };
    const viaLet = () => {
      let result = text();
      return result + 1;
    };
    const viaCompound = () => {
      let result = text();
      result += 1;
      return result;
    };
    const viaClosure = () => {
      const result = text();
      const read = () => result + 1;
      return read();
    };
    const viaIncrement = () => {
      const numeric = (): number => "2";
      let result = numeric();
      result++;
      return result;
    };
    expect(viaConst()).toBe("s1");
    expect(viaLet()).toBe("s1");
    expect(viaCompound()).toBe("s1");
    expect(viaClosure()).toBe("s1");
    expect(viaIncrement()).toBe(3);
  });

  test("an enforced binding checks a result combined into it", () => {
    const addFrom = () => {
      let total = 1;
      total += text();
      return total;
    };
    const assignSum = () => {
      let total: number = 0;
      total = total + text();
      return total;
    };
    expect(addFrom).toThrow(TypeError);
    expect(assignSum).toThrow(TypeError);
  });

  test(": string with a number result keeps the string conversion", () => {
    const five = (): string => 5;
    expect(typeof (five() + "")).toBe("string");
    expect("" + five()).toBe("5");
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
