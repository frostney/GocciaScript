/*---
description: Addition operator with ToPrimitive conversion for objects
features: [addition, ToPrimitive]
---*/

describe("addition with ToPrimitive", () => {
  test("object with valueOf in addition", () => {
    const obj = { valueOf: () => 42 };
    expect(obj + 1).toBe(43);
    expect(1 + obj).toBe(43);
  });

  test("object with toString in addition", () => {
    const obj = { toString: () => "hello" };
    expect(obj + " world").toBe("hello world");
    expect("say " + obj).toBe("say hello");
  });

  test("array concatenation produces string", () => {
    expect([1, 2] + [3, 4]).toBe("1,23,4");
  });

  test("null and undefined in addition", () => {
    expect(1 + null).toBe(1);
    expect(1 + undefined).toBe(NaN);
  });

  test("boolean in addition", () => {
    expect(true + 1).toBe(2);
    expect(false + 1).toBe(1);
    expect(true + true).toBe(2);
  });

  test("Symbol.toPrimitive takes precedence over valueOf and toString", () => {
    const obj = {
      valueOf() { return "valueOf"; },
      toString() { return "toString"; },
      [Symbol.toPrimitive](hint) { return "@@toPrimitive(" + hint + ")"; },
    };
    expect(obj + "").toBe("@@toPrimitive(default)");
    expect("" + obj).toBe("@@toPrimitive(default)");
  });

  test("Symbol.toPrimitive receives 'default' hint for addition", () => {
    const hints = [];
    const obj = {
      [Symbol.toPrimitive](hint) {
        hints.push(hint);
        return 0;
      },
    };
    obj + 1;
    expect(hints[0]).toBe("default");
  });

  test("Symbol.toPrimitive returning object throws TypeError", () => {
    const obj = {
      [Symbol.toPrimitive]() { return {}; },
    };
    expect(() => { obj + 1; }).toThrow(TypeError);
  });

  test("left primitive remains alive while the right operand triggers GC", () => {
    const left = {
      [Symbol.toPrimitive]() {
        return String.fromCharCode(108, 101, 102, 116);
      },
    };
    const right = {
      [Symbol.toPrimitive]() {
        Goccia.gc();
        return "right";
      },
    };
    expect(left + right).toBe("leftright");
  });
});
