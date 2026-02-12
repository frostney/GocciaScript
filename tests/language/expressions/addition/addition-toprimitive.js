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
});
