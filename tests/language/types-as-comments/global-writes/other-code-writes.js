/*---
description: Under strict types, code compiled apart from a Script (a Function body) is checked when it writes the Script's typed top-level let or var
features: [types-as-comments, strict-type-enforcement, compat-var, Function]
---*/

let text: string = "start";
let anything = text;
const fixed: number = 1;
var count: number = 1;

describe("writes from a Function body to typed top-level bindings", () => {
  test("a typed let rejects a value of another type and keeps its value", () => {
    expect(() => Function("text = 5")()).toThrow(TypeError);
    expect(text).toBe("start");
    Function("text = 'next'")();
    expect(text).toBe("next");
  });

  test("a typed var rejects a value of another type and keeps its value", () => {
    expect(() => Function("count = 'text'")()).toThrow(TypeError);
    expect(count).toBe(1);
    Function("count = 2")();
    expect(count).toBe(2);
  });

  test("an untyped let takes any value", () => {
    Function("anything = 5")();
    expect(anything).toBe(5);
  });

  test("a const still refuses every store", () => {
    expect(() => Function("fixed = 2")()).toThrow(TypeError);
    expect(fixed).toBe(1);
  });

  test("a repeated store from a hot Function body is still checked", () => {
    const setText = Function("value", "text = value");
    const setCount = Function("value", "count = value");
    for (const step of Array.from({ length: 5000 }, (_, index) => index)) {
      setText("loop");
      setCount(step);
    }
    expect(() => setText(5)).toThrow(TypeError);
    expect(() => setCount("text")).toThrow(TypeError);
    expect([text, count]).toEqual(["loop", 4999]);
  });
});
