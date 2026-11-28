/*---
description: Under strict types, code compiled apart from a Script (a Function body) is checked when it writes the Script's typed top-level let or var
features: [types-as-comments, strict-type-enforcement, compat-var, Function]
---*/

let text: string = "start";
let anything = text;
const fixed: number = 1;
var count: number = 1;

const setText = Function("value", "text = value");
const setCount = Function("value", "count = value");
const setAnything = Function("value", "anything = value");
const setFixed = Function("value", "fixed = value");

describe("writes from a Function body to typed top-level bindings", () => {
  test("a typed let rejects a value of another type and keeps its value", () => {
    expect(() => setText(5)).toThrow(TypeError);
    expect(text).toBe("start");
    setText("next");
    expect(text).toBe("next");
  });

  test("a typed var rejects a value of another type and keeps its value", () => {
    expect(() => setCount("text")).toThrow(TypeError);
    expect(count).toBe(1);
    setCount(2);
    expect(count).toBe(2);
  });

  test("an untyped let takes any value", () => {
    setAnything(5);
    expect(anything).toBe(5);
  });

  test("a const still refuses every store", () => {
    expect(() => setFixed(2)).toThrow(TypeError);
    expect(fixed).toBe(1);
  });

  test("a repeated store from a hot Function body is still checked", () => {
    for (const step of Array.from({ length: 5000 }, (_, index) => index)) {
      setText("loop");
      setCount(step);
    }
    expect(() => setText(5)).toThrow(TypeError);
    expect(() => setCount("text")).toThrow(TypeError);
    expect([text, count]).toEqual(["loop", 4999]);
  });
});
