/*---
description: A var initializer that applies a postfix update to its own binding stores the update, then initializes the binding with the old value
features: [compat-var, update-expressions]
---*/

var topLevelCounter = 5;
var topLevelCounter = topLevelCounter++;
var topLevelFresh = topLevelFresh--;

describe("var initializer that updates its own binding", () => {
  test("postfix ++ and -- leave the old value", () => {
    var up = 5;
    var up = up++;
    expect(up).toBe(5);
    var down = 5;
    var down = down--;
    expect(down).toBe(5);
  });

  test("prefix ++ and -- leave the new value", () => {
    var up = 5;
    var up = ++up;
    expect(up).toBe(6);
    var down = 5;
    var down = --down;
    expect(down).toBe(4);
  });

  test("an uninitialized var converts undefined to NaN", () => {
    var fresh = fresh++;
    expect(fresh).toBeNaN();
  });

  test("the old value is converted with ToNumeric", () => {
    var text = "5";
    var text = text++;
    expect(text).toBe(5);
    var big = 5n;
    var big = big--;
    expect(big).toBe(5n);
    var object = { valueOf: () => 4 };
    var object = object++;
    expect(object).toBe(4);
  });

  test("the update inside a comma or conditional expression", () => {
    var comma = 5;
    var comma = (0, comma++);
    expect(comma).toBe(5);
    var conditional = 5;
    var conditional = true ? conditional-- : 0;
    expect(conditional).toBe(5);
  });

  test("a closure sees the binding's final value", () => {
    var captured = 5;
    const read = () => captured;
    var captured = captured++;
    expect(read()).toBe(5);
    expect(captured).toBe(5);
  });

  test("a top-level var", () => {
    expect(topLevelCounter).toBe(5);
    expect(topLevelFresh).toBeNaN();
  });
});
