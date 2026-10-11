/*---
description: In function declarations, function expressions and generator functions a parameter is in its temporal dead zone while its own default initializer runs, so assigning or reading it there throws ReferenceError
features: [compat-function, default-parameters, destructuring, temporal-dead-zone, generators]
---*/

describe("default parameter initializer of function-keyword functions", () => {
  test("declarations and expressions", () => {
    function assign(c = (c = 7)) {
      return c;
    }
    function build(c = [c]) {
      return c;
    }
    function destructure(c = ([c] = [7])) {
      return c;
    }
    expect(() => assign()).toThrow(ReferenceError);
    expect(() => build()).toThrow(ReferenceError);
    expect(() => destructure()).toThrow(ReferenceError);
    expect(() => (function (c = { v: c }) {
      return c;
    })()).toThrow(ReferenceError);
    expect(() => (function (c = (function () {
      c = 7;
      return 1;
    })()) {
      return c;
    })()).toThrow(ReferenceError);
    expect(assign(3)).toBe(3);
    expect(build(4)).toBe(4);
  });

  test("a generator throws when it is called", () => {
    function* gen(c = (c = 7)) {
      yield c;
    }
    expect(() => gen()).toThrow(ReferenceError);
    expect(gen(2).next().value).toBe(2);
  });

  test("a constructor called with new", () => {
    function F(c = 1 && c) {
      this.c = c;
    }
    expect(() => new F()).toThrow(ReferenceError);
    expect(new F(5).c).toBe(5);
  });
});
