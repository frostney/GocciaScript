/*---
description: top-level var declarations may shadow built-in globals in script mode (§16.1.7)
features: [compat-var]
---*/

var NaN;
var Infinity;
var undefined;

test("top-level var NaN without initializer preserves the built-in value", () => {
  expect(typeof NaN).toBe("number");
});

test("top-level var Infinity without initializer preserves the built-in value", () => {
  expect(typeof Infinity).toBe("number");
});

test("top-level var undefined without initializer preserves the built-in value", () => {
  expect(typeof undefined).toBe("undefined");
});

var Array = "shadowed";

test("top-level var Array with initializer shadows the constructor", () => {
  expect(Array).toBe("shadowed");
});

test("var inside a function creates a local binding, does not touch the global", () => {
  const fn = () => {
    var Map = "local";
    return Map;
  };
  expect(fn()).toBe("local");
  expect(typeof Map).toBe("function");
});
