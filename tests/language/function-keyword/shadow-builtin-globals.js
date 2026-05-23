/*---
description: function declarations may shadow built-in globals in script source (§16.1.7)
features: [compat-function, compat-var]
---*/

function Array() { return "custom"; }
function Object() { return "custom-object"; }
function Error() { return "custom-error"; }

test("top-level function declaration shadows built-in Array", () => {
  expect(typeof Array).toBe("function");
  expect(Array()).toBe("custom");
});

test("top-level function declaration shadows built-in Object", () => {
  expect(typeof Object).toBe("function");
  expect(Object()).toBe("custom-object");
});

test("top-level function declaration shadows built-in Error", () => {
  expect(typeof Error).toBe("function");
  expect(Error()).toBe("custom-error");
});
