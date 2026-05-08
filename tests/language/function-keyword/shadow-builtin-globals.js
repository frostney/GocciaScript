/*---
description: function declarations may shadow built-in globals in script mode (§16.1.7)
features: [compat-function, compat-var]
---*/

test("function declaration shadows built-in Array", () => {
  function Array() { return "custom"; }
  expect(typeof Array).toBe("function");
  expect(Array()).toBe("custom");
});

test("function declaration shadows built-in Object", () => {
  function Object() { return "custom-object"; }
  expect(typeof Object).toBe("function");
  expect(Object()).toBe("custom-object");
});

test("function declaration shadows built-in Error", () => {
  function Error() { return "custom-error"; }
  expect(typeof Error).toBe("function");
  expect(Error()).toBe("custom-error");
});
