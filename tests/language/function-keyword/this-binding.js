/*---
description: Function declarations have call-site this binding
features: [compat-function]
---*/

test("function gets this from call-site object", () => {
  function getName() {
    return this.name;
  }
  const obj = { name: "test", getName };
  expect(obj.getName()).toBe("test");
});

test("function this is undefined when called standalone", () => {
  function getThis() {
    return this;
  }
  expect(getThis()).toBeUndefined();
});
