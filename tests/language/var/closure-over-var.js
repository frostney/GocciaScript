/*---
description: Closures over var declarations share the same binding
features: [compat-var]
---*/

test("closure captures var from enclosing function", () => {
  const fn = () => {
    var x = 10;
    const getter = () => x;
    x = 20;
    return getter();
  };
  expect(fn()).toBe(20);
});

test("var assignment visible to previously-created closure", () => {
  const fn = () => {
    var value = "initial";
    const getter = () => value;
    value = "updated";
    return getter();
  };
  expect(fn()).toBe("updated");
});

test("declaration-only var redeclaration does not overwrite captured cell", () => {
  const fn = () => {
    var value = "initial";
    const getter = () => value;
    value = "updated";
    var value;
    return getter();
  };
  expect(fn()).toBe("updated");
});

test("var redeclaration with undefined initializer updates captured cell", () => {
  const fn = () => {
    var value = "initial";
    const getter = () => value;
    value = "updated";
    var value = undefined;
    return getter();
  };
  expect(fn()).toBeUndefined();
});
