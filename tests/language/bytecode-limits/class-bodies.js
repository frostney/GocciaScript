/*---
description: A class body may hold more than 65,535 constants and more than 65,536 functions
features: [ShadowRealm]
---*/

const realm = new ShadowRealm();
const list = (count, item, separator = ", ") =>
  Array.from({ length: count }, (_, i) => item(i)).join(separator);

describe("class body limits", () => {
  // Neither a function's constants nor the functions defined directly in it
  // are limited to 16-bit counts: these class bodies compile and run.
  test("65,600 distinct field names evaluate", () => {
    expect(realm.evaluate(`const C = class { ${list(65600, (i) => `f${i} = ${i};`, " ")} }; const c = new C(); Object.keys(c).length + ":" + c.f65599`))
      .toBe("65600:65599");
  });

  test("65,537 methods of one name evaluate, the last definition winning", () => {
    expect(realm.evaluate(`const C = class { ${list(65537, (i) => `m() { return ${i}; }`, " ")} }; new C().m()`)).toBe(65536);
  });
});
