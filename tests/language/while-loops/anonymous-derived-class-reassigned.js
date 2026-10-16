/*---
description: A while-loop body that reassigns a binding to an anonymous class extending it leaves the binding intact for closures that outlive the function
features: [compat-while-loops, classes]
---*/

test("the class assigned in a while-loop body constructs after the function returns", () => {
  const make = () => {
    let C = class {};
    const first = C;
    let i = 0;
    while (i < 2) {
      C = class extends C {};
      i++;
    }
    const last = C;
    return () => [C === last, typeof new C(),
      Object.getPrototypeOf(Object.getPrototypeOf(C)) === first, i];
  };
  expect(make()()).toEqual([true, "object", true, 2]);
});

test("the class assigned in a do...while body constructs after the function returns", () => {
  const make = () => {
    let C = class {};
    let n = 0;
    do {
      C = class extends C {};
      n++;
    } while (n < 1);
    const after = "after";
    return () => [typeof new C(), n, after];
  };
  expect(make()()).toEqual(["object", 1, "after"]);
});
