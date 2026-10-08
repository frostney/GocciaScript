/*---
description: A block-level function named like a let, const or class binding of its function stays block-scoped
features: [compat-function, compat-non-strict-mode, compat-var]
---*/

test("a block function leaves a let of the function body alone", () => {
  function f() {
    let a = 1;
    {
      function a() {}
    }
    return a;
  }

  expect(f()).toBe(1);
});

test("a block function leaves a const of the function body alone", () => {
  function f() {
    const a = 1;
    {
      function a() {}
    }
    switch (1) {
      case 1:
        function a() {}
    }
    return a;
  }

  expect(f()).toBe(1);
});

test("a block function leaves a class of the function body alone", () => {
  function f() {
    class A {
      static kind = "class";
    }
    {
      function A() {}
    }
    return A.kind;
  }

  expect(f()).toBe("class");
});

test("a block function leaves a let of an enclosing block alone", () => {
  function f() {
    let seen;
    {
      let a = 1;
      {
        function a() {}
      }
      seen = a;
    }
    return seen;
  }

  expect(f()).toBe(1);
});

test("under a plain catch parameter the block function still updates its var binding", () => {
  function f() {
    try {
      throw 1;
    } catch (a) {
      {
        function a() {
          return 2;
        }
      }
    }
    return a();
  }

  expect(f()).toBe(2);
});
