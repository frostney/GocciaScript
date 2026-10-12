/*---
description: A block-level function named like a parameter stays block-scoped and leaves the parameter alone
features: [compat-function, compat-non-strict-mode, compat-var]
---*/

test("a block function does not overwrite the parameter it is named after", () => {
  function f(a) {
    {
      function a() {}
    }
    return typeof a + ":" + a;
  }

  expect(f(1)).toBe("number:1");
});

test("inside its block the function shadows the parameter", () => {
  function f(a) {
    let inside;
    {
      function a() {
        return "block";
      }
      inside = a();
    }
    return inside + ":" + a;
  }

  expect(f(1)).toBe("block:1");
});

test("a function two blocks deep leaves the parameter alone", () => {
  function f(a) {
    {
      {
        function a() {}
      }
    }
    return a;
  }

  expect(f(1)).toBe(1);
});

test("a function in a switch case leaves the parameter alone", () => {
  function f(a) {
    switch (1) {
      case 1:
        function a() {}
    }
    return a;
  }
  function g(a) {
    switch (0) {
      default:
        function a() {}
    }
    return a;
  }

  expect(f(1)).toBe(1);
  expect(g(2)).toBe(2);
});

test("a closure reads the parameter, not the block function", () => {
  function f(a) {
    const read = () => a;
    {
      function a() {}
    }
    return read();
  }

  expect(f(1)).toBe(1);
});

test("arrow functions, methods and setters keep their parameter", () => {
  const arrow = (a) => {
    {
      function a() {}
    }
    return a;
  };
  const object = {
    method(a) {
      {
        function a() {}
      }
      return a;
    },
    set value(a) {
      {
        function a() {}
      }
      this.seen = a;
    },
  };
  object.value = 3;

  expect(arrow(1)).toBe(1);
  expect(object.method(2)).toBe(2);
  expect(object.seen).toBe(3);
});

test("destructured and rest parameters keep their values", () => {
  function destructured({ a }, [b]) {
    {
      function a() {}
      function b() {}
    }
    return [a, b];
  }
  function rest(...a) {
    {
      function a() {}
    }
    return a;
  }

  expect(destructured({ a: 1 }, [2])).toEqual([1, 2]);
  expect(rest(1, 2)).toEqual([1, 2]);
});

test("a var named like the parameter does not give the block function a var binding", () => {
  function f(a) {
    var a;
    {
      function a() {}
    }
    return a;
  }

  expect(f(1)).toBe(1);
});

test("a block function with another name still updates its var binding", () => {
  function f(a) {
    const before = typeof q;
    {
      function q() {
        return a;
      }
    }
    return before + ":" + q();
  }

  expect(f(1)).toBe("undefined:1");
});
