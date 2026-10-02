/*---
description: A hoisted function declaration that writes a let binding or a parameter is seen by operands in the enclosing function, and a var or function declaration can redeclare a parameter
features: [compat-function, compat-var]
---*/

const pass = (value) => value;

describe("hoisted functions that write an enclosing binding", () => {
  test("a function declared after the read writes a let binding", () => {
    function outer(seed) {
      let value = seed;
      const before = value + 1;
      write(10);
      const after = value + 1;
      function write(next) {
        value = next;
      }
      return [before, after, value * 2];
    }

    expect(outer(pass(1))).toEqual([2, 11, 20]);
  });

  test("a function declared after the read writes a parameter", () => {
    function outer(a, b) {
      const before = a + b;
      bump();
      bump();
      function bump() {
        a = a + 10;
        b = b * 2;
      }
      return [before, a + b, a - 1, b < a];
    }

    expect(outer(pass(1), pass(2))).toEqual([3, 29, 20, true]);
  });

  test("a function declared in a block writes a binding of that block", () => {
    function outer(seed) {
      const seen = [];
      {
        let value = seed;
        seen.push(value + 1);
        write();
        seen.push(value + 1);
        function write() {
          value = value * 5;
        }
      }
      return seen;
    }

    expect(outer(pass(2))).toEqual([3, 11]);
  });

  test("a hoisted function called before the declaration it writes throws", () => {
    function outer() {
      write();
      let value = 1;
      function write() {
        value = 2;
      }
      return value;
    }

    expect(() => outer()).toThrow(ReferenceError);
  });

  test("a function expression assigned later writes a binding read in a loop", () => {
    function outer(steps) {
      let value = 1;
      let write = null;
      const seen = [];
      for (const step of steps) {
        seen.push(value + step);
        if (write) {
          write(step * 10);
        }
        write = function (next) {
          value = next;
        };
      }
      return seen;
    }

    expect(outer(pass([1, 2, 3]))).toEqual([2, 3, 23]);
  });

  test("a function declared in a nested block later in the loop body writes a binding read at the top", () => {
    function outer(steps) {
      let value = 1;
      let write = null;
      const seen = [];
      for (const step of steps) {
        seen.push(value + step);
        if (write) {
          write(step * 10);
        }
        {
          function assign(next) {
            value = next;
          }
          write = assign;
        }
      }
      return seen;
    }

    expect(outer(pass([1, 2, 3]))).toEqual([2, 3, 23]);
  });

  test("a function declared directly in the loop body writes a binding read at the top", () => {
    function outer(steps) {
      let value = 1;
      let write = null;
      const seen = [];
      for (const step of steps) {
        seen.push(value + step);
        if (write) {
          write(step * 10);
        }
        write = assign;
        function assign(next) {
          value = next;
        }
      }
      return seen;
    }

    expect(outer(pass([1, 2, 3]))).toEqual([2, 3, 23]);
  });
});

describe("redeclaring a parameter", () => {
  test("a var with the parameter's name is the same binding", () => {
    function run(a) {
      const before = a + 1;
      var a = a * 10;
      return [before, a + 1];
    }

    expect(run(pass(2))).toEqual([3, 21]);
  });

  test("a var initializer may read the parameter it redeclares", () => {
    function element(a, i) {
      var a = a[i];
      return a;
    }
    function index(a, i) {
      var i = a[i];
      return i;
    }
    function twice(a) {
      var a = a + a;
      return a;
    }
    function compare(a, b) {
      var b = a < b;
      return b;
    }
    function minusOne(a) {
      var a = a - 1;
      return a;
    }

    expect(element(pass([1, 2]), pass(1))).toBe(2);
    expect(index(pass([5, 6]), pass(1))).toBe(6);
    expect(twice(pass(3))).toBe(6);
    expect(twice(pass("ab"))).toBe("abab");
    expect(compare(pass(1), pass(2))).toBe(true);
    expect(minusOne(pass(3))).toBe(2);
  });

  test("a var without an initializer keeps the argument", () => {
    function run(a) {
      var a;
      return a + 1;
    }

    expect(run(pass(2))).toBe(3);
  });

  test("a function declaration with the parameter's name replaces the argument", () => {
    function run(a) {
      const kind = typeof a;
      function a() {
        return 7;
      }
      return [kind, a() + 1];
    }

    expect(run(pass(2))).toEqual(["function", 8]);
  });

  test("a function parameter keeps its value through this and new.target reads", () => {
    function Shape(width, height) {
      this.area = width * height;
      this.sum = width + height + (new.target ? 1 : 0);
    }

    const shape = new Shape(pass(2), pass(3));
    expect([shape.area, shape.sum]).toEqual([6, 6]);
  });
});
