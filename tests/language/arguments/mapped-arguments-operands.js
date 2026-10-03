/*---
description: A parameter used as an operand follows writes made through a mapped arguments object, and the object follows writes to the parameter
features: [arguments-object, compat-non-strict-mode, function-declarations]
---*/

const pass = (value) => value;

describe("mapped arguments and parameter operands", () => {
  test("a write through arguments is seen by an arithmetic read", () => {
    function run(a, b) {
      const before = a + b;
      arguments[0] = 10;
      arguments[1] = arguments[1] + 20;
      return [before, a + b, a * 2, b - 1, a < b];
    }

    expect(run(pass(1), pass(2))).toEqual([3, 32, 20, 21, true]);
  });

  test("a write to the parameter is seen through arguments", () => {
    function run(a, b) {
      a = a + 10;
      b += 1;
      return [arguments[0] + arguments[1], a + b];
    }

    expect(run(pass(1), pass(2))).toEqual([14, 14]);
  });

  test("the left operand is read before arguments rebinds the parameter", () => {
    function run(a) {
      return [a + ((arguments[0] = 5), a), a + 1];
    }

    expect(run(pass(1))).toEqual([6, 6]);
  });

  test("an escaped arguments object keeps aliasing the parameter", () => {
    const poke = (args, value) => {
      args[0] = value;
      return 1;
    };
    function run(a) {
      const first = a + poke(arguments, 50);
      const second = a + poke(arguments, 60) + a;
      return [first, second, a - 1];
    }

    expect(run(pass(1))).toEqual([2, 111, 59]);
  });

  test("a write through arguments inside a loop is seen on the next pass", () => {
    function run(a, steps) {
      const seen = [];
      for (const step of steps) {
        seen.push(a + step, a * 2);
        arguments[0] = step * 10;
      }
      seen.push(a + 1);
      return seen;
    }

    expect(run(pass(1), pass([1, 2]))).toEqual([2, 2, 12, 20, 21]);
  });

  test("an assignment and a compound assignment follow the mapping", () => {
    function run(a) {
      arguments[0] = 5;
      a = a + 1;
      a += arguments[0];
      a++;
      return [a, arguments[0]];
    }

    expect(run(pass(1))).toEqual([13, 13]);
  });

  test("a parameter the caller left out is not mapped", () => {
    function run(a, b) {
      arguments[1] = 10;
      b = 2;
      return [b + 1, arguments[1] + 1, arguments.length];
    }

    expect(run(pass(1))).toEqual([3, 11, 1]);
  });

  test("an arrow inside the function writes through the outer arguments", () => {
    function run(a) {
      const poke = (value) => {
        arguments[0] = value;
      };
      const before = a + 1;
      poke(40);
      return [before, a + 1];
    }

    expect(run(pass(1))).toEqual([2, 41]);
  });

  test("a function with default values has unmapped arguments", () => {
    function run(a, b = 2) {
      arguments[0] = 10;
      a = a + 1;
      return [a + b, arguments[0] + 1];
    }

    expect(run(pass(1))).toEqual([4, 11]);
  });

  test("methods map their parameters too", () => {
    const holder = {
      run(a) {
        arguments[0] = a + 10;
        return a * 2;
      },
    };
    class Holder {
      run(a) {
        arguments[0] = a + 100;
        return a * 2;
      }
    }

    expect(holder.run(pass(1))).toBe(22);
    expect(new Holder().run(pass(1))).toBe(2);
  });
});
