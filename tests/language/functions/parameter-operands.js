/*---
description: Parameters used as operands hold the argument, the default or the value last assigned, in every kind of function
features: [arrow-functions, default-parameters, rest-parameters, destructuring, classes, getters-setters]
---*/

// Arguments go through a call so the compiler cannot fold the expressions.
const pass = (value) => value;

describe("parameters as operands", () => {
  test("arithmetic and comparison on the arguments", () => {
    const combine = (a, b) => a + b * 2;
    const compare = (a, b) => [a < b, a <= b, a > b, a >= b, a === b, a !== b];
    const bitwise = (a, b) => [a & b, a | b, a ^ b, a << b, a >> b, a >>> b];
    const literal = (a) => [a + 1, 1 + a, a - 1, a * 2, a <= 1 ? "low" : "high"];

    expect(combine(pass(1), pass(2))).toBe(5);
    expect(compare(pass(1), pass(2))).toEqual([true, true, false, false, false, true]);
    expect(bitwise(pass(6), pass(3))).toEqual([2, 7, 5, 48, 0, 0]);
    expect(literal(pass(1))).toEqual([2, 2, 0, 2, "low"]);
    expect(literal(pass(7))).toEqual([8, 8, 6, 14, "high"]);
  });

  test("the same parameter as both operands", () => {
    const twice = (a) => [a + a, a * a, a - a, a === a, a < a];

    expect(twice(pass(6))).toEqual([12, 36, 0, true, false]);
    expect(twice(pass("ab"))).toEqual(["abab", NaN, NaN, true, false]);
  });

  test("a missing argument reads as undefined", () => {
    const add = (a, b) => a + b;
    const index = (list, position) => list[position];

    expect(Number.isNaN(add(pass(1)))).toBe(true);
    expect(add(pass("x"))).toBe("xundefined");
    expect(index(pass([1, 2]))).toBe(undefined);
  });

  test("an extra argument is ignored", () => {
    const add = (a, b) => a + b;

    expect(add(pass(1), pass(2), pass(100))).toBe(3);
  });

  test("non-numeric arguments keep generic operator semantics", () => {
    const add = (a, b) => a + b;
    const less = (a, b) => a < b;
    const multiply = (a, b) => a * b;

    expect(add(pass("ab"), pass([1, 2]))).toBe("ab1,2");
    expect(add(pass(10n), pass(5n))).toBe(15n);
    expect(multiply(pass({ valueOf: () => 4 }), pass("3"))).toBe(12);
    expect(less(pass("a"), pass("b"))).toBe(true);
    expect(less(pass(null), pass(1))).toBe(true);
    expect(() => add(pass(1n), pass(1))).toThrow(TypeError);
    expect(() => add(pass(Symbol("s")), pass(1))).toThrow(TypeError);
  });

  test("a parameter reassigned in the body", () => {
    const run = (a, b) => {
      const before = a + b;
      a = a * 10;
      b += a;
      a++;
      return [before, a + b, a - 1, b];
    };

    expect(run(pass(1), pass(2))).toEqual([3, 23, 10, 12]);
  });

  test("a parameter used as object, key and stored value", () => {
    const run = (target, key, value) => {
      target[key] = value;
      target.copy = value;
      key = key + 1;
      target[key] = target[key - 1] + value;
      return target;
    };

    const result = run(pass([0, 0]), pass(0), pass(5));
    expect(result[0]).toBe(5);
    expect(result[1]).toBe(10);
    expect(result.copy).toBe(5);
  });

  test("recursion keeps each activation's arguments apart", () => {
    const fib = (n) => (n <= 1 ? n : fib(n - 1) + fib(n - 2));
    const sum = (n, total) => (n < 1 ? total : sum(n - 1, total + n));
    const depth = (n) => {
      if (n < 1) {
        return 0;
      }
      const below = depth(n - 1);
      return below + n * 2 - n;
    };

    expect(fib(pass(15))).toBe(610);
    expect(sum(pass(100), pass(0))).toBe(5050);
    expect(depth(pass(10))).toBe(55);
  });
});

describe("default parameter values", () => {
  test("a default can read an earlier parameter", () => {
    const run = (a, b = a * 2, c = a + b) => [a, b, c];

    expect(run(pass(1))).toEqual([1, 2, 3]);
    expect(run(pass(1), pass(5))).toEqual([1, 5, 6]);
    expect(run(pass(1), undefined, pass(0))).toEqual([1, 2, 0]);
  });

  test("a default that reads a later parameter throws", () => {
    const run = (a = b + 1, b = 2) => a + b;
    const self = (a = a + 1) => a;

    expect(() => run()).toThrow(ReferenceError);
    expect(run(pass(1))).toBe(3);
    expect(() => self()).toThrow(ReferenceError);
    expect(self(pass(1))).toBe(1);
  });

  test("the body reads the default once it has been applied", () => {
    const run = (a, b = 10) => {
      const sum = a + b;
      b = b + 1;
      return [sum, a * b, b - 1];
    };

    expect(run(pass(2))).toEqual([12, 22, 10]);
    expect(run(pass(2), pass(3))).toEqual([5, 8, 3]);
  });

  test("a default that is a closure over another parameter", () => {
    const run = (a, read = () => a * 2, write = (value) => (a = value)) => {
      const before = a + read();
      write(10);
      return [before, a + 1, read()];
    };

    expect(run(pass(1))).toEqual([3, 11, 20]);
  });
});

describe("rest and destructured parameters", () => {
  test("a rest parameter is an ordinary array operand", () => {
    const run = (first, ...rest) => [rest.length, rest[0] + first, rest[first], first + rest];

    expect(run(pass(1), pass(2), pass(3))).toEqual([2, 3, 3, "12,3"]);
    expect(run(pass(1))).toEqual([0, NaN, undefined, "1"]);
  });

  test("bindings taken from a destructured parameter", () => {
    const fromObject = ({ a, b }, scale) => (a + b) * scale;
    const fromArray = ([first, second = first * 2], offset) => first + second + offset;
    const reassigned = ({ a }, b) => {
      a = a + b;
      b = b + a;
      return a * b;
    };

    expect(fromObject(pass({ a: 1, b: 2 }), pass(3))).toBe(9);
    expect(fromArray(pass([1]), pass(10))).toBe(13);
    expect(reassigned(pass({ a: 1 }), pass(2))).toBe(15);
  });
});

describe("parameters and closures", () => {
  test("a closure that reads the parameter sees later assignments", () => {
    const run = (a) => {
      const read = () => a + 1;
      const first = read();
      a = a * 10;
      return [first, read(), a + 1];
    };

    expect(run(pass(1))).toEqual([2, 11, 11]);
  });

  test("a closure that writes the parameter is seen by the function", () => {
    const run = (a, b) => {
      const before = a + b;
      const write = (value) => {
        a = value;
        b = b + value;
      };
      write(10);
      const middle = a + b;
      write(20);
      return [before, middle, a + b, a - 1, b * 2];
    };

    expect(run(pass(1), pass(2))).toEqual([3, 22, 52, 19, 64]);
  });

  test("a nested block can shadow a parameter", () => {
    const run = (a) => {
      const results = [a + 1];
      {
        let a = pass(100);
        a = a + 1;
        results.push(a + 1);
      }
      results.push(a + 2);
      return results;
    };

    expect(run(pass(1))).toEqual([2, 102, 3]);
  });
});

describe("parameters of methods and accessors", () => {
  test("object methods and setters", () => {
    const calculator = {
      total: 0,
      add(a, b = a) {
        return a + b * 2;
      },
      set value(next) {
        this.total = next * 2 + next;
      },
    };

    calculator.value = pass(3);
    expect(calculator.add(pass(1), pass(2))).toBe(5);
    expect(calculator.add(pass(1))).toBe(3);
    expect(calculator.total).toBe(9);
  });

  test("class constructors, methods, static methods and setters", () => {
    class Point {
      constructor(x, y) {
        this.x = x + 0;
        this.y = y * 1;
      }
      plus(other, scale = 1) {
        return new Point(this.x + other.x * scale, this.y + other.y * scale);
      }
      static distance(a, b) {
        return (a.x - b.x) * (a.x - b.x) + (a.y - b.y) * (a.y - b.y);
      }
      set both(value) {
        this.x = value + 1;
        this.y = value - 1;
      }
    }
    class Point3 extends Point {
      constructor(x, y, z) {
        super(x * 2, y * 2);
        this.z = z + x;
      }
    }

    const sum = new Point(pass(1), pass(2)).plus(new Point(pass(3), pass(4)), pass(2));
    const moved = new Point(pass(0), pass(0));
    moved.both = pass(5);
    const deep = new Point3(pass(1), pass(2), pass(3));

    expect([sum.x, sum.y]).toEqual([7, 10]);
    expect(Point.distance(new Point(pass(0), pass(0)), new Point(pass(3), pass(4)))).toBe(25);
    expect([moved.x, moved.y]).toEqual([6, 4]);
    expect([deep.x, deep.y, deep.z]).toEqual([2, 4, 4]);
  });
});
