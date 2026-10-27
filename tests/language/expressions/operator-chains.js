/*---
description: A chain of binary operators keeps each operator's value, its type and its constant folding, whichever way the chain nests
features: [let, const, arrow-functions, addition-operator, template-literals]
---*/

// ES2026 §13.15.3 ApplyStringOrNumericBinaryOperator: every operator of a
// chain applies to the values its operands produced, left operand first.
// Each helper takes its inputs as arguments, so that only the parts the tests
// name can be folded at compile time.

const opaque = (value) => value;

describe("a left-nested chain", () => {
  test("of Numbers adds every term", () => {
    const sum = (p) => p + p + p + p + p + p + p + p + p + p + p + p;
    expect(sum(opaque(2))).toBe(24);
    expect(sum(opaque(0.5))).toBe(6);
  });

  test("turns into concatenation at the first String", () => {
    const mixed = (n, s) => n + n + s + n + n;
    expect(mixed(opaque(1), opaque("a"))).toBe("2a11");
    expect(mixed(opaque(1), opaque(2))).toBe(6);
  });

  test("with Int16 immediates", () => {
    const steps = (x) => x + 1 - 2 + 3 - 4 + 5;
    expect(steps(opaque(10))).toBe(13);
    expect(steps(opaque("a"))).toBeNaN();
    const concat = (x) => x + 1 + 2;
    expect(concat(opaque("a"))).toBe("a12");
    expect(concat(opaque(1))).toBe(4);
  });

  test("of other operators", () => {
    const product = (p) => p * p * p * p * p;
    const bits = (p) => p | 1 | 2 | 4 | 8;
    const compare = (p) => p < p < p < p;
    const shifts = (p) => p << 1 << 1 >>> 1;
    expect(product(opaque(2))).toBe(32);
    expect(bits(opaque(16))).toBe(31);
    expect(compare(opaque(5))).toBe(true);
    expect(shifts(opaque(-1))).toBe(2147483646);
  });

  test("with constants at its start folds them and keeps the rest", () => {
    const start = (p) => 1 + 2 + p + 3;
    expect(start(opaque(10))).toBe(16);
    expect(start(opaque("a"))).toBe("3a3");
    const k = 4;
    const named = (p) => k * 2 + k + p + k;
    expect(named(opaque(1))).toBe(17);
    expect(named(opaque("b"))).toBe("12b4");
  });

  test("of constants only folds to the same value", () => {
    expect(1 + 2 + 3 + 4 + 5 + 6 + 7 + 8).toBe(36);
    expect("a" + 1 + 2 + "b" + 3).toBe("a12b3");
    expect(1 + 2 + "c" + 3 + 4).toBe("3c34");
  });

  test("of logical operators short-circuits at the first deciding term", () => {
    const all = (a, b, c) => a && b && c && "end";
    const any = (a, b, c) => a || b || c || "end";
    expect(all(opaque(1), opaque(2), opaque(3))).toBe("end");
    expect(all(opaque(1), opaque(0), opaque(3))).toBe(0);
    expect(any(opaque(0), opaque(""), opaque(null))).toBe("end");
    expect(any(opaque(0), opaque("x"), opaque(null))).toBe("x");
  });
});

describe("a right-nested chain", () => {
  test("of Numbers adds every term", () => {
    const sum = (p) => p + (p + (p + (p + (p + p))));
    expect(sum(opaque(3))).toBe(18);
  });

  test("with a String innermost concatenates from the inside out", () => {
    const nested = (n, s) => n + (n + (s + (n + n)));
    expect(nested(opaque(1), opaque("a"))).toBe("11a2");
  });

  test("with an Int16 immediate on the left", () => {
    const leading = (x) => 1 + (2 + (3 + x));
    expect(leading(opaque(4))).toBe(10);
    expect(leading(opaque("a"))).toBe("123a");
  });
});

describe("an operator whose right operand changes a binding of its left one", () => {
  test("keeps the type the left operand's value had", () => {
    const run = (start) => {
      let x = start;
      const f = () => {
        x = "s";
        return 2;
      };
      return (x + 1) + (f(), 2) + x;
    };
    expect(run(opaque(1))).toBe("4s");
  });

  test("sees the assignment in a later operand", () => {
    const run = (start) => {
      let x = start;
      return x + 1 + (x = "t", 1) + x;
    };
    expect(run(opaque(1))).toBe("3t");
  });

  test("reads a closure's binding after the closure ran in an earlier iteration", () => {
    const run = () => {
      let x = 1;
      let set = null;
      const results = [];
      for (const round of [1, 2]) {
        results.push(x + 1 + 1);
        set = () => {
          x = "r";
        };
        set();
      }
      return results;
    };
    expect(run()).toEqual([3, "r11"]);
  });
});
