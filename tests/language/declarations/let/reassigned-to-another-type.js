/*---
description: A let that starts as a number and is later given another type keeps JavaScript semantics at every read
features: [let-declaration, for-of, closures, destructuring, bigint]
---*/

const pass = (value) => value;

describe("a later assignment in a loop body reaches earlier reads", () => {
  test("addition of the binding to itself", () => {
    let x = 1;
    const seen = [];
    for (const step of [0, 1]) {
      seen.push(x + x);
      x = "s";
    }
    expect(seen).toEqual([2, "ss"]);
  });

  test("addition, subtraction and multiplication with a literal", () => {
    let x = 1;
    const seen = [];
    for (const step of [0, 1]) {
      seen.push(x + 1, 1 + x, x - 1, x * 2, x + 2.5);
      x = "4";
    }
    expect(seen).toEqual([2, 2, 0, 2, 3.5, "41", "14", 3, 8, "42.5"]);
  });

  test("subtraction of a non-numeric string is NaN", () => {
    let x = 1;
    const seen = [];
    for (const step of [0, 1]) {
      seen.push(x - 1);
      x = "s";
    }
    expect(seen[0]).toBe(0);
    expect(Number.isNaN(seen[1])).toBe(true);
  });

  test("comparison and equality", () => {
    let x = 1;
    const seen = [];
    for (const step of [0, 1]) {
      seen.push(x < 5, x < "10", x === 1, x <= 1);
      x = "9";
    }
    expect(seen).toEqual([true, true, true, true, false, false, false, false]);
  });

  test("self increment by one", () => {
    let x = 1;
    const seen = [];
    for (const step of [0, 1]) {
      x = x + 1;
      seen.push(x);
      x = "s" + step;
    }
    expect(seen).toEqual([2, "s01"]);
  });

  test("an assignment inside a nested block", () => {
    let x = 1;
    const seen = [];
    for (const step of [0, 1]) {
      seen.push(x + 1);
      {
        if (step >= 0) {
          x = "s";
        }
      }
    }
    expect(seen).toEqual([2, "s1"]);
  });

  test("compound assignments", () => {
    let x = 1;
    let y = 10;
    const seen = [];
    for (const step of [0, 1]) {
      seen.push(x + 1, y + 1);
      x += "a";
      y ||= 0;
      y &&= "b";
    }
    expect(seen).toEqual([2, 11, "1a1", "b1"]);
  });

  test("an assignment from a call that returns a string", () => {
    const text = () => "s";
    let x = 1;
    const seen = [];
    for (const step of [0, 1]) {
      seen.push(x + 1);
      x = text();
    }
    expect(seen).toEqual([2, "s1"]);
  });

  test("an assignment from a call that returns a BigInt", () => {
    const big = () => 5n;
    let x = 1;
    const seen = [];
    for (const step of [0, 1]) {
      seen.push(x + x, x * x);
      x = big();
    }
    expect(seen).toEqual([2, 1, 10n, 25n]);
    expect(() => x + 1).toThrow(TypeError);
  });

  test("an assignment of an object with valueOf", () => {
    const boxed = () => ({ valueOf() { return 10; } });
    let x = 1;
    const seen = [];
    for (const step of [0, 1]) {
      seen.push(x - 1, x * 2);
      x = boxed();
    }
    expect(seen).toEqual([0, 2, 9, 20]);
  });

  test("array and object destructuring assignments", () => {
    let x = 1;
    let y = 1;
    const seen = [];
    for (const step of [0, 1]) {
      seen.push(x + 1, y + 1);
      [x] = ["a"];
      ({ y } = { y: "b" });
    }
    expect(seen).toEqual([2, 2, "a1", "b1"]);
  });

  test("a for...of loop that assigns the binding", () => {
    let x = 1;
    const seen = [];
    for (const step of [0, 1]) {
      seen.push(x + 1);
      for (x of ["s"]) {
        // The loop head is the assignment.
      }
    }
    expect(seen).toEqual([2, "s1"]);
  });

  test("an assignment by a closure created in the loop", () => {
    let x = 1;
    const seen = [];
    for (const step of [0, 1]) {
      seen.push(x + 1);
      const assign = () => { x = "s"; };
      assign();
    }
    expect(seen).toEqual([2, "s1"]);
  });

  test("an assignment by a closure created before the loop", () => {
    let x = 1;
    const seen = [];
    const assign = (value) => { x = value; };
    for (const step of [0, 1]) {
      seen.push(x + 1);
      assign(pass("s"));
    }
    expect(seen).toEqual([2, "s1"]);
  });

  test("an assignment by a method of an object literal", () => {
    let x = 1;
    const seen = [];
    const holder = { set(value) { x = value; } };
    for (const step of [0, 1]) {
      seen.push(x + 1);
      holder.set("s");
    }
    expect(seen).toEqual([2, "s1"]);
  });

  test("an assignment in a finally block", () => {
    let x = 1;
    const seen = [];
    for (const step of [0, 1]) {
      try {
        seen.push(x + 1);
      } finally {
        x = "s";
      }
    }
    expect(seen).toEqual([2, "s1"]);
  });

  test("an assignment in a switch clause", () => {
    let x = 1;
    const seen = [];
    for (const step of [0, 1]) {
      switch (step) {
        case 0:
          seen.push(x + 1);
          x = "s";
          break;
        default:
          seen.push(x + 1);
      }
    }
    expect(seen).toEqual([2, "s1"]);
  });

  test("a continue that skips a numeric assignment", () => {
    let x = 1;
    const seen = [];
    for (const step of [0, 1, 2]) {
      seen.push(x + 1);
      x = "s";
      if (step < 5) continue;
      x = 2;
    }
    expect(seen).toEqual([2, "s1", "s1"]);
  });

  test("a break that skips a numeric assignment", () => {
    let x = 1;
    for (const step of [0, 1]) {
      x = "s";
      if (step === 0) break;
      x = 2;
    }
    expect(x + 1).toBe("s1");
  });
});

describe("an assignment in one branch reaches a read after the join", () => {
  test("if and else", () => {
    const run = (flag) => {
      let x = 1;
      if (flag) {
        x = "s";
      } else {
        x = 2;
      }
      return x + 1;
    };
    expect(run(pass(true))).toBe("s1");
    expect(run(pass(false))).toBe(3);
  });

  test("a conditional expression", () => {
    const run = (flag) => {
      let x = 1;
      flag ? (x = "s") : (x = 2);
      return x + 1;
    };
    expect(run(pass(true))).toBe("s1");
    expect(run(pass(false))).toBe(3);
  });

  test("a switch", () => {
    const run = (key) => {
      let x = 1;
      switch (key) {
        case "text":
          x = "s";
          break;
        default:
          x = 2;
      }
      return x + 1;
    };
    expect(run(pass("text"))).toBe("s1");
    expect(run(pass("number"))).toBe(3);
  });

  test("a throw out of a try block", () => {
    let x = 1;
    try {
      x = "s";
      throw new Error("stop");
      x = 2;
    } catch (error) {
      // x keeps the string.
    }
    expect(x + 1).toBe("s1");
  });

  test("a let without an initializer read in the other branch", () => {
    const run = (flag) => {
      let x;
      if (flag) {
        x = 1;
      } else {
        return x + 1;
      }
      return x + 1;
    };
    expect(Number.isNaN(run(pass(false)))).toBe(true);
    expect(run(pass(true))).toBe(2);
  });

  test("a let without an initializer read in a later switch clause", () => {
    const run = (key) => {
      let x;
      switch (key) {
        case 0:
          x = 1;
          break;
        case 1:
          return x + 1;
      }
      return x + 1;
    };
    expect(Number.isNaN(run(pass(1)))).toBe(true);
    expect(run(pass(0))).toBe(2);
  });
});

describe("bindings that only ever hold numbers", () => {
  test("accumulators and counters", () => {
    const run = (values) => {
      let total = 0;
      let count = 0;
      let scaled = 1.5;
      for (const value of values) {
        const doubled = value * 2;
        total = total + doubled;
        count += 1;
        scaled = scaled * 0.5 + count;
      }
      return [total, count, scaled, total + count];
    };
    expect(run([1, 2, 3])).toEqual([12, 3, 4.4375, 15]);
  });

  test("bitwise updates and unary operators", () => {
    let x = 0;
    let y = 1;
    for (const step of [0, 1, 2, 3, 4, 5, 6, 7]) {
      x = x + y;
      if ((x & 7) === 0) {
        y = y + 1;
      } else {
        y = y ^ 3;
      }
      y = -(-y);
    }
    expect([x, y, x + y]).toEqual([12, 1, 13]);
  });

  test("a recursive function", () => {
    const fib = (n) => n <= 1 ? n : fib(n - 1) + fib(n - 2);
    let total = 0;
    for (const n of [10, 15]) {
      total = total + fib(n);
    }
    expect(total).toBe(665);
  });
});
