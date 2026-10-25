/*---
description: A let binding keeps an inferred Number type only when every value it can hold is a Number
features: [let, closures, ToNumeric]
---*/

// A typed instruction does not check its operands, so a let that can be assigned
// is typed only when the whole-body proof (Goccia.Compiler.NumericBindings)
// shows its initializer and every assignment, closures included, produce a
// Number.

describe("let bindings", () => {
  test("a let reassigned to another type later in a loop", () => {
    const seen = [];
    let x = 1;
    for (const i of [0, 1]) {
      seen.push(x + x);
      x = "s";
    }
    expect(seen).toEqual([2, "ss"]);
  });

  test("a let with a computed Number initializer reassigned to a string", () => {
    const k = { valueOf: () => 2 };
    const seen = [];
    let x = k * 2;
    for (const i of [0, 1]) {
      seen.push(x === x, x + x);
      x = "t";
    }
    expect(seen).toEqual([true, 8, true, "tt"]);
  });

  test("a let that only ever holds Numbers", () => {
    const k = { valueOf: () => 3 };
    let x = k * 1;
    let total = 0;
    for (const i of [1, 2, 3]) {
      x = x * i;
      total = total + x;
    }
    expect([total === total, total + total]).toEqual([true, 54]);
  });

  test("a captured let mutated by a closure", () => {
    const k = { valueOf: () => 2 };
    let n = 2 * k;
    const set = () => {
      n = "z";
    };
    const read = () => n + n;
    expect(read()).toBe(8);
    set();
    expect(read()).toBe("zz");
    expect(n + 1).toBe("z1");
  });

  test("a captured let that only holds Numbers and no closure assigns", () => {
    const k = { valueOf: () => 2 };
    let n = 2 * k;
    const read = () => [n === n, n + n];
    expect(read()).toEqual([true, 8]);
    n = n + 1;
    expect(read()).toEqual([true, 10]);
  });

  test("a let initialized from a value that may not be a Number", () => {
    const f = (v) => {
      let x = v * 1;
      let y = v;
      return [x === x, y + y];
    };
    expect(f("2")).toEqual([true, "22"]);
  });
});
