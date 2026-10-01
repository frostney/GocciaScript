/*---
description: const bindings are in the temporal dead zone until their declaration runs, and hold their value afterwards
features: [const, temporal-dead-zone]
---*/

describe("const temporal dead zone", () => {
  test("a read before the declaration in the same block throws", () => {
    expect(() => {
      const early = value + 1;
      const value = 2;
      return early;
    }).toThrow(ReferenceError);
  });

  test("the initializer cannot read its own binding", () => {
    expect(() => {
      const value = value + 1;
      return value;
    }).toThrow(ReferenceError);
  });

  test("an inner declaration shadows the outer one from the start of its block", () => {
    expect(() => {
      const value = 1;
      {
        const early = value + 0;
        const value = 2;
        return early;
      }
    }).toThrow(ReferenceError);
  });

  test("every loop iteration starts with the binding uninitialized", () => {
    const results = [];
    for (const step of [0, 1]) {
      try {
        results.push(doubled * 2);
      } catch (error) {
        results.push(error instanceof ReferenceError);
      }
      const doubled = step + 1;
      results.push(doubled * 2);
    }
    expect(results).toEqual([true, 2, true, 4]);
  });

  test("a switch clause entered directly does not see an earlier clause's declaration", () => {
    const read = (which) => {
      switch (which) {
        case 0:
          const scale = which + 5;
          return scale * 2;
        case 1:
          return scale * 2;
      }
      return "none";
    };
    expect(read(0)).toBe(10);
    expect(() => read(1)).toThrow(ReferenceError);
  });

  test("a switch clause reached by falling through sees the declaration", () => {
    const read = (which) => {
      switch (which) {
        case 0:
          const scale = which + 5;
        case 1:
          return scale + 1;
      }
      return "none";
    };
    expect(read(0)).toBe(6);
    expect(() => read(1)).toThrow(ReferenceError);
  });

  test("a closure created before the declaration throws until it runs", () => {
    const run = () => {
      const read = () => value * 2;
      const before = [];
      try {
        read();
      } catch (error) {
        before.push(error instanceof ReferenceError);
      }
      const value = 21;
      return [before, read(), value * 2];
    };
    expect(run()).toEqual([[true], 42, 42]);
  });
});

describe("const bindings as operands", () => {
  test("arithmetic, comparison and logical operands read the declared value", () => {
    const a = 7;
    const b = 3;
    expect(a + b).toBe(10);
    expect(a - b).toBe(4);
    expect(a * b).toBe(21);
    expect(a / b).toBe(7 / 3);
    expect(a % b).toBe(1);
    expect(a ** b).toBe(343);
    expect(a & b).toBe(3);
    expect(a | b).toBe(7);
    expect(a ^ b).toBe(4);
    expect(a << b).toBe(56);
    expect(a >> b).toBe(0);
    expect(a < b).toBe(false);
    expect(a > b).toBe(true);
    expect(a <= b).toBe(false);
    expect(a >= b).toBe(true);
    expect(a === b).toBe(false);
    expect(a !== b).toBe(true);
    expect(a + 1).toBe(8);
    expect(1 + a).toBe(8);
    expect(a - 1).toBe(6);
  });

  test("the same binding can be both operands", () => {
    const value = 6;
    expect(value + value).toBe(12);
    expect(value * value).toBe(36);
    expect(value === value).toBe(true);
    expect(value < value).toBe(false);
  });

  test("non-numeric values keep generic operator semantics", () => {
    const text = "ab";
    const list = [1, 2];
    const big = 10n;
    const box = { valueOf: () => 4 };
    expect(text + text).toBe("abab");
    expect(text + list).toBe("ab1,2");
    expect(big * big).toBe(100n);
    expect(box + box).toBe(8);
    expect(box < text).toBe(false);
    expect("length" in list).toBe(true);
    expect(list instanceof Array).toBe(true);
  });

  test("computed reads and writes use the binding as object, key and value", () => {
    const target = { count: 1 };
    const list = [10, 20, 30];
    const key = "count";
    const index = 1;
    const replacement = "x";

    expect(target[key]).toBe(1);
    expect(list[index]).toBe(20);
    list[index] = replacement;
    target[key] = index;
    target.other = replacement;
    expect(list).toEqual([10, "x", 30]);
    expect(target).toEqual({ count: 1, other: "x" });
    expect(index).toBe(1);
    expect(key).toBe("count");
    expect(replacement).toBe("x");
  });

  test("an assignment expression still produces the assigned value", () => {
    const target = {};
    const list = [];
    const value = 5;
    const index = 0;
    const fromIndex = (list[index] = value);
    const fromName = (target.name = value);
    expect(fromIndex).toBe(5);
    expect(fromName).toBe(5);
    expect(value).toBe(5);
  });

  test("a compound assignment through a computed key leaves the key binding intact", () => {
    const list = [1, 2, 3];
    const index = 1;
    list[index] *= -1;
    list[index] += index;
    expect(list).toEqual([1, -1, 3]);
    expect(index).toBe(1);
    expect(typeof index).toBe("number");
  });

  test("a binding captured by a closure keeps its value in each iteration", () => {
    const readers = [];
    for (const step of [1, 2, 3]) {
      const scaled = step * 10;
      readers.push(() => scaled + step);
      expect(scaled + step).toBe(step * 11);
    }
    expect(readers.map((read) => read())).toEqual([11, 22, 33]);
  });

  test("the left operand is unaffected by side effects in the right operand", () => {
    const base = 10;
    let calls = 0;
    const bump = () => {
      calls += 1;
      return calls;
    };
    expect(base + bump()).toBe(11);
    expect(base * bump() + base).toBe(30);
    expect(calls).toBe(2);
  });

  test("values survive a generator suspension and an await", async () => {
    const source = {
      *produce() {
        const first = 2;
        const second = yield first + 1;
        const third = first * second;
        yield third + first;
      },
    };
    const iterator = source.produce();
    expect(iterator.next().value).toBe(3);
    expect(iterator.next(5).value).toBe(12);

    const compute = async () => {
      const first = 2;
      const second = await Promise.resolve(5);
      return first * second + first;
    };
    expect(await compute()).toBe(12);
  });

  test("a for...of binding is usable as an operand in the loop body", () => {
    const totals = [];
    const weights = [2, 3];
    for (const index of [0, 1]) {
      totals.push(weights[index] * index + index);
    }
    expect(totals).toEqual([0, 4]);
  });
});
