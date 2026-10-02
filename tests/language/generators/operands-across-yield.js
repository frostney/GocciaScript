/*---
description: let bindings and parameters used as operands keep their values across a generator suspension
features: [generators, let, closures]
---*/

const pass = (value) => value;

describe("operands across yield", () => {
  test("a let binding and a parameter survive a suspension", () => {
    const source = {
      *produce(a, b) {
        let total = a + b;
        const sent = yield total * 2;
        total = total + sent;
        a = a + 1;
        yield total + a;
        yield a * b + total;
      },
    };
    const iterator = source.produce(pass(1), pass(2));

    expect(iterator.next().value).toBe(6);
    expect(iterator.next(10).value).toBe(15);
    expect(iterator.next().value).toBe(17);
    expect(iterator.next().done).toBe(true);
  });

  test("the left operand is read before the generator suspends", () => {
    const source = {
      *produce(a) {
        let value = a;
        const first = value + (yield "first");
        value = value * 10;
        const second = (yield "second") + value;
        return [first, second, value];
      },
    };
    const iterator = source.produce(pass(2));

    expect(iterator.next().value).toBe("first");
    expect(iterator.next(5).value).toBe("second");
    expect(iterator.next(7).value).toEqual([7, 27, 20]);
  });

  test("a closure writes the binding while the generator is suspended", () => {
    const source = {
      *produce(a) {
        let value = a;
        const controls = {
          set: (next) => {
            value = next;
            a = a + next;
          },
        };
        const before = value + a;
        yield controls;
        yield [before, value + 1, a * 2, value + a];
      },
    };
    const iterator = source.produce(pass(1));
    const controls = iterator.next().value;
    controls.set(10);

    expect(iterator.next().value).toEqual([2, 11, 22, 21]);
  });

  test("two iterators of the same generator keep separate bindings", () => {
    const source = {
      *count(start, step) {
        let current = start;
        for (const turn of [0, 1, 2, 3]) {
          const jump = yield current + turn * 0;
          current = current + step + (jump ? jump : 0);
        }
      },
    };
    const low = source.count(pass(0), pass(1));
    const high = source.count(pass(100), pass(10));

    expect([low.next().value, high.next().value]).toEqual([0, 100]);
    expect([low.next().value, high.next(5).value]).toEqual([1, 115]);
    expect([low.next(2).value, high.next().value]).toEqual([4, 125]);
  });

  test("a loop in a generator reads the binding again after each resumption", () => {
    const source = {
      *accumulate(items) {
        let total = 0;
        for (const item of items) {
          total = total + item;
          const reset = yield total * 2;
          if (reset) {
            total = 0;
          }
        }
        return total + 1;
      },
    };
    const iterator = source.accumulate(pass([1, 2, 3]));

    expect(iterator.next().value).toBe(2);
    expect(iterator.next(false).value).toBe(6);
    expect(iterator.next(true).value).toBe(6);
    expect(iterator.next(false)).toEqual({ value: 4, done: true });
  });

  test("return() runs a finally block that reads the bindings", () => {
    const seen = [];
    const source = {
      *guarded(a) {
        let value = a * 2;
        try {
          value = value + 1;
          yield value + a;
          value = value + 100;
          yield value;
        } finally {
          seen.push(value + a, value - 1);
        }
      },
    };
    const iterator = source.guarded(pass(3));

    expect(iterator.next().value).toBe(10);
    expect(iterator.return(42)).toEqual({ value: 42, done: true });
    expect(seen).toEqual([10, 6]);
  });

  test("a closure created in a yield operand inside a loop is seen on the next pass", () => {
    const source = {
      *produce(steps) {
        let value = 1;
        let write = null;
        const seen = [];
        for (const step of steps) {
          seen.push(value + 1);
          if (write) write(step * 10);
          yield (write = (next) => (value = next));
        }
        return seen;
      },
    };
    const iterator = source.produce(pass([1, 2, 3]));
    iterator.next();
    iterator.next();
    iterator.next();

    expect(iterator.next()).toEqual({ value: [2, 2, 21], done: true });
  });

  test("a default parameter value in a generator", () => {
    const source = {
      *produce(a, b = a * 2) {
        yield a + b;
        b = b + 1;
        yield a + b;
      },
    };

    expect([...source.produce(pass(1))]).toEqual([3, 4]);
    expect([...source.produce(pass(1), pass(5))]).toEqual([6, 7]);
  });
});
