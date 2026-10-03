/*---
description: let bindings and parameters used as operands in try, catch and finally blocks, including a finally block that runs on the way out of a return
features: [let, try-catch-finally, closures]
---*/

const pass = (value) => value;

describe("operands around try, catch and finally", () => {
  test("a returned value is fixed before the finally block rebinds its operands", () => {
    const run = (a) => {
      let value = a;
      try {
        return value + a;
      } finally {
        value = value + 100;
        a = a + 100;
      }
    };

    expect(run(pass(1))).toBe(2);
  });

  test("a finally block reads the bindings as the try block left them", () => {
    const run = (a, fail) => {
      const seen = [];
      let value = a;
      try {
        value = value + 1;
        if (fail) {
          throw new Error("stop");
        }
        value = value + 10;
      } catch (error) {
        value = value * 2;
        seen.push(value + a);
      } finally {
        seen.push(value + a, value - 1);
      }
      return seen;
    };

    expect(run(pass(1), pass(false))).toEqual([13, 11]);
    expect(run(pass(1), pass(true))).toEqual([5, 5, 3]);
  });

  test("a finally block that runs for an early return reads the outer binding", () => {
    const run = (early) => {
      const seen = [];
      let value = pass(1);
      const inner = () => {
        try {
          if (early) {
            return value + 1;
          }
          value = value + 10;
          return value + 1;
        } finally {
          value = value + 100;
          seen.push(value + 1);
        }
      };
      const result = inner();
      return [result, seen, value + 1];
    };

    expect(run(pass(true))).toEqual([2, [102], 102]);
    expect(run(pass(false))).toEqual([12, [112], 112]);
  });

  test("the catch parameter is an ordinary operand", () => {
    const run = (a) => {
      try {
        throw a * 2;
      } catch (error) {
        const first = error + a;
        error = error + 1;
        return [first, error * a];
      }
    };

    expect(run(pass(3))).toEqual([9, 21]);
  });

  test("an exception thrown while evaluating an operand leaves earlier writes in place", () => {
    const run = (a) => {
      let value = a;
      const fail = () => {
        throw new Error("stop");
      };
      try {
        value = (value = value + 1) + fail();
      } catch (error) {
        return [value, value + a];
      }
      return "not reached";
    };

    expect(run(pass(1))).toEqual([2, 3]);
  });

  test("a loop inside a try block and a try block inside a loop", () => {
    const run = (items) => {
      let total = 0;
      const seen = [];
      try {
        for (const item of items) {
          try {
            if (item === 2) {
              throw new Error("skip");
            }
            total = total + item;
          } catch (error) {
            total = total + 100;
          } finally {
            seen.push(total + item);
          }
        }
      } finally {
        seen.push(total * 2);
      }
      return seen;
    };

    expect(run(pass([1, 2, 3]))).toEqual([2, 103, 107, 208]);
  });
});

// A finally block is compiled again at each return that leaves its try
// statement. When that return sits inside a loop, the finally block's own
// loops are compiled in the middle of a loop they are not part of.
describe("a loop inside a finally block that runs for a return from another loop", () => {
  test("a let binding rewritten by a closure is read afresh on each iteration", () => {
    const run = () => {
      let value = 1;
      const seen = [];
      try {
        for (const item of [1]) {
          return seen;
        }
      } finally {
        for (const step of [1, 2, 3]) {
          seen.push(value + 1);
          const scale = () => {
            value = value * 10;
          };
          scale();
        }
      }
      return seen;
    };

    expect(run()).toEqual([2, 11, 101]);
  });

  test("a parameter rewritten by a closure is read afresh on each iteration", () => {
    const run = (value) => {
      const seen = [];
      try {
        for (const item of [1]) {
          return seen;
        }
      } finally {
        for (const step of [1, 2, 3]) {
          seen.push(value + 1, value < 50);
          const scale = () => {
            value = value * 10;
          };
          scale();
        }
      }
      return seen;
    };

    expect(run(pass(1))).toEqual([2, true, 11, true, 101, false]);
  });

  test("a store goes to the object the closure put in the binding", () => {
    const run = () => {
      const first = { hits: 0 };
      const second = { hits: 0 };
      let target = first;
      try {
        for (const item of [1]) {
          return [first, second];
        }
      } finally {
        for (const step of [1, 2]) {
          target.hits = step;
          const retarget = () => {
            target = second;
          };
          retarget();
        }
      }
      return [first, second];
    };

    expect(run()).toEqual([{ hits: 1 }, { hits: 2 }]);
  });

  test("the same holds when the return is two loops deep", () => {
    const run = (limit) => {
      let total = 0;
      const seen = [];
      try {
        for (const outer of [1, 2]) {
          for (const inner of [1, 2]) {
            if (outer + inner === limit) {
              return seen;
            }
            total = total + inner;
          }
        }
      } finally {
        for (const step of [1, 2, 3]) {
          seen.push(total + step);
          const bump = () => {
            total = total + 100;
          };
          bump();
        }
      }
      return seen;
    };

    expect(run(pass(4))).toEqual([5, 106, 207]);
    expect(run(pass(9))).toEqual([7, 108, 209]);
  });

  test("a finally loop without a closure still sees writes made in its own body", () => {
    const run = () => {
      let value = 1;
      const seen = [];
      try {
        for (const item of [1]) {
          return seen;
        }
      } finally {
        for (const step of [1, 2, 3]) {
          seen.push(value + 1);
          value = value * 10;
        }
      }
      return seen;
    };

    expect(run()).toEqual([2, 11, 101]);
  });
});
