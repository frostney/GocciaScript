/*---
description: let bindings and parameters used as operands keep their values across an await
features: [async-await, let, closures]
---*/

const pass = (value) => value;

describe("operands across await", () => {
  test("a let binding and a parameter survive an await", async () => {
    const compute = async (a, b) => {
      let total = a + b;
      const sent = await Promise.resolve(10);
      total = total + sent;
      a = a + 1;
      return [total + a, a * b + total];
    };

    expect(await compute(pass(1), pass(2))).toEqual([15, 17]);
  });

  test("the left operand is read before the function suspends", async () => {
    const compute = async (a) => {
      let value = a;
      const first = value + (await Promise.resolve(5));
      value = value * 10;
      const second = (await Promise.resolve(7)) + value;
      return [first, second, value];
    };

    expect(await compute(pass(2))).toEqual([7, 27, 20]);
  });

  test("a closure writes the binding while the function is suspended", async () => {
    let controls = null;
    const compute = async (a) => {
      let value = a;
      controls = {
        set: (next) => {
          value = next;
          a = a + next;
        },
      };
      const before = value + a;
      await Promise.resolve();
      return [before, value + 1, a * 2, value + a];
    };

    const pending = compute(pass(1));
    controls.set(10);

    expect(await pending).toEqual([2, 11, 22, 21]);
  });

  test("a loop reads the binding again after each await", async () => {
    const compute = async (items) => {
      let total = 0;
      const seen = [];
      for (const item of items) {
        total = total + (await Promise.resolve(item));
        seen.push(total * 2);
      }
      return [seen, total + 1];
    };

    expect(await compute(pass([1, 2, 3]))).toEqual([[2, 6, 12], 7]);
  });

  test("a closure created in an await operand inside a loop is seen on the next pass", async () => {
    const compute = async (steps) => {
      let value = 1;
      let write = null;
      const seen = [];
      for (const step of steps) {
        seen.push(value + 1);
        if (write) write(step * 10);
        await (write = (next) => (value = next));
      }
      return seen;
    };

    expect(await compute(pass([1, 2, 3]))).toEqual([2, 2, 21]);
  });

  test("concurrent calls keep separate bindings", async () => {
    const compute = async (a, delay) => {
      let value = a;
      for (const step of delay) {
        await Promise.resolve(step);
        value = value + a;
      }
      return value * 2;
    };

    const results = await Promise.all([compute(pass(1), pass([0, 0, 0])), compute(pass(10), pass([0]))]);

    expect(results).toEqual([8, 40]);
  });
});
