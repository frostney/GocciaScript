/*---
description: An async function that awaits repeatedly resumes each activation with its own state and keeps spec microtask order
features: [async-await]
---*/

// A body resumed from one await suspends again at the next. Each activation
// must keep its own registers, `this` and arguments however many times it
// suspends, also while another activation of the same function is suspended
// or running, and every resumption must take exactly the microtask ticks
// ES2026 §27.7.5.3 Await gives it.

const ticker = (log, count) => {
  let chain = Promise.resolve();
  for (const index of Array.from({ length: count }, (_, i) => i)) {
    chain = chain.then(() => {
      log.push("t" + index);
    });
  }
  return chain;
};

describe("repeated awaits", () => {
  test("a loop of awaits accumulates across every resumption", async () => {
    const sum = async (values) => {
      let total = 0;
      for (const value of values) {
        total = total + (await value);
      }
      return total;
    };
    const values = Array.from({ length: 50 }, (_, i) =>
      i % 2 === 0 ? i : Promise.resolve(i));
    expect(await sum(values)).toBe(1225);
  });

  test("concurrent activations of one function keep their own state",
    async () => {
      const step = async (tag, count) => {
        const seen = [];
        for (const index of Array.from({ length: count }, (_, i) => i)) {
          await null;
          seen.push(tag + index);
        }
        return seen.join(",");
      };
      const results = await Promise.all([step("a", 3), step("b", 2),
        step("c", 4)]);
      expect(results).toEqual(["a0,a1,a2", "b0,b1", "c0,c1,c2,c3"]);
    });

  test("a method keeps this and its arguments across awaits", async () => {
    const counter = {
      base: 10,
      async add(first, ...rest) {
        await null;
        const partial = this.base + first;
        await Promise.resolve();
        return partial + rest[0] + rest.length;
      },
    };
    expect(await counter.add(1, 2, 3)).toBe(15);
  });

  test("nested async calls resume in spec order", async () => {
    const log = [];
    const ticks = ticker(log, 10);
    const inner = async (x) => {
      await null;
      log.push("in" + x);
      await null;
      return x;
    };
    const outer = async () => {
      let total = 0;
      for (const i of [1, 2]) {
        total += await inner(i);
        log.push("o" + i);
        await undefined;
      }
      return total;
    };
    log.push("r" + await outer());
    await ticks;
    expect(log).toEqual(["t0", "in1", "t1", "t2", "o1", "t3", "t4", "in2",
      "t5", "t6", "o2", "t7", "t8", "r3", "t9"]);
  });

  test("a recursive async function resumes each depth in spec order",
    async () => {
      const log = [];
      const ticks = ticker(log, 8);
      const depth = async (n) => {
        if (n === 0) return 0;
        await null;
        const below = await depth(n - 1);
        log.push("f" + n);
        return below + n;
      };
      log.push("r" + await depth(3));
      await ticks;
      expect(log).toEqual(["t0", "t1", "t2", "t3", "f1", "t4", "f2", "t5",
        "f3", "t6", "r6", "t7"]);
    });
});
