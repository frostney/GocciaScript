/*---
description: a suspended continuation keeps its own function object reachable across a collection
features: [async-await, async-generators, generators, Goccia.gc]
---*/

// A generator object — including the continuation the bytecode VM builds for a
// plain async function at every `await` — clones the closure it resumes
// through, and that clone still borrows the function object which owns the
// original closure. Resumption reads the borrowed function object back for the
// execution realm and the global `this`, so a continuation has to hold its
// function object alive on its own: the moment nothing else refers to the
// function, the continuation is the only owner. Every case below therefore
// hands its function straight to a caller that keeps no reference, so the
// suspended continuation really is the last one standing, and forces two
// collections at each resumption so a survivor has to be genuinely rooted
// rather than merely not-yet-swept.
const collect = () => {
  Goccia.gc();
  Goccia.gc();
};

// Calls the function and drops it: the returned promise or iterator is the only
// thing the caller keeps.
const callAndDrop = (fn) => fn();

describe("suspended continuations under garbage collection", () => {
  test("an async function collects after its first resumption", async () => {
    const seen = [];
    await callAndDrop(async () => {
      await Promise.resolve();
      collect();
      seen.push("resumed");
    });
    expect(seen).toEqual(["resumed"]);
  });

  test("a collection before the first resumption keeps the body alive",
    async () => {
      const pending = callAndDrop(async () => {
        await Promise.resolve();
        return "late";
      });
      collect();
      expect(await pending).toBe("late");
    });

  test("every suspension point of one function survives a collection",
    async () => {
      const steps = [];
      await callAndDrop(async () => {
        await Promise.resolve();
        collect();
        steps.push(1);
        await Promise.resolve();
        collect();
        steps.push(2);
        await Promise.resolve();
        collect();
        steps.push(3);
      });
      expect(steps).toEqual([1, 2, 3]);
    });

  test("a nested await chain survives collections at every depth", async () => {
    const depths = [];
    const descend = (depth) =>
      callAndDrop(async () => {
        if (depth > 0) await descend(depth - 1);
        else await Promise.resolve();
        collect();
        depths.push(depth);
      });
    await descend(4);
    expect(depths).toEqual([0, 1, 2, 3, 4]);
  });

  test("interleaved continuations survive collections between resumptions",
    async () => {
      const observed = [];
      const chain = (tag) =>
        callAndDrop(async () => {
          await Promise.resolve();
          collect();
          observed.push(tag + "1");
          await Promise.resolve();
          collect();
          observed.push(tag + "2");
          return tag;
        });
      const settled = await Promise.all([chain("a"), chain("b"), chain("c")]);
      expect(settled).toEqual(["a", "b", "c"]);
      expect(observed.sort()).toEqual(["a1", "a2", "b1", "b2", "c1", "c2"]);
    });

  test("a collection inside a finally that follows an await is safe",
    async () => {
      const order = [];
      await callAndDrop(async () => {
        try {
          await Promise.resolve();
          collect();
          order.push("try");
        } finally {
          collect();
          order.push("finally");
        }
      });
      expect(order).toEqual(["try", "finally"]);
    });

  test("an async object method survives a collection after its receiver is gone",
    async () => {
      const pending = callAndDrop(() =>
        ({
          async probe() {
            await Promise.resolve();
            collect();
            return "method";
          },
        }).probe());
      collect();
      expect(await pending).toBe("method");
    });

  test("an async class method survives a collection after its class is gone",
    async () => {
      const pending = callAndDrop(() =>
        new (class {
          async probe() {
            await Promise.resolve();
            collect();
            return "class";
          }
        })().probe());
      collect();
      expect(await pending).toBe("class");
    });

  test("a frame that suspends at different depths survives collections",
    async () => {
      // The continuation keeps its storage between suspensions; what it holds
      // must follow the frame as the frame grows and shrinks.
      const total = await callAndDrop(async () => {
        const outer = { value: 1 };
        await Promise.resolve();
        collect();
        let inner = 0;
        {
          const a = { value: 2 };
          const b = { value: 3 };
          const c = { value: 4 };
          await Promise.resolve();
          collect();
          inner = a.value + b.value + c.value;
        }
        await Promise.resolve();
        collect();
        return outer.value + inner;
      });
      expect(total).toBe(10);
    });

  test("a for-await loop over an array survives collections between steps",
    async () => {
      const values = await callAndDrop(async () => {
        const seen = [];
        const thenable = { then: (resolve) => resolve({ tag: "thenable" }) };
        for await (const item of [{ tag: "a" }, Promise.resolve({ tag: "b" }),
          thenable, "c"]) {
          collect();
          seen.push(typeof item === "string" ? item : item.tag);
        }
        return seen;
      });
      expect(values).toEqual(["a", "b", "thenable", "c"]);
    });

  test("yield* over an array in an async generator survives collections",
    async () => {
      const iterator = callAndDrop(() =>
        ({
          async *items() {
            yield* [{ tag: "a" }, Promise.resolve({ tag: "b" }), { tag: "c" }];
          },
        }).items());
      collect();
      const values = [];
      for await (const item of iterator) {
        collect();
        values.push(item.tag);
      }
      expect(values).toEqual(["a", "b", "c"]);
    });

  test("a sync generator resumes after collections between its yields", () => {
    const iterator = callAndDrop(() =>
      ({
        *counter() {
          yield 1;
          collect();
          yield 2;
          collect();
          yield 3;
        },
      }).counter());
    collect();
    expect([...iterator]).toEqual([1, 2, 3]);
  });

  test("an async generator resumes after collections between its yields",
    async () => {
      const iterator = callAndDrop(() =>
        ({
          async *counter() {
            yield 1;
            collect();
            await Promise.resolve();
            yield 2;
            collect();
          },
        }).counter());
      collect();
      const values = [];
      for await (const value of iterator) {
        collect();
        values.push(value);
      }
      expect(values).toEqual([1, 2]);
    });
});
