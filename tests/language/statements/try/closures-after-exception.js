/*---
description: Closures over a block left by an exception keep that block's variables, and later variables stay independent of them
features: [for-of, generators, async-await, explicit-resource-management]
---*/

const id = (value) => value;

describe("closures over a block left by an exception", () => {
  test("a nested block left by a throw", () => {
    const callbacks = [];
    try {
      {
        let captured = id(1);
        callbacks.push(() => captured, () => { captured = captured + 100; });
        throw new Error("leave the block");
      }
    } catch (error) {}
    {
      const first = id(7);
      const second = id(8);
      const third = id(9);
      callbacks[1]();
      expect([first, second, third]).toEqual([7, 8, 9]);
      expect(callbacks[0]()).toBe(101);
    }
  });

  test("the try block itself left by a throw", () => {
    const callbacks = [];
    try {
      let captured = id(1);
      callbacks.push(() => captured, () => { captured = captured + 100; });
      throw new Error("leave the block");
    } catch (error) {}
    {
      const first = id(7);
      const second = id(8);
      const third = id(9);
      callbacks[1]();
      expect([first, second, third]).toEqual([7, 8, 9]);
      expect(callbacks[0]()).toBe(101);
    }
  });

  test("a throw from a called function", () => {
    const callbacks = [];
    const fail = () => {
      throw new Error("leave the block");
    };
    try {
      {
        let captured = id(1);
        callbacks.push(() => captured, () => { captured = captured + 100; });
        fail();
      }
    } catch (error) {}
    {
      const first = id(7);
      const second = id(8);
      const third = id(9);
      callbacks[1]();
      expect([first, second, third]).toEqual([7, 8, 9]);
      expect(callbacks[0]()).toBe(101);
    }
  });

  test("an error raised by the engine", () => {
    const callbacks = [];
    const missing = id(null);
    try {
      {
        let captured = id(1);
        callbacks.push(() => captured, () => { captured = captured + 100; });
        missing.property;
      }
    } catch (error) {
      expect(error instanceof TypeError).toBe(true);
    }
    {
      const first = id(7);
      const second = id(8);
      const third = id(9);
      callbacks[1]();
      expect([first, second, third]).toEqual([7, 8, 9]);
      expect(callbacks[0]()).toBe(101);
    }
  });

  test("the catch parameter does not replace the captured variable", () => {
    const callbacks = [];
    try {
      let captured = id(1);
      callbacks.push(() => captured);
      throw new Error("leave the block");
    } catch (error) {
      expect(error.message).toBe("leave the block");
    }
    expect(callbacks[0]()).toBe(1);
  });

  test("a finally block that runs while the exception propagates", () => {
    const callbacks = [];
    const seen = [];
    try {
      try {
        {
          let captured = id(1);
          callbacks.push(() => captured, () => { captured = captured + 100; });
          throw new Error("leave the block");
        }
      } finally {
        const first = id(7);
        const second = id(8);
        const third = id(9);
        callbacks[1]();
        seen.push(first, second, third);
      }
    } catch (error) {}
    expect(seen).toEqual([7, 8, 9]);
    expect(callbacks[0]()).toBe(101);
  });

  test("a catch block left by a throw runs its finally block", () => {
    const callbacks = [];
    const seen = [];
    try {
      try {
        throw new Error("first");
      } catch {
        let captured = id(1);
        callbacks.push(() => captured, () => { captured = captured + 100; });
        throw new Error("second");
      } finally {
        const first = id(7);
        const second = id(8);
        const third = id(9);
        callbacks[1]();
        seen.push(first, second, third);
      }
    } catch (error) {}
    expect(seen).toEqual([7, 8, 9]);
    expect(callbacks[0]()).toBe(101);
  });

  test("a captured catch parameter whose catch block throws", () => {
    const callbacks = [];
    try {
      try {
        throw 1;
      } catch (inner) {
        callbacks.push(() => inner, () => { inner = inner + 100; });
        throw new Error("second");
      }
    } catch (error) {}
    {
      const first = id(7);
      const second = id(8);
      const third = id(9);
      const fourth = id(10);
      callbacks[1]();
      expect([first, second, third, fourth]).toEqual([7, 8, 9, 10]);
      expect(callbacks[0]()).toBe(101);
    }
  });

  test("nested blocks left by one rethrown exception", () => {
    const callbacks = [];
    try {
      {
        let outer = id(1);
        callbacks.push(() => outer, () => { outer = outer + 100; });
        try {
          {
            let inner = id(2);
            callbacks.push(() => inner, () => { inner = inner + 200; });
            throw new Error("leave both blocks");
          }
        } catch (error) {
          throw error;
        }
      }
    } catch (error) {}
    {
      const first = id(7);
      const second = id(8);
      const third = id(9);
      const fourth = id(10);
      const fifth = id(11);
      callbacks[1]();
      callbacks[3]();
      expect([first, second, third, fourth, fifth]).toEqual([7, 8, 9, 10, 11]);
      expect(callbacks[0]()).toBe(101);
      expect(callbacks[2]()).toBe(202);
    }
  });

  test("an enclosing block that is still running keeps its variable shared", () => {
    const callbacks = [];
    {
      let live = id(1);
      callbacks.push(() => live, () => { live = live + 100; });
      try {
        {
          let dead = id(2);
          callbacks.push(() => dead, () => { dead = dead + 200; });
          throw new Error("leave the inner block");
        }
      } catch (error) {}
      live = live + 1;
      callbacks[1]();
      const first = id(7);
      const second = id(8);
      callbacks[3]();
      expect(live).toBe(102);
      expect(callbacks[0]()).toBe(102);
      expect([first, second]).toEqual([7, 8]);
      expect(callbacks[2]()).toBe(202);
    }
  });

  test("each loop iteration that throws keeps its own variable", () => {
    const getters = [];
    for (const value of [1, 2, 3]) {
      try {
        let captured = value * 10;
        getters.push(() => captured);
        throw new Error("leave the block");
      } catch (error) {}
    }
    expect(getters.map((getter) => getter())).toEqual([10, 20, 30]);
  });

  test("a loop body left by a throw", () => {
    const callbacks = [];
    try {
      for (const value of [1, 2, 3]) {
        let captured = id(value);
        callbacks.push(() => captured, () => { captured = captured + 100; });
        throw new Error("leave the loop");
      }
    } catch (error) {}
    {
      const first = id(7);
      const second = id(8);
      const third = id(9);
      const fourth = id(10);
      const fifth = id(11);
      const sixth = id(12);
      const seventh = id(13);
      callbacks[1]();
      expect([first, second, third, fourth, fifth, sixth, seventh])
        .toEqual([7, 8, 9, 10, 11, 12, 13]);
      expect(callbacks[0]()).toBe(101);
    }
  });

  test("a block with a using declaration left by a throw", () => {
    const callbacks = [];
    const log = [];
    try {
      {
        using resource = { [Symbol.dispose]() { log.push("disposed"); } };
        let captured = id(1);
        callbacks.push(() => captured, () => { captured = captured + 100; });
        throw new Error("leave the block");
      }
    } catch (error) {}
    {
      const first = id(7);
      const second = id(8);
      const third = id(9);
      const fourth = id(10);
      callbacks[1]();
      expect([first, second, third, fourth]).toEqual([7, 8, 9, 10]);
      expect(callbacks[0]()).toBe(101);
      expect(log).toEqual(["disposed"]);
    }
  });

  test("a generator resumed with throw()", () => {
    const generators = {
      *run(callbacks, seen) {
        try {
          {
            let captured = id(1);
            callbacks.push(() => captured, () => { captured = captured + 100; });
            yield "paused";
          }
        } catch (error) {}
        {
          const first = id(7);
          const second = id(8);
          const third = id(9);
          callbacks[1]();
          seen.push(first, second, third);
        }
      },
    };
    const callbacks = [];
    const seen = [];
    const iterator = generators.run(callbacks, seen);
    expect(iterator.next().value).toBe("paused");
    expect(iterator.throw(new Error("leave the block")).done).toBe(true);
    expect(seen).toEqual([7, 8, 9]);
    expect(callbacks[0]()).toBe(101);
  });

  test("a generator resumed with return() runs its finally block", () => {
    const generators = {
      *run(callbacks, seen) {
        try {
          {
            let captured = id(1);
            callbacks.push(() => captured, () => { captured = captured + 100; });
            yield "paused";
          }
        } finally {
          const first = id(7);
          const second = id(8);
          const third = id(9);
          callbacks[1]();
          seen.push(first, second, third);
        }
      },
    };
    const callbacks = [];
    const seen = [];
    const iterator = generators.run(callbacks, seen);
    expect(iterator.next().value).toBe("paused");
    expect(iterator.return("stopped")).toEqual({ value: "stopped", done: true });
    expect(seen).toEqual([7, 8, 9]);
    expect(callbacks[0]()).toBe(101);
  });

  test("an async function whose awaited promise rejects", async () => {
    const callbacks = [];
    const run = async () => {
      try {
        {
          let captured = id(1);
          callbacks.push(() => captured, () => { captured = captured + 100; });
          await Promise.reject(new Error("leave the block"));
        }
      } catch (error) {}
      {
        const first = id(7);
        const second = id(8);
        const third = id(9);
        callbacks[1]();
        return [first, second, third];
      }
    };
    expect(await run()).toEqual([7, 8, 9]);
    expect(callbacks[0]()).toBe(101);
  });
});
