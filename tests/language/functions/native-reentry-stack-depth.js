/*---
description: Recursion through calls the bytecode VM enters natively is bounded by the call stack limit, not a fixed cap of 512
features: [stack-depth-limit]
---*/

// Constructors, async functions, generators, accessors and callbacks from
// built-ins run on a native re-entry of the bytecode VM. Each one is a nested
// call like any other, so the test runner's default limit of 2,200 nested
// calls applies to them, not a fixed cap of 512 native re-entries.
const DEPTH = 1000;

describe("recursion through native re-entry", () => {
  test("constructors", () => {
    let depth = 0;
    class Node {
      constructor(n) {
        depth++;
        this.next = n > 1 ? new Node(n - 1) : null;
      }
    }
    new Node(DEPTH);
    expect(depth).toBe(DEPTH);
  });

  test("derived class constructors", () => {
    let depth = 0;
    class Base {}
    class Derived extends Base {
      constructor(n) {
        super();
        depth++;
        if (n > 1) new Derived(n - 1);
      }
    }
    new Derived(DEPTH);
    expect(depth).toBe(DEPTH);
  });

  test("async functions", async () => {
    let depth = 0;
    const descend = async (n) => {
      depth++;
      if (n > 1) await descend(n - 1);
    };
    await descend(DEPTH);
    expect(depth).toBe(DEPTH);
  });

  test("generator next()", () => {
    let depth = 0;
    const source = {
      *values(n) {
        depth++;
        if (n > 1) source.values(n - 1).next();
        yield n;
      },
    };
    expect(source.values(DEPTH).next().value).toBe(DEPTH);
    expect(depth).toBe(DEPTH);
  });

  test("getters", () => {
    let remaining = DEPTH;
    const chain = {
      get depth() {
        remaining--;
        return remaining > 0 ? chain.depth + 1 : 1;
      },
    };
    expect(chain.depth).toBe(DEPTH);
  });

  test("callbacks from a built-in", () => {
    // Two calls per level: count and the callback map makes.
    const count = (n) => (n > 1 ? [n - 1].map(count)[0] + 1 : 1);
    expect(count(DEPTH)).toBe(DEPTH);
  });
});

describe("recursion through native re-entry without end", () => {
  test("constructors throw RangeError", () => {
    class Endless {
      constructor() {
        new Endless();
      }
    }
    expect(() => new Endless()).toThrow(RangeError);
  });

  test("async functions reject with RangeError", async () => {
    const endless = async () => {
      await endless();
    };
    let caught;
    try {
      await endless();
    } catch (e) {
      caught = e;
    }
    expect(caught instanceof RangeError).toBe(true);
    expect(caught.message).toBe("Maximum call stack size exceeded");
  });

  test("generator next() throws RangeError", () => {
    const source = {
      *values() {
        source.values().next();
        yield 1;
      },
    };
    expect(() => source.values().next()).toThrow(RangeError);
  });
});
