/*---
description: A call in a return statement's tail position reuses the caller's frame, so recursion through it is bounded by neither the stack limit nor the engine's frames
features: [tail-call-optimization]
---*/

// ES2026 §15.10.1 IsInTailPosition and §15.10.2 HasCallInTailPosition: a
// return's call is in tail position in strict code outside generators and
// async functions, unless a try block, a for-of loop or a using declaration
// still has work to do after it returns. A catch block without a finally, and
// a finally block, are tail positions. DEPTH is deeper than any engine's frame
// bound, so each test passes only if the frames are reused.

const DEPTH = 300000;

describe("a return's call in tail position", () => {
  test("self recursion", () => {
    const countDown = (n) => {
      return n === 0 ? "done" : countDown(n - 1);
    };
    expect(countDown(DEPTH)).toBe("done");
  });

  test("mutual recursion", () => {
    let isOdd;
    const isEven = (n) => {
      return n === 0 ? true : isOdd(n - 1);
    };
    isOdd = (n) => {
      return n === 0 ? false : isEven(n - 1);
    };
    expect(isEven(DEPTH)).toBe(true);
    expect(isOdd(DEPTH + 1)).toBe(true);
  });

  test("a method call keeps its receiver", () => {
    const counter = {
      steps: 0,
      countDown(n) {
        this.steps++;
        return n === 0 ? this.steps : this.countDown(n - 1);
      },
    };
    expect(counter.countDown(DEPTH)).toBe(DEPTH + 1);
  });

  test("spread arguments", () => {
    const countDown = (...args) => {
      return args[0] === 0 ? args.length : countDown(...[args[0] - 1, "extra"]);
    };
    expect(countDown(DEPTH)).toBe(2);
  });

  test("a tagged template", () => {
    const countDown = (strings, n) => {
      return n === 0 ? strings.length : countDown`${n - 1}`;
    };
    expect(countDown`${DEPTH}`).toBe(2);
  });

  test("an optional call", () => {
    const holder = {
      countDown(n) {
        return n === 0 ? "done" : holder.countDown?.(n - 1);
      },
    };
    expect(holder.countDown(DEPTH)).toBe("done");
  });

  test("the right operand of && and ||", () => {
    const andDown = (n) => {
      return n !== 0 && andDown(n - 1);
    };
    const orDown = (n) => {
      return n === 0 || orDown(n - 1);
    };
    expect(andDown(DEPTH)).toBe(false);
    expect(orDown(DEPTH)).toBe(true);
  });

  test("a catch block without a finally", () => {
    const countDown = (n) => {
      try {
        throw new Error("caught");
      } catch {
        return n === 0 ? "done" : countDown(n - 1);
      }
    };
    expect(countDown(DEPTH)).toBe("done");
  });

  test("a finally block", () => {
    const countDown = (n) => {
      try {
        n = n - 0;
      } finally {
        return n === 0 ? "done" : countDown(n - 1);
      }
    };
    expect(countDown(DEPTH)).toBe("done");
  });

  test("a callee's closure keeps the values it captured from the frame it replaced", () => {
    const chain = (n, acc) => {
      const captured = n;
      const read = () => captured;
      return n === 0 ? acc.map((f) => f()).slice(0, 3) : chain(n - 1, n % 100000 === 0 ? [read, ...acc] : acc);
    };
    expect(chain(DEPTH, [])).toEqual([100000, 200000, 300000]);
  });

  test("a constructor's result still goes through its checks", () => {
    class Box {
      constructor(value) {
        this.value = value;
      }
      static make(n) {
        return n === 0 ? new Box("made") : Box.make(n - 1);
      }
    }
    expect(Box.make(DEPTH).value).toBe("made");
  });
});

describe("a return's call outside tail position keeps its frame", () => {
  test("inside a try block", () => {
    const countDown = (n) => {
      try {
        return n === 0 ? "done" : countDown(n - 1);
      } catch (error) {
        throw error;
      }
    };
    expect(() => countDown(DEPTH)).toThrow(RangeError);
  });

  test("inside a for-of loop, which closes its iterator after the call", () => {
    const countDown = (n) => {
      for (const unused of [0]) {
        return n === 0 ? "done" : countDown(n - 1);
      }
      return "unreached";
    };
    expect(() => countDown(DEPTH)).toThrow(RangeError);
  });

  test("an operand of an operator", () => {
    const count = (n) => {
      return n === 0 ? 0 : 1 + count(n - 1);
    };
    expect(() => count(DEPTH)).toThrow(RangeError);
  });

  test("in an async function", async () => {
    const countDown = async (n) => {
      return n === 0 ? "done" : countDown(n - 1);
    };
    await expect(countDown(DEPTH)).rejects.toThrow(RangeError);
  });
});
