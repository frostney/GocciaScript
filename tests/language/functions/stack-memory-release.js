/*---
description: >
  The stack memory a deep recursion takes counts against the memory limit
  only while the recursion runs. Once it has returned, the stacks shrink back
  and the limit gets the memory back.
features: [stack-depth-limit, Goccia.gc]
---*/

const hasGoccia = typeof Goccia !== "undefined";

const deep = (n) => (n === 0 ? 0 : 1 + deep(n - 1));
const sumTo = (n) => (n === 0 ? 0 : n + sumTo(n - 1));
const step = (x) => x + 1;
// Runs a few thousand instructions, so that the periodic memory check runs
// at least once with the recursion gone.
const runAWhile = () => [...Array(4096).keys()].map(step).length;

describe.runIf(hasGoccia)("stack memory after a deep recursion", () => {
  test("is given back once the recursion has returned", () => {
    runAWhile();
    Goccia.gc();
    const before = Goccia.gc.bytesAllocated;

    expect(deep(2000)).toBe(2000);
    expect(runAWhile()).toBe(4096);
    Goccia.gc();

    expect(Goccia.gc.bytesAllocated - before).toBeLessThan(32 * 1024);
  });

  test("is given back after a recursion through try blocks", () => {
    // Each level holds an exception handler as well as its frame.
    const guarded = (n) => {
      try {
        return n === 0 ? 0 : 1 + guarded(n - 1);
      } catch (error) {
        throw error;
      }
    };
    runAWhile();
    Goccia.gc();
    const before = Goccia.gc.bytesAllocated;

    expect(guarded(2000)).toBe(2000);
    expect(runAWhile()).toBe(4096);
    Goccia.gc();

    // The handlers alone hold about 32 KiB at that depth.
    expect(Goccia.gc.bytesAllocated - before).toBeLessThan(16 * 1024);
  });

  test("is given back while a native callback that recursed is still running", () => {
    const results = [1, 2, 3].map((x) => {
      const depth = deep(2000);
      runAWhile();
      return depth + sumTo(1500) + x;
    });
    expect(results).toEqual([1127751, 1127752, 1127753]);
  });

  test("leaves a numeric self-recursion correct across shrinking and growing again", () => {
    expect(sumTo(2000)).toBe(2001000);
    runAWhile();
    expect(sumTo(2000)).toBe(2001000);
    runAWhile();
    expect(deep(2000) + sumTo(10)).toBe(2055);
  });
});
