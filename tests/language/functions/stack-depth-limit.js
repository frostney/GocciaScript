/*---
description: Infinite recursion throws RangeError when stack depth limit is set
features: [stack-depth-limit]
---*/

// Each recursion below calls from a block body, where the call is not in tail
// position (ES2026 §15.10.2), so every call keeps its frame. A concise body
// such as `() => recurse()` would be a proper tail call and never overflow.
test("infinite recursion throws RangeError", () => {
  const recurse = () => {
    recurse();
  };
  expect(() => recurse()).toThrow(RangeError);
});

test("mutual recursion throws RangeError", () => {
  let ping, pong;
  ping = () => {
    pong();
  };
  pong = () => {
    ping();
  };
  expect(() => ping()).toThrow(RangeError);
});

test("error message matches V8 convention", () => {
  const recurse = () => {
    recurse();
  };
  let caught;
  try {
    recurse();
  } catch (e) {
    caught = e;
  }
  expect(caught).toBeDefined();
  expect(caught instanceof RangeError).toBe(true);
  expect(caught.message).toBe("Maximum call stack size exceeded");
});

test("recursion within limit succeeds", () => {
  let count = 0;
  const countDown = (n) => {
    count++;
    if (n <= 0) return;
    countDown(n - 1);
  };
  countDown(50);
  expect(count).toBe(51);
});

test("deep recursion succeeds within limit", () => {
  let count = 0;
  const deep = (n) => {
    count++;
    if (n > 0) deep(n - 1);
  };
  deep(2000);
  expect(count).toBe(2001);
});

test("mutual recursion with return values", () => {
  let isEven, isOdd;
  isEven = (n) => n === 0 ? true : isOdd(n - 1);
  isOdd = (n) => n === 0 ? false : isEven(n - 1);
  expect(isEven(100)).toBe(true);
  expect(isOdd(101)).toBe(true);
  expect(isEven(99)).toBe(false);
});

test("exception propagates through nested calls", () => {
  const inner = () => { throw new Error("boom"); };
  const middle = () => inner();
  const outer = () => middle();
  expect(() => outer()).toThrow(Error);
  try {
    outer();
  } catch (e) {
    expect(e.message).toBe("boom");
  }
});

test("try-catch works across nested calls", () => {
  const thrower = (n) => {
    if (n === 0) throw new RangeError("done");
    return thrower(n - 1);
  };
  let caught;
  try {
    thrower(100);
  } catch (e) {
    caught = e;
  }
  expect(caught instanceof RangeError).toBe(true);
  expect(caught.message).toBe("done");
});

test("error has stack trace", () => {
  const deep = () => {
    deep();
  };
  let caught;
  try {
    deep();
  } catch (e) {
    caught = e;
  }
  expect(caught).toBeDefined();
  expect(caught.stack).toBeDefined();
  expect(caught.stack.includes("deep")).toBe(true);
});

test("closed numeric scalar recursion preserves the stack limit and trace", () => {
  const run = () => {
    const numeric = (n) => numeric(n - 1) + 0;
    return numeric(1);
  };
  let caught;
  try {
    run();
  } catch (e) {
    caught = e;
  }
  expect(caught instanceof RangeError).toBe(true);
  expect(caught.message).toBe("Maximum call stack size exceeded");
  expect(caught.stack.includes("numeric")).toBe(true);
});

describe("the default limit allows 200,000 nested calls", () => {
  // The engine counts frames from the entry's, and the testing library calls
  // each test function from JavaScript, so a test function starts a few
  // frames down. Each test therefore measures how deep a kind of call goes,
  // through the same helper, and compares the kinds.
  const count = (n) => (n <= 1 ? 1 : count(n - 1) + 1);
  // The deepest n for which run(n) completes, by bisection.
  const maxDepth = (run) => {
    let low = 1;
    let high = 400000;
    for (const _ of Array.from({ length: 40 })) {
      if (low >= high) break;
      const middle = Math.ceil((low + high) / 2);
      let completed = true;
      try {
        run(middle);
      } catch (error) {
        if (!(error instanceof RangeError)) throw error;
        completed = false;
      }
      if (completed) low = middle;
      else high = middle - 1;
    }
    return low;
  };

  test("plain calls reach nearly 200,000 frames, and no further", () => {
    const depth = maxDepth(count);
    expect(depth > 199900).toBe(true);
    expect(depth <= 200000).toBe(true);
  });

  test("method calls", () => {
    const counter = {
      count(n) {
        return n <= 1 ? 1 : this.count(n - 1) + 1;
      },
    };
    // The arrow that calls the method is one of the nested calls.
    expect(maxDepth((n) => counter.count(n))).toBe(maxDepth(count) - 1);
  });

  test("closed numeric self-calls", () => {
    // The outer arrow is the first call; numeric's calls to itself are
    // the rest.
    const outer = (m) => {
      const numeric = (k) => (k <= 1 ? k : numeric(k - 1) + 1);
      return numeric(m - 1) + 1;
    };
    expect(maxDepth(outer)).toBe(maxDepth(count));
  });

  test("calls from a native callback", () => {
    // The arrow is one call; map and the callback it calls count as one.
    expect(maxDepth((n) => [n].map(count))).toBe(maxDepth(count) - 1);
  });

  test("calls from a generator", () => {
    const source = {
      *values(n) {
        yield count(n);
      },
    };
    // The arrow and next() are one call each.
    expect(maxDepth((n) => source.values(n).next())).toBe(maxDepth(count) - 2);
  });

  test("calls after an await", async () => {
    await Promise.resolve();
    const depth = maxDepth(count);
    expect(depth > 199900).toBe(true);
    expect(depth <= 200000).toBe(true);
  });
});
