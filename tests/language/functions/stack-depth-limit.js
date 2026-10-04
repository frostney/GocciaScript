/*---
description: Infinite recursion throws RangeError when stack depth limit is set
features: [stack-depth-limit]
---*/

test("infinite recursion throws RangeError", () => {
  const recurse = () => recurse();
  expect(() => recurse()).toThrow(RangeError);
});

test("mutual recursion throws RangeError", () => {
  let ping, pong;
  ping = () => pong();
  pong = () => ping();
  expect(() => ping()).toThrow(RangeError);
});

test("error message matches V8 convention", () => {
  const recurse = () => recurse();
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
  const deep = () => deep();
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

describe("the default limit allows exactly 2,200 nested calls", () => {
  // The test runner's default --max-stack. The runner calls each test
  // function itself, so the test function is not one of the nested calls.
  const LIMIT = 2200;
  const count = (n) => (n <= 1 ? 1 : count(n - 1) + 1);

  test("plain calls", () => {
    expect(count(LIMIT)).toBe(LIMIT);
    expect(() => count(LIMIT + 1)).toThrow(RangeError);
  });

  test("method calls", () => {
    const counter = {
      count(n) {
        return n <= 1 ? 1 : this.count(n - 1) + 1;
      },
    };
    expect(counter.count(LIMIT)).toBe(LIMIT);
    expect(() => counter.count(LIMIT + 1)).toThrow(RangeError);
  });

  test("closed numeric self-calls", () => {
    // The outer arrow is the first call. Called with a number literal,
    // numeric's calls to itself compile to OP_CALL_SELF_NUM in bytecode.
    const below = () => {
      const numeric = (k) => (k <= 1 ? k : numeric(k - 1) + 1);
      return numeric(2199) + 1;
    };
    const above = () => {
      const numeric = (k) => (k <= 1 ? k : numeric(k - 1) + 1);
      return numeric(2200) + 1;
    };
    expect(below()).toBe(LIMIT);
    expect(() => above()).toThrow(RangeError);
  });

  test("calls from a native callback", () => {
    // map and the callback it calls count as one call between them.
    expect([LIMIT].map(count)).toEqual([LIMIT]);
    expect(() => [LIMIT + 1].map(count)).toThrow(RangeError);
  });

  test("calls from a getter", () => {
    let depth = 0;
    const holder = {
      get value() {
        return count(depth);
      },
    };
    depth = LIMIT;
    expect(holder.value).toBe(LIMIT);
    depth = LIMIT + 1;
    expect(() => holder.value).toThrow(RangeError);
  });

  test("calls from a generator", () => {
    // next() is the first call.
    const source = {
      *values(n) {
        yield count(n);
      },
    };
    expect(source.values(LIMIT - 1).next().value).toBe(LIMIT - 1);
    expect(() => source.values(LIMIT).next()).toThrow(RangeError);
  });

  test("calls after an await", async () => {
    await Promise.resolve();
    expect(count(LIMIT)).toBe(LIMIT);
    expect(() => count(LIMIT + 1)).toThrow(RangeError);
  });
});
