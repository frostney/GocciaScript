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
  deep(3000);
  expect(count).toBe(3001);
});

test("mutual recursion with return values", () => {
  let isEven, isOdd;
  isEven = (n) => n === 0 ? true : isOdd(n - 1);
  isOdd = (n) => n === 0 ? false : isEven(n - 1);
  expect(isEven(100)).toBe(true);
  expect(isOdd(101)).toBe(true);
  expect(isEven(99)).toBe(false);
});

test("exception propagates through trampolined frames", () => {
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

test("try-catch works across trampolined frames", () => {
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
