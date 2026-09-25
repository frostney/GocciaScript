/*---
description: setTimeout, setInterval and their clear counterparts over the virtual timer queue
features: [Timers]
---*/

import {
  useFakeTimers,
  useRealTimers,
  setSystemTime,
  advanceTimersByTime,
  getTimerCount,
} from "goccia:timers";

// The queue is virtual: nothing here waits on wall time, and a delay is an
// ordering key on a clock the test moves. Every expectation was probed against
// Vitest 4.1.10, whose fake timers wrap @sinonjs/fake-timers.

describe("scheduling", () => {
  afterEach(() => {
    useRealTimers();
  });

  test("timers fire in due-time order, then in registration order", () => {
    useFakeTimers();
    setSystemTime(0);
    const log = [];
    setTimeout(() => log.push("late"), 20);
    setTimeout(() => log.push("early-a"), 10);
    setTimeout(() => log.push("early-b"), 10);
    advanceTimersByTime(20);

    expect(log).toEqual(["early-a", "early-b", "late"]);
  });

  test("a zero, missing or negative delay is due immediately", () => {
    useFakeTimers();
    setSystemTime(0);
    const log = [];
    setTimeout(() => log.push("zero"), 0);
    setTimeout(() => log.push("missing"));
    setTimeout(() => log.push("negative"), -5);
    setTimeout(() => log.push("one"), 1);
    advanceTimersByTime(0);

    expect(log).toEqual(["zero", "missing", "negative"]);

    advanceTimersByTime(1);
    expect(log).toEqual(["zero", "missing", "negative", "one"]);
  });

  test("a zero-delay timer scheduled from inside a callback lands on the next millisecond", () => {
    useFakeTimers();
    setSystemTime(0);
    const log = [];
    setTimeout(() => {
      log.push("outer@" + Date.now());
      setTimeout(() => log.push("inner@" + Date.now()), 0);
    }, 5);

    // The chain does not collapse onto the instant it started, which is what
    // keeps a self-rescheduling zero-delay timer from looping inside one
    // advance.
    advanceTimersByTime(5);
    expect(log).toEqual(["outer@5"]);

    advanceTimersByTime(1);
    expect(log).toEqual(["outer@5", "inner@6"]);
  });

  test("extra arguments reach the callback", () => {
    useFakeTimers();
    const seen = [];
    setTimeout((first, second) => seen.push([first, second]), 1, "x", 2);
    advanceTimersByTime(1);

    expect(seen).toEqual([["x", 2]]);
  });

  test("clearTimeout cancels a pending timer, from outside and from a callback", () => {
    useFakeTimers();
    const log = [];
    const cancelled = setTimeout(() => log.push("cancelled"), 10);
    setTimeout(() => {
      log.push("canceller");
      clearTimeout(cancelled);
    }, 5);
    advanceTimersByTime(20);

    expect(log).toEqual(["canceller"]);
  });

  test("clearing an id that is absent, falsy or undefined is a no-op", () => {
    useFakeTimers();
    expect(() => clearTimeout(undefined)).not.toThrow();
    expect(() => clearTimeout(0)).not.toThrow();
    expect(() => clearTimeout(999999)).not.toThrow();
    expect(() => clearInterval(undefined)).not.toThrow();
  });

  test("either clear name cancels either kind, as the fake clock allows", () => {
    useFakeTimers();
    const log = [];
    const timeoutId = setTimeout(() => log.push("timeout"), 5);
    const intervalId = setInterval(() => log.push("interval"), 5);
    clearInterval(timeoutId);
    clearTimeout(intervalId);
    advanceTimersByTime(20);

    expect(log).toEqual([]);
    expect(getTimerCount()).toBe(0);
  });

  test("a timer id is a number", () => {
    useFakeTimers();
    const id = setTimeout(() => {}, 5);

    expect(typeof id).toBe("number");
    clearTimeout(id);
  });

  test("setTimeout rejects a non-callable callback", () => {
    useFakeTimers();
    expect(() => setTimeout()).toThrow(TypeError);
    expect(() => setTimeout("log('hi')", 1)).toThrow(TypeError);
  });
});

describe("intervals", () => {
  afterEach(() => {
    useRealTimers();
  });

  test("an interval reschedules from its previous due time", () => {
    useFakeTimers();
    setSystemTime(0);
    const log = [];
    const id = setInterval(() => log.push(Date.now()), 10);
    advanceTimersByTime(35);
    clearInterval(id);

    expect(log).toEqual([10, 20, 30]);
  });

  test("a single advance fires every tick the interval crossed", () => {
    useFakeTimers();
    setSystemTime(0);
    const log = [];
    const id = setInterval(() => log.push(Date.now()), 10);
    advanceTimersByTime(50);
    clearInterval(id);

    expect(log).toEqual([10, 20, 30, 40, 50]);
  });

  test("an interval that clears itself stops and leaves nothing pending", () => {
    useFakeTimers();
    setSystemTime(0);
    let runs = 0;
    const id = setInterval(() => {
      runs += 1;
      if (runs === 3) clearInterval(id);
    }, 10);
    advanceTimersByTime(100);

    expect(runs).toBe(3);
    expect(getTimerCount()).toBe(0);
  });
});
