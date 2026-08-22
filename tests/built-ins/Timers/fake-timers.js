/*---
description: the goccia:timers control surface — advancing, running and the mocked system clock
features: [Timers]
---*/

import {
  useFakeTimers,
  useRealTimers,
  isFakeTimers,
  setSystemTime,
  getMockedSystemTime,
  getRealSystemTime,
  advanceTimersByTime,
  advanceTimersByTimeAsync,
  advanceTimersToNextTimer,
  runAllTimers,
  runOnlyPendingTimers,
  clearAllTimers,
  getTimerCount,
} from "goccia:timers";

describe("advancing", () => {
  afterEach(() => {
    useRealTimers();
  });

  test("a synchronous advance runs no microtasks between timers", () => {
    useFakeTimers();
    setSystemTime(0);
    const log = [];
    setTimeout(() => {
      log.push("first");
      Promise.resolve().then(() => log.push("microtask"));
    }, 10);
    setTimeout(() => log.push("second"), 20);
    advanceTimersByTime(25);

    // The microtask the first timer queued is still waiting: the synchronous
    // advance never yields, so it cannot land between the two timers.
    expect(log).toEqual(["first", "second"]);
  });

  test("an asynchronous advance drains microtasks around every timer", async () => {
    useFakeTimers();
    setSystemTime(0);
    const log = [];
    setTimeout(() => {
      log.push("first");
      Promise.resolve().then(() => log.push("after-first"));
    }, 10);
    setTimeout(() => {
      log.push("second");
      Promise.resolve().then(() => log.push("after-second"));
    }, 20);
    Promise.resolve().then(() => log.push("pending"));
    await advanceTimersByTimeAsync(25);

    expect(log).toEqual([
      "pending",
      "first",
      "after-first",
      "second",
      "after-second",
    ]);
  });

  test("advanceTimersToNextTimer fires every timer due at that instant", () => {
    useFakeTimers();
    setSystemTime(0);
    const log = [];
    setTimeout(() => log.push("a@" + Date.now()), 10);
    setTimeout(() => log.push("b@" + Date.now()), 20);
    setTimeout(() => log.push("c@" + Date.now()), 20);

    advanceTimersToNextTimer();
    expect(log).toEqual(["a@10"]);
    expect(Date.now()).toBe(10);

    advanceTimersToNextTimer();
    expect(log).toEqual(["a@10", "b@20", "c@20"]);
    expect(Date.now()).toBe(20);
  });

  test("a negative advance is refused", () => {
    useFakeTimers();
    expect(() => advanceTimersByTime(-1)).toThrow(
      "Negative ticks are not supported",
    );
  });

  test("a throwing callback does not stop the timers behind it", () => {
    useFakeTimers();
    setSystemTime(0);
    const log = [];
    setTimeout(() => {
      log.push("throwing");
      throw new Error("boom");
    }, 5);
    setTimeout(() => log.push("later"), 10);

    // The error surfaces once the advance is over, and the clock still went
    // the whole distance it was asked to.
    expect(() => advanceTimersByTime(20)).toThrow("boom");
    expect(log).toEqual(["throwing", "later"]);
    expect(Date.now()).toBe(20);
  });

  test("stepping to a single timer stops at a throwing callback", () => {
    useFakeTimers();
    setSystemTime(0);
    const log = [];
    setTimeout(() => {
      log.push("throwing");
      throw new Error("boom");
    }, 5);
    setTimeout(() => log.push("later"), 5);

    // Unlike an advance, a step has no handler of its own, so the rest of the
    // instant is left pending.
    expect(() => advanceTimersToNextTimer()).toThrow("boom");
    expect(log).toEqual(["throwing"]);
    expect(getTimerCount()).toBe(1);
  });
});

describe("running", () => {
  afterEach(() => {
    useRealTimers();
  });

  test("runAllTimers drains a chain of timers", () => {
    useFakeTimers();
    setSystemTime(0);
    const log = [];
    setTimeout(() => {
      log.push("a");
      setTimeout(() => log.push("b"), 100);
    }, 10);
    runAllTimers();

    expect(log).toEqual(["a", "b"]);
    expect(getTimerCount()).toBe(0);
  });

  test("runAllTimers gives up on a self-rescheduling timer", () => {
    useFakeTimers();
    let runs = 0;
    const reschedule = () => {
      runs += 1;
      setTimeout(reschedule, 1);
    };
    setTimeout(reschedule, 1);

    expect(() => runAllTimers()).toThrow(
      "Aborting after running 10000 timers, assuming an infinite loop!",
    );
    expect(runs).toBe(10000);
  });

  test("runAllTimers stops at a throwing callback", () => {
    useFakeTimers();
    setSystemTime(0);
    const log = [];
    setTimeout(() => {
      log.push("throwing");
      throw new Error("boom");
    }, 5);
    setTimeout(() => log.push("later"), 10);

    expect(() => runAllTimers()).toThrow("boom");
    expect(log).toEqual(["throwing"]);
    expect(getTimerCount()).toBe(1);
    expect(Date.now()).toBe(5);
  });

  test("runOnlyPendingTimers stops at the timers that were pending", () => {
    useFakeTimers();
    setSystemTime(0);
    const log = [];
    setTimeout(() => {
      log.push("a");
      setTimeout(() => log.push("far"), 100);
    }, 10);
    setTimeout(() => log.push("b"), 20);
    runOnlyPendingTimers();

    // The clock advanced to the latest due time that existed when the call was
    // made, so the nested timer beyond it is still waiting.
    expect(log).toEqual(["a", "b"]);
    expect(Date.now()).toBe(20);
    expect(getTimerCount()).toBe(1);
  });

  test("runOnlyPendingTimers still fires what a pending timer scheduled inside the window", () => {
    useFakeTimers();
    setSystemTime(0);
    const log = [];
    setTimeout(() => {
      log.push("a");
      setTimeout(() => log.push("nested"), 1);
    }, 10);
    setTimeout(() => log.push("b"), 20);
    runOnlyPendingTimers();

    expect(log).toEqual(["a", "nested", "b"]);
    expect(getTimerCount()).toBe(0);
  });

  test("clearAllTimers drops everything pending and rewinds the clock", () => {
    useFakeTimers();
    setSystemTime(1000);
    setTimeout(() => {}, 5);
    setInterval(() => {}, 5);
    advanceTimersByTime(2);
    expect(getTimerCount()).toBe(2);

    // The fake clock's reset does both halves, so both are reproduced: the
    // clock goes back to the instant fake timers were installed.
    clearAllTimers();
    expect(getTimerCount()).toBe(0);
    expect(Date.now() > 1600000000000).toBe(true);
  });

  test("clearAllTimers does nothing without fake timers", () => {
    expect(() => clearAllTimers()).not.toThrow();
  });
});

describe("the mocked system clock", () => {
  afterEach(() => {
    useRealTimers();
  });

  test("enabling fake timers freezes Date at the real time it started from", () => {
    expect(isFakeTimers()).toBe(false);
    useFakeTimers();
    expect(isFakeTimers()).toBe(true);

    const frozen = Date.now();
    Array.from({ length: 20000 }, (_, index) => index * 2).reduce(
      (total, value) => total + value,
      0,
    );
    expect(Date.now()).toBe(frozen);
    expect(frozen).toBe(getMockedSystemTime());
  });

  test("setSystemTime moves Date and new Date together", () => {
    useFakeTimers();
    setSystemTime(1000);

    expect(Date.now()).toBe(1000);
    expect(new Date().getTime()).toBe(1000);
    expect(getMockedSystemTime()).toBe(1000);
  });

  test("advancing the clock advances Date", () => {
    useFakeTimers();
    setSystemTime(1577836800000);
    advanceTimersByTime(500);

    expect(Date.now()).toBe(1577836800500);
  });

  test("setSystemTime keeps a pending timer's remaining delay", () => {
    useFakeTimers();
    setSystemTime(1000);
    const log = [];
    setTimeout(() => log.push(Date.now()), 10);

    // Moving the wall clock is not the same as letting time pass, so the timer
    // is still 10ms away — now from the new instant.
    setSystemTime(5000);
    advanceTimersByTime(10);
    expect(log).toEqual([5010]);
  });

  test("performance.now measures elapsed virtual time, not the simulated date", () => {
    useFakeTimers();
    setSystemTime(1000);
    const before = performance.now();
    setSystemTime(9999999);
    expect(performance.now()).toBe(before);

    advanceTimersByTime(250);
    expect(performance.now() - before).toBe(250);
  });

  test("getRealSystemTime reads the real clock even while one is mocked", () => {
    useFakeTimers();
    setSystemTime(0);

    expect(Date.now()).toBe(0);
    expect(getRealSystemTime() > 1600000000000).toBe(true);
  });

  test("setSystemTime without fake timers freezes Date and nothing else", () => {
    setSystemTime(4200);

    expect(isFakeTimers()).toBe(false);
    expect(Date.now()).toBe(4200);
    expect(getMockedSystemTime()).toBe(4200);

    useRealTimers();
    expect(getMockedSystemTime()).toBe(null);
    expect(Date.now() > 1600000000000).toBe(true);
  });

  test("re-enabling fake timers installs a fresh clock at the current instant", () => {
    useFakeTimers();
    setSystemTime(0);
    setTimeout(() => {}, 5);
    expect(getTimerCount()).toBe(1);

    useFakeTimers();
    expect(getTimerCount()).toBe(0);
    expect(Date.now()).toBe(0);
  });

  test("useRealTimers hands the clock and the queue back", () => {
    useFakeTimers();
    setSystemTime(0);
    setTimeout(() => {}, 5);
    useRealTimers();

    expect(isFakeTimers()).toBe(false);
    expect(getMockedSystemTime()).toBe(null);
    expect(Date.now() > 1600000000000).toBe(true);
  });

  test("the advance members refuse to run without fake timers", () => {
    const message =
      "A function to advance timers was called but the timers APIs are not mocked";

    expect(() => advanceTimersByTime(1)).toThrow(message);
    expect(() => advanceTimersToNextTimer()).toThrow(message);
    expect(() => runAllTimers()).toThrow(message);
    expect(() => runOnlyPendingTimers()).toThrow(message);
    expect(() => getTimerCount()).toThrow(message);
  });
});
