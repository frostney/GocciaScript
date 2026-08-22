// Differential suite: the `vi` fake-timer family.
//
// Vitest gates. Fake timers are testing-API semantics — the product target is
// Vitest-exact behaviour, and Vitest's own timers wrap @sinonjs/fake-timers,
// so only vitest can decide what a tick does. Bun is skipped for the reason
// e-mocks records: the suite imports `vi` from a bare `vitest` specifier, and
// importing the real `vitest` package from a `bun test` file drops bun's
// injected globals and dies on `describe is not defined`.
//
// Deliberately not asserted here: the type of a timer id. Vitest runs in Node,
// where the fake clock hands back a Node-shaped `Timeout` object; GocciaScript
// hands back a number, as the web platform does. Both are cleared by passing
// them to clearTimeout, which is the part a suite depends on.
//
// ---------------------------------------------------------------------------
// Reading a failure after a Vitest bump
// ---------------------------------------------------------------------------
// Most of what is below is ordinary behaviour that any fake-timer
// implementation would have to keep. A dozen assertions are not: they pin a
// number, a string, or a workaround that belongs to @sinonjs/fake-timers and
// to the way Vitest drives it, so a bump can move them without anything being
// wrong with this engine. Each one carries a `PINNED:` note naming what it
// depends on. If one of those fails after a bump, re-probe the pinned Vitest
// and move the expectation; if anything WITHOUT such a note fails, the engine
// regressed.

import { vi } from "vitest";

describe("advancing", () => {
  afterEach(() => {
    vi.useRealTimers();
  });

  test("timers fire in due-time order, then in registration order", () => {
    vi.useFakeTimers();
    vi.setSystemTime(0);
    const log = [];
    setTimeout(() => log.push("late"), 20);
    setTimeout(() => log.push("early-a"), 10);
    setTimeout(() => log.push("early-b"), 10);
    vi.advanceTimersByTime(20);

    expect(log).toEqual(["early-a", "early-b", "late"]);
  });

  test("a synchronous advance runs no microtasks between timers", () => {
    vi.useFakeTimers();
    vi.setSystemTime(0);
    const log = [];
    setTimeout(() => {
      log.push("first");
      Promise.resolve().then(() => log.push("microtask"));
    }, 10);
    setTimeout(() => log.push("second"), 20);
    Promise.resolve().then(() => log.push("outer-microtask"));
    vi.advanceTimersByTime(25);

    expect(log).toEqual(["first", "second"]);
  });

  test("an asynchronous advance drains microtasks around every timer", async () => {
    vi.useFakeTimers();
    vi.setSystemTime(0);
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
    await vi.advanceTimersByTimeAsync(25);

    expect(log).toEqual([
      "pending",
      "first",
      "after-first",
      "second",
      "after-second",
    ]);
  });

  test("a zero, missing or negative delay is due immediately", () => {
    vi.useFakeTimers();
    vi.setSystemTime(0);
    const log = [];
    setTimeout(() => log.push("zero"), 0);
    setTimeout(() => log.push("missing"));
    setTimeout(() => log.push("negative"), -5);
    setTimeout(() => log.push("one"), 1);
    vi.advanceTimersByTime(0);

    expect(log).toEqual(["zero", "missing", "negative"]);

    vi.advanceTimersByTime(1);
    expect(log).toEqual(["zero", "missing", "negative", "one"]);
  });

  // PINNED: the `delay || (duringTick ? 1 : 0)` rule in the fake clock's
  // addTimer. Nothing requires a nested zero-delay timer to land one
  // millisecond later rather than on the current instant — it is how sinon
  // stops a zero-delay chain from looping inside one tick.
  test("a zero-delay timer scheduled inside a callback lands on the next millisecond", () => {
    vi.useFakeTimers();
    vi.setSystemTime(0);
    const log = [];
    setTimeout(() => {
      log.push("outer@" + Date.now());
      setTimeout(() => log.push("inner@" + Date.now()), 0);
    }, 5);

    vi.advanceTimersByTime(5);
    expect(log).toEqual(["outer@5"]);

    vi.advanceTimersByTime(1);
    expect(log).toEqual(["outer@5", "inner@6"]);
  });

  test("a timer scheduled inside a callback fires later in the same advance", () => {
    vi.useFakeTimers();
    vi.setSystemTime(0);
    const log = [];
    setTimeout(() => {
      log.push("outer@" + Date.now());
      setTimeout(() => log.push("inner@" + Date.now()), 5);
    }, 10);
    vi.advanceTimersByTime(20);

    expect(log).toEqual(["outer@10", "inner@15"]);
  });

  test("extra arguments reach the callback", () => {
    vi.useFakeTimers();
    const seen = [];
    setTimeout((first, second) => seen.push([first, second]), 1, "x", 2);
    vi.advanceTimersByTime(1);

    expect(seen).toEqual([["x", 2]]);
  });

  // PINNED: Vitest's own workaround for sinonjs/fake-timers#250 — it follows
  // clock.next() with a zero-length tick so the whole instant fires. If
  // upstream fixes the issue and Vitest drops the workaround, only the second
  // and third timers move.
  test("advanceTimersToNextTimer fires every timer due at that instant", () => {
    vi.useFakeTimers();
    vi.setSystemTime(0);
    const log = [];
    setTimeout(() => log.push("a@" + Date.now()), 10);
    setTimeout(() => log.push("b@" + Date.now()), 20);
    setTimeout(() => log.push("c@" + Date.now()), 20);

    vi.advanceTimersToNextTimer();
    expect(log).toEqual(["a@10"]);
    expect(Date.now()).toBe(10);

    vi.advanceTimersToNextTimer();
    expect(log).toEqual(["a@10", "b@20", "c@20"]);
    expect(Date.now()).toBe(20);
  });

  // PINNED: the exact string thrown by the fake clock's doTick.
  test("a negative advance is refused", () => {
    vi.useFakeTimers();

    expect(() => vi.advanceTimersByTime(-1)).toThrow(
      "Negative ticks are not supported",
    );
  });

  test("a throwing callback does not stop the timers behind it", () => {
    vi.useFakeTimers();
    vi.setSystemTime(0);
    const log = [];
    setTimeout(() => {
      log.push("throwing");
      throw new Error("boom");
    }, 5);
    setTimeout(() => log.push("later"), 10);

    expect(() => vi.advanceTimersByTime(20)).toThrow("boom");
    expect(log).toEqual(["throwing", "later"]);
    expect(Date.now()).toBe(20);
  });

  // PINNED: which advance members catch and which do not. The fake clock's
  // tick records the first exception and carries on; its `next` has no handler,
  // so runAllTimers and advanceTimersToNextTimer stop. That asymmetry is an
  // implementation detail of sinon, not a rule about timers.
  test("stepping to a single timer stops at a throwing callback", () => {
    vi.useFakeTimers();
    vi.setSystemTime(0);
    const log = [];
    setTimeout(() => {
      log.push("throwing");
      throw new Error("boom");
    }, 5);
    setTimeout(() => log.push("later"), 5);

    expect(() => vi.advanceTimersToNextTimer()).toThrow("boom");
    expect(log).toEqual(["throwing"]);
    expect(vi.getTimerCount()).toBe(1);
  });
});

describe("cancelling", () => {
  afterEach(() => {
    vi.useRealTimers();
  });

  test("clearTimeout cancels from outside and from inside a callback", () => {
    vi.useFakeTimers();
    const log = [];
    const cancelled = setTimeout(() => log.push("cancelled"), 10);
    setTimeout(() => {
      log.push("canceller");
      clearTimeout(cancelled);
    }, 5);
    vi.advanceTimersByTime(20);

    expect(log).toEqual(["canceller"]);
  });

  test("clearing an absent or falsy id is a no-op", () => {
    vi.useFakeTimers();

    expect(() => clearTimeout(undefined)).not.toThrow();
    expect(() => clearTimeout(0)).not.toThrow();
    expect(() => clearInterval(undefined)).not.toThrow();
  });

  // PINNED: the rewind half. clearAllTimers maps onto the clock's `reset`,
  // which also restores `now` to the install instant — a suite would not
  // predict that from the member's name.
  test("clearAllTimers drops everything pending and rewinds the clock", () => {
    vi.useFakeTimers();
    vi.setSystemTime(1000);
    setTimeout(() => {}, 5);
    setInterval(() => {}, 5);
    vi.advanceTimersByTime(2);
    expect(vi.getTimerCount()).toBe(2);

    vi.clearAllTimers();
    expect(vi.getTimerCount()).toBe(0);
    expect(Date.now() > 1600000000000).toBe(true);
  });

  test("getTimerCount reports what is still pending", () => {
    vi.useFakeTimers();
    const id = setTimeout(() => {}, 5);
    expect(vi.getTimerCount()).toBe(1);

    clearTimeout(id);
    expect(vi.getTimerCount()).toBe(0);
  });
});

describe("intervals", () => {
  afterEach(() => {
    vi.useRealTimers();
  });

  test("an interval reschedules from its previous due time", () => {
    vi.useFakeTimers();
    vi.setSystemTime(0);
    const log = [];
    const id = setInterval(() => log.push(Date.now()), 10);
    vi.advanceTimersByTime(35);
    clearInterval(id);

    expect(log).toEqual([10, 20, 30]);
  });

  test("a single advance fires every tick the interval crossed", () => {
    vi.useFakeTimers();
    vi.setSystemTime(0);
    const log = [];
    const id = setInterval(() => log.push(Date.now()), 10);
    vi.advanceTimersByTime(50);
    clearInterval(id);

    expect(log).toEqual([10, 20, 30, 40, 50]);
  });

  test("an interval that clears itself stops and leaves nothing pending", () => {
    vi.useFakeTimers();
    vi.setSystemTime(0);
    let runs = 0;
    const id = setInterval(() => {
      runs += 1;
      if (runs === 3) clearInterval(id);
    }, 10);
    vi.advanceTimersByTime(100);

    expect(runs).toBe(3);
    expect(vi.getTimerCount()).toBe(0);
  });
});

describe("running", () => {
  afterEach(() => {
    vi.useRealTimers();
  });

  test("runAllTimers drains a chain of timers", () => {
    vi.useFakeTimers();
    vi.setSystemTime(0);
    const log = [];
    setTimeout(() => {
      log.push("a");
      setTimeout(() => log.push("b"), 100);
    }, 10);
    vi.runAllTimers();

    expect(log).toEqual(["a", "b"]);
    expect(vi.getTimerCount()).toBe(0);
  });

  // PINNED: both the number and the sentence. 10000 is Vitest's configured
  // loopLimit, not a property of timers, and the message is @sinonjs's.
  test("runAllTimers gives up on a self-rescheduling timer", () => {
    vi.useFakeTimers();
    let runs = 0;
    const reschedule = () => {
      runs += 1;
      setTimeout(reschedule, 1);
    };
    setTimeout(reschedule, 1);

    expect(() => vi.runAllTimers()).toThrow(
      "Aborting after running 10000 timers, assuming an infinite loop!",
    );
    expect(runs).toBe(10000);
  });

  // PINNED: same asymmetry as above, from the other side.
  test("runAllTimers stops at a throwing callback", () => {
    vi.useFakeTimers();
    vi.setSystemTime(0);
    const log = [];
    setTimeout(() => {
      log.push("throwing");
      throw new Error("boom");
    }, 5);
    setTimeout(() => log.push("later"), 10);

    expect(() => vi.runAllTimers()).toThrow("boom");
    expect(log).toEqual(["throwing"]);
    expect(vi.getTimerCount()).toBe(1);
    expect(Date.now()).toBe(5);
  });

  // PINNED: same asymmetry again — runToLast goes through tick, so it catches.
  test("runOnlyPendingTimers keeps going past a throwing callback", () => {
    vi.useFakeTimers();
    vi.setSystemTime(0);
    const log = [];
    setTimeout(() => {
      log.push("throwing");
      throw new Error("boom");
    }, 5);
    setTimeout(() => log.push("later"), 10);

    expect(() => vi.runOnlyPendingTimers()).toThrow("boom");
    expect(log).toEqual(["throwing", "later"]);
    expect(Date.now()).toBe(10);
  });

  // PINNED: runToLast advances to the LATEST due time among the timers pending
  // at the call, so what counts as "only pending" includes anything that
  // becomes due inside that window.
  test("runOnlyPendingTimers stops at the timers that were pending", () => {
    vi.useFakeTimers();
    vi.setSystemTime(0);
    const log = [];
    setTimeout(() => {
      log.push("a");
      setTimeout(() => log.push("far"), 100);
    }, 10);
    setTimeout(() => log.push("b"), 20);
    vi.runOnlyPendingTimers();

    expect(log).toEqual(["a", "b"]);
    expect(Date.now()).toBe(20);
    expect(vi.getTimerCount()).toBe(1);
  });

  test("runOnlyPendingTimers still fires what a pending timer scheduled inside the window", () => {
    vi.useFakeTimers();
    vi.setSystemTime(0);
    const log = [];
    setTimeout(() => {
      log.push("a");
      setTimeout(() => log.push("nested"), 1);
    }, 10);
    setTimeout(() => log.push("b"), 20);
    vi.runOnlyPendingTimers();

    expect(log).toEqual(["a", "nested", "b"]);
    expect(vi.getTimerCount()).toBe(0);
  });
});

describe("the mocked system clock", () => {
  afterEach(() => {
    vi.useRealTimers();
  });

  test("setSystemTime moves Date and new Date together", () => {
    vi.useFakeTimers();
    vi.setSystemTime(1000);

    expect(Date.now()).toBe(1000);
    expect(new Date().getTime()).toBe(1000);
    expect(vi.getMockedSystemTime() instanceof Date).toBe(true);
    expect(vi.getMockedSystemTime().getTime()).toBe(1000);
  });

  test("setSystemTime takes a Date as well as a number", () => {
    vi.useFakeTimers();
    vi.setSystemTime(new Date("2020-01-01T00:00:00.000Z"));

    expect(Date.now()).toBe(1577836800000);

    vi.advanceTimersByTime(500);
    expect(Date.now()).toBe(1577836800500);
  });

  test("setSystemTime keeps a pending timer's remaining delay", () => {
    vi.useFakeTimers();
    vi.setSystemTime(1000);
    const log = [];
    setTimeout(() => log.push(Date.now()), 10);
    vi.setSystemTime(5000);
    vi.advanceTimersByTime(10);

    expect(log).toEqual([5010]);
  });

  test("moving the clock backwards keeps the timer pending", () => {
    vi.useFakeTimers();
    vi.setSystemTime(1000);
    const log = [];
    setTimeout(() => log.push(Date.now()), 10);
    vi.setSystemTime(0);

    expect(vi.getTimerCount()).toBe(1);

    vi.advanceTimersByTime(10);
    expect(log).toEqual([10]);
  });

  test("performance.now measures elapsed virtual time, not the simulated date", () => {
    vi.useFakeTimers();
    vi.setSystemTime(1000);
    const before = performance.now();
    vi.setSystemTime(9999999);

    expect(performance.now()).toBe(before);

    vi.advanceTimersByTime(250);
    expect(performance.now() - before).toBe(250);
  });

  test("getRealSystemTime reads the real clock even while one is mocked", () => {
    vi.useFakeTimers();
    vi.setSystemTime(0);

    expect(Date.now()).toBe(0);
    expect(vi.getRealSystemTime() > 1600000000000).toBe(true);
  });

  test("isFakeTimers and getMockedSystemTime follow the mode", () => {
    expect(vi.isFakeTimers()).toBe(false);
    expect(vi.getMockedSystemTime()).toBe(null);

    vi.useFakeTimers();
    expect(vi.isFakeTimers()).toBe(true);
    expect(vi.getMockedSystemTime() instanceof Date).toBe(true);

    vi.useRealTimers();
    expect(vi.isFakeTimers()).toBe(false);
    expect(vi.getMockedSystemTime()).toBe(null);
  });

  // PINNED: that a second useFakeTimers() discards pending timers rather than
  // carrying them over.
  test("re-enabling fake timers installs a fresh clock at the current instant", () => {
    vi.useFakeTimers();
    vi.setSystemTime(0);
    setTimeout(() => {}, 5);
    expect(vi.getTimerCount()).toBe(1);

    vi.useFakeTimers();
    expect(vi.getTimerCount()).toBe(0);
    expect(Date.now()).toBe(0);
  });
});

describe("fractional delays and advances", () => {
  afterEach(() => {
    vi.useRealTimers();
  });

  // PINNED: the fake clock computes a due time with parseInt, which drops the
  // fraction of a DELAY, while the fraction of an ADVANCE is banked in a
  // nanosecond remainder and carried. The pairing is unintuitive enough that
  // the opposite is the natural guess, so both halves are pinned together.
  test("a fractional delay is truncated", () => {
    vi.useFakeTimers();
    vi.setSystemTime(0);
    const log = [];
    setTimeout(() => log.push(Date.now()), 1.5);

    vi.advanceTimersByTime(1);
    expect(log).toEqual([1]);
  });

  test("a delay below one millisecond is due immediately", () => {
    vi.useFakeTimers();
    vi.setSystemTime(0);
    const log = [];
    setTimeout(() => log.push(Date.now()), 0.4);

    vi.advanceTimersByTime(0);
    expect(log).toEqual([0]);
  });

  test("fractional advances accumulate", () => {
    vi.useFakeTimers();
    vi.setSystemTime(0);
    const log = [];
    setTimeout(() => log.push(Date.now()), 2);

    vi.advanceTimersByTime(1.5);
    expect(log).toEqual([]);
    expect(Date.now()).toBe(1);

    vi.advanceTimersByTime(0.5);
    expect(log).toEqual([2]);
    expect(Date.now()).toBe(2);
  });
});

describe("zero-period intervals", () => {
  afterEach(() => {
    vi.useRealTimers();
  });

  // PINNED: an interval reschedules by adding its period to the due time, so a
  // period of zero re-arms at the instant it just ran. Every tick therefore
  // lands on the same instant, and the advance still finishes where it was
  // asked to. Node would clamp the period to 1ms; the fake clock does not, and
  // the fake clock is the oracle here.
  test("every tick lands on the same instant", () => {
    vi.useFakeTimers();
    vi.setSystemTime(0);
    const stamps = [];
    let runs = 0;
    const id = setInterval(() => {
      runs += 1;
      stamps.push(Date.now());
      if (runs >= 5) clearInterval(id);
    }, 0);

    vi.advanceTimersByTime(3);

    expect(stamps).toEqual([0, 0, 0, 0, 0]);
    expect(Date.now()).toBe(3);
    expect(vi.getTimerCount()).toBe(0);
  });
});

describe("an advance keeps its own recorded exception", () => {
  afterEach(() => {
    vi.useRealTimers();
  });

  // The recorded exception is per advance operation, not per clock: an advance
  // called from inside a timer callback sees nothing of the enclosing one's
  // error. Not pinned — a shared slot would be a bug in any implementation,
  // and it was one here.
  test("a nested advance does not steal the outer one's error", () => {
    vi.useFakeTimers();
    vi.setSystemTime(0);
    const log = [];

    setTimeout(() => {
      log.push("first");
      throw new Error("outer");
    }, 5);
    setTimeout(() => {
      log.push("second");
      let inner = null;
      try {
        vi.advanceTimersByTime(0);
      } catch (error) {
        inner = error.message;
      }
      log.push("inner=" + inner);
    }, 10);

    let outer = null;
    try {
      vi.advanceTimersByTime(20);
    } catch (error) {
      outer = error.message;
    }
    log.push("outer=" + outer);

    expect(log).toEqual(["first", "second", "inner=null", "outer=outer"]);
  });
});

describe("performance.now across the fake/real transition", () => {
  afterEach(() => {
    vi.useRealTimers();
  });

  // PINNED: that a mocked monotonic clock starts at zero rather than continuing
  // the process timeline, and that leaving fake timers puts it back rather than
  // stranding it at whatever the advance reached.
  test("it starts at zero when faked and returns to the real timeline after", () => {
    const realBefore = performance.now();

    vi.useFakeTimers();
    expect(performance.now()).toBe(0);
    vi.advanceTimersByTime(500);
    expect(performance.now()).toBe(500);

    vi.useRealTimers();
    expect(performance.now() >= realBefore).toBe(true);
    expect(typeof performance.timeOrigin).toBe("number");
  });

  // A setSystemTime jump is a change of date, not elapsed time.
  test("setSystemTime does not move it", () => {
    vi.useFakeTimers();
    vi.setSystemTime(1000);
    const before = performance.now();

    vi.setSystemTime(9999999);
    expect(performance.now()).toBe(before);

    vi.advanceTimersByTime(250);
    expect(performance.now() - before).toBe(250);
  });
});

describe("setSystemTime accepts what Date accepts", () => {
  afterEach(() => {
    vi.useRealTimers();
  });

  // Anything that is not already a Date goes through the Date constructor, so a
  // date string is supported API. Not pinned: this is Vitest's documented
  // signature, not an implementation detail.
  test("an ISO string", () => {
    vi.useFakeTimers();
    vi.setSystemTime("2020-01-01T00:00:00.000Z");

    expect(Date.now()).toBe(1577836800000);
  });

  test("a Date and a number agree", () => {
    vi.useFakeTimers();
    vi.setSystemTime(new Date("2020-01-01T00:00:00.000Z"));
    const fromDate = Date.now();

    vi.setSystemTime(1577836800000);
    expect(Date.now()).toBe(fromDate);
  });
});

describe("the unmocked guard", () => {
  // PINNED: Vitest's own guard message, verbatim, from its FakeTimers wrapper.
  test("the advance members refuse to run without fake timers", () => {
    vi.useRealTimers();
    const message =
      "A function to advance timers was called but the timers APIs are not mocked";

    expect(() => vi.advanceTimersByTime(1)).toThrow(message);
    expect(() => vi.advanceTimersToNextTimer()).toThrow(message);
    expect(() => vi.runAllTimers()).toThrow(message);
    expect(() => vi.runOnlyPendingTimers()).toThrow(message);
    expect(() => vi.getTimerCount()).toThrow(message);
  });

  // PINNED: Vitest returns its utils object from these; nothing about a timer
  // queue requires it.
  test("every timer member chains by returning vi", () => {
    expect(vi.useFakeTimers()).toBe(vi);
    expect(vi.setSystemTime(0)).toBe(vi);
    expect(vi.advanceTimersByTime(0)).toBe(vi);
    expect(vi.advanceTimersToNextTimer()).toBe(vi);
    expect(vi.runAllTimers()).toBe(vi);
    expect(vi.runOnlyPendingTimers()).toBe(vi);
    expect(vi.useRealTimers()).toBe(vi);
  });
});
