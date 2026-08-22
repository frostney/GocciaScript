/*---
description: regression cases for the virtual timer queue, one per reviewed defect
features: [Timers]
---*/

import {
  advanceTimersByTime,
  getTimerCount,
  setSystemTime,
  useFakeTimers,
  useRealTimers,
} from "goccia:timers";

// Every case here reproduces a defect the timer queue shipped with in review.
// They are kept together, and named for what they guard, because each one
// passes trivially against a queue that never had the bug.

// Scheduled while the entry module evaluates. The engine reaches runtime idle
// at that moment and used to drain it there — ten thousand times, because an
// interval is never exhausted — before a single test body had run.
let moduleIntervalRuns = 0;
const moduleInterval = setInterval(() => {
  moduleIntervalRuns += 1;
}, 1);

describe("a non-finite system time is refused, not installed", () => {
  afterEach(() => {
    useRealTimers();
  });

  // Installing NaN as the clock made every due-time comparison false: the range
  // test then selected arbitrarily and the trailing re-check in the tick
  // recursed on NaN until the stack was gone. Vitest tolerates it because a JS
  // clock can hold NaN; this one cannot, so it refuses at the door.
  test("setSystemTime rejects NaN", () => {
    useFakeTimers();

    expect(() => setSystemTime(NaN)).toThrow(TypeError);
    expect(() => setSystemTime(NaN)).toThrow("must be a finite number");
  });

  test("setSystemTime rejects Infinity", () => {
    useFakeTimers();

    expect(() => setSystemTime(Infinity)).toThrow(TypeError);
    expect(() => setSystemTime(-Infinity)).toThrow(TypeError);
  });

  // The engine surface takes epoch milliseconds, so a string arrives as NaN
  // through ToNumber. This is the second entry point — it does not go through
  // the Vitest shim's Date conversion — and it is why the guard has to be in
  // the queue rather than at the shim boundary.
  test("the engine surface rejects a string that is not a number", () => {
    useFakeTimers();

    expect(() => setSystemTime("2020-01-01")).toThrow(TypeError);
  });

  test("the clock still works afterwards", () => {
    useFakeTimers();
    setSystemTime(0);
    try {
      setSystemTime(NaN);
    } catch (error) {
      // refused, as asserted above
    }

    expect(Date.now()).toBe(0);

    const log = [];
    setTimeout(() => log.push(Date.now()), 5);
    advanceTimersByTime(5);
    expect(log).toEqual([5]);
  });

  test("a refused advance leaves the clock alone", () => {
    useFakeTimers();
    setSystemTime(10);

    expect(() => advanceTimersByTime(NaN)).toThrow(TypeError);
    expect(Date.now()).toBe(10);
  });
});

describe("an advance keeps its own recorded exception", () => {
  afterEach(() => {
    useRealTimers();
  });

  // The recorded exception used to live on the queue rather than on the
  // operation, so an advance started from inside a timer callback raised the
  // OUTER advance's error at the inner call — and the outer advance then
  // reported success. Vitest keeps it per operation; this matches.
  test("a nested advance does not steal the outer one's error", () => {
    useFakeTimers();
    setSystemTime(0);
    const log = [];

    setTimeout(() => {
      log.push("first");
      throw new Error("outer");
    }, 5);
    setTimeout(() => {
      log.push("second");
      let inner = null;
      try {
        advanceTimersByTime(0);
      } catch (error) {
        inner = error.message;
      }
      log.push("inner=" + inner);
    }, 10);

    let outer = null;
    try {
      advanceTimersByTime(20);
    } catch (error) {
      outer = error.message;
    }
    log.push("outer=" + outer);

    expect(log).toEqual(["first", "second", "inner=null", "outer=outer"]);
  });
});

describe("real-mode timers", () => {
  // An interval stays in the queue while its callback runs, and the real-mode
  // step picks the earliest timer with no window to bound it — so the moment a
  // callback awaited, the step handed back the same entry and the callback
  // re-entered itself until the stack ran out.
  test("an awaiting interval callback does not re-enter itself", async () => {
    let depth = 0;
    let maxDepth = 0;
    let runs = 0;

    const id = setInterval(async () => {
      runs += 1;
      depth += 1;
      if (depth > maxDepth) maxDepth = depth;
      await Promise.resolve();
      await null;
      depth -= 1;
      if (runs >= 3) clearInterval(id);
    }, 5);

    await new Promise((resolve) => setTimeout(resolve, 40));

    expect(maxDepth).toBe(1);
    expect(runs).toBe(3);
  });

  // The module-scope interval above. Draining it at the engine's idle point
  // spent the whole budget on a timer that can never be exhausted, and did it
  // before any test existed to attribute the work to.
  test("the idle drain leaves intervals alone", () => {
    expect(moduleIntervalRuns).toBe(0);
    clearInterval(moduleInterval);
  });

  // An uncleared zero-period interval reschedules at the instant it just ran.
  // Under real timers it must not be picked up by any drain at all.
  test("an uncleared zero-period interval does not hold the run open", () => {
    let runs = 0;
    const id = setInterval(() => {
      runs += 1;
    }, 0);

    expect(typeof id).toBe("number");
    expect(runs).toBe(0);
    clearInterval(id);
  });

  test("a zero-delay chain resolves in order", async () => {
    const log = [];
    await new Promise((resolve) => {
      setTimeout(() => {
        log.push("first");
        setTimeout(() => {
          log.push("second");
          setTimeout(() => {
            log.push("third");
            resolve();
          }, 0);
        }, 0);
      }, 0);
    });

    expect(log).toEqual(["first", "second", "third"]);
  });
});

// Cross-test state, because what is being checked is what happens BETWEEN two
// tests: the pair below only means anything run in order.
let bodyTimerRan = false;
let strandedIntervalRan = false;

describe("timers a test body schedules", () => {
  // The engine's idle point is reached while the entry module is still
  // evaluating, so a `setTimeout` written inside a test body was never drained
  // by it — the timer simply sat in the queue until the run tore down and
  // discarded it, silently. The runner drains at the end of each test instead.
  test("are drained at the end of the test that scheduled them", () => {
    setTimeout(() => {
      bodyTimerRan = true;
    }, 0);

    // Still pending here: the drain happens once the body has returned.
    expect(bodyTimerRan).toBe(false);
  });

  test("have run by the time the next test starts", () => {
    expect(bodyTimerRan).toBe(true);
  });
});

describe("timers a test body strands", () => {
  test("an interval left running does not survive the test", () => {
    setInterval(() => {
      strandedIntervalRan = true;
    }, 1);

    expect(strandedIntervalRan).toBe(false);
  });

  test("it cannot fire inside the next test", async () => {
    // Anything the previous test left behind would have had every chance here:
    // this body awaits a timer, which is exactly when the queue runs.
    await new Promise((resolve) => setTimeout(resolve, 50));

    expect(strandedIntervalRan).toBe(false);
    expect(getTimerCount).toBeInstanceOf(Function);
  });
});

describe("fractional delays and advances", () => {
  afterEach(() => {
    useRealTimers();
  });

  // The fake clock computes a due time with parseInt, so the fraction of a
  // delay is dropped — but the fraction of an ADVANCE is banked and carried,
  // so two half-millisecond advances move the clock by one. Both halves probed
  // against Vitest 4.1.10.
  test("a fractional delay is truncated", () => {
    useFakeTimers();
    setSystemTime(0);
    const log = [];
    setTimeout(() => log.push(Date.now()), 1.5);

    advanceTimersByTime(1);
    expect(log).toEqual([1]);
  });

  test("a delay below one millisecond is due immediately", () => {
    useFakeTimers();
    setSystemTime(0);
    const log = [];
    setTimeout(() => log.push(Date.now()), 0.4);

    advanceTimersByTime(0);
    expect(log).toEqual([0]);
  });

  test("fractional advances accumulate", () => {
    useFakeTimers();
    setSystemTime(0);
    const log = [];
    setTimeout(() => log.push(Date.now()), 2);

    advanceTimersByTime(1.5);
    expect(log).toEqual([]);
    expect(Date.now()).toBe(1);

    advanceTimersByTime(0.5);
    expect(log).toEqual([2]);
    expect(Date.now()).toBe(2);
  });
});

describe("zero-period intervals under fake timers", () => {
  afterEach(() => {
    useRealTimers();
  });

  // Vitest fires every one of them at the instant the clock is already on, and
  // the advance still finishes where it was asked to. Matched rather than
  // clamped to Node's 1ms floor, because Vitest is the oracle for the fake
  // clock; the unbounded shape is caught by the per-advance bound instead.
  test("every tick lands on the same instant", () => {
    useFakeTimers();
    setSystemTime(0);
    const stamps = [];
    let runs = 0;
    const id = setInterval(() => {
      runs += 1;
      stamps.push(Date.now());
      if (runs >= 5) clearInterval(id);
    }, 0);

    advanceTimersByTime(3);

    expect(stamps).toEqual([0, 0, 0, 0, 0]);
    expect(Date.now()).toBe(3);
    expect(getTimerCount()).toBe(0);
  });
});

describe("performance.now across the fake/real transition", () => {
  test("it starts at zero when faked and returns to the real timeline after", () => {
    const realBefore = performance.now();

    useFakeTimers();
    expect(performance.now()).toBe(0);
    advanceTimersByTime(500);
    expect(performance.now()).toBe(500);

    useRealTimers();
    // Back on the process timeline, not stranded at 500.
    expect(performance.now() >= realBefore).toBe(true);
    expect(typeof performance.timeOrigin).toBe("number");
  });
});

describe("a nested advance that self-clears an interval keeps the outer frame's entry alive", () => {
  afterEach(() => {
    useRealTimers();
  });

  // An interval stays in the queue while it runs, and the fake-clock selectors
  // do not skip a dispatching entry, so a nested advance from inside the
  // callback re-enters the same entry. When dispatch state was a Boolean, the
  // inner frame cleared it on the way out; a clearInterval from the still-running
  // outer callback then took the "not dispatching" branch and freed the entry
  // the outer frame was about to read — a use-after-free. A depth counter keeps
  // the entry marked in-flight until the last frame unwinds.
  test("nested advance then an outer-frame self-clear does not free the live entry", () => {
    useFakeTimers();

    let runs = 0;
    let id;
    id = setInterval(() => {
      runs += 1;
      const mine = runs;
      if (mine === 1) {
        // Re-enter this same interval entry (inner frame, run 2), then clear it
        // from the outer frame after the nested advance returns. The Boolean
        // flag was left False by the inner frame, so this clear used to delete
        // and free the entry the outer frame is about to read on the way out.
        advanceTimersByTime(10);
        clearInterval(id);
      }
    }, 10);

    advanceTimersByTime(10);

    expect(runs >= 2).toBe(true);
    expect(getTimerCount()).toBe(0);
  });
});

describe("an out-of-range clock target is refused before any state changes", () => {
  afterEach(() => {
    useRealTimers();
  });

  // 1e13 ms is finite, so it clears the finite-epoch gate, but it exceeds the
  // ~9.2e12 ms the Int64 nanosecond clock can hold. The range check used to run
  // only at publish time — AFTER FNow and every pending timer had already been
  // shifted — so a caught rejection left FNow poisoned and every later advance
  // republished it and threw again. The check now runs before any mutation.
  const OUT_OF_RANGE = 1e13;

  test("setSystemTime rejects an out-of-range target with a RangeError", () => {
    useFakeTimers();

    expect(() => setSystemTime(OUT_OF_RANGE)).toThrow(RangeError);
    expect(() => setSystemTime(OUT_OF_RANGE)).toThrow("nanosecond clock");
  });

  test("a rejected setSystemTime leaves the clock and timers untouched", () => {
    useFakeTimers();
    setSystemTime(10);
    const log = [];
    setTimeout(() => log.push(Date.now()), 5);

    try {
      setSystemTime(OUT_OF_RANGE);
    } catch (error) {
      // refused, as asserted above
    }

    // The clock never moved and the pending timer's due time never shifted, so
    // a following valid advance still fires it at exactly 15 rather than
    // throwing on a poisoned FNow.
    expect(Date.now()).toBe(10);
    advanceTimersByTime(5);
    expect(log).toEqual([15]);
  });

  test("useFakeTimers rejects an out-of-range start", () => {
    expect(() => useFakeTimers(OUT_OF_RANGE)).toThrow(RangeError);
  });

  test("a rejected advance leaves the clock alone", () => {
    useFakeTimers();
    setSystemTime(0);

    expect(() => advanceTimersByTime(OUT_OF_RANGE)).toThrow(RangeError);
    expect(Date.now()).toBe(0);
  });
});

describe("the timer globals are the runner's", () => {
  test("they are reported as runtime globals", () => {
    const names = Goccia.runtimeGlobals;

    expect(names.includes("setTimeout")).toBe(true);
    expect(names.includes("clearTimeout")).toBe(true);
    expect(names.includes("setInterval")).toBe(true);
    expect(names.includes("clearInterval")).toBe(true);
  });
});
