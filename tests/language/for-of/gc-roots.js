/*---
description: for-of iteration heads stay reachable during explicit GC
features: [Goccia.gc, Symbol.iterator]
---*/

const hasGoccia = typeof Goccia !== "undefined";

const churn = () => {
  Goccia.gc();
  let total = 0;
  for (const i of [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]) {
    const scratch = { a: i * 7.5, b: [i, i + 1], c: "x" + i };
    total += scratch.a + scratch.b[0];
  }
  return total;
};

// A hand-written iterator whose values exist only for the iteration that
// consumes them: nothing outside the closure keeps them alive.
const freshValues = (count) => ({
  [Symbol.iterator]() {
    let index = 0;
    return {
      next() {
        if (index >= count) return { done: true, value: undefined };
        index += 1;
        return {
          done: false,
          value: { missing: undefined, tag: "item-" + index },
        };
      },
    };
  },
});

describe.runIf(hasGoccia)("for-of iteration head GC roots", () => {
  test("a destructured iteration value survives a collecting default", () => {
    const seen = [];

    for (const { missing = churn(), tag } of freshValues(3)) {
      expect(typeof missing).toBe("number");
      seen.push(tag);
    }

    expect(seen).toEqual(["item-1", "item-2", "item-3"]);
  });

  test("a collecting next() does not lose the previous iteration scope", () => {
    const collecting = {
      [Symbol.iterator]() {
        let index = 0;
        return {
          next() {
            churn();
            if (index >= 3) return { done: true, value: undefined };
            index += 1;
            return { done: false, value: "value-" + index };
          },
        };
      },
    };

    const seen = [];
    for (const value of collecting) seen.push(value);

    expect(seen).toEqual(["value-1", "value-2", "value-3"]);
  });

  test("a yield inside the head does not corrupt the resumed iteration", () => {
    // Resuming into the head takes the branch that skips the iterator
    // advance, so any per-iteration local the head roots must actually have
    // been assigned on this path. The churn after the resume forces a
    // collection while the head's frame is live.
    const holder = {
      *walk() {
        for (const [first = (yield "paused", churn(), "filled")] of [
          [undefined],
          [undefined],
        ]) {
          yield first;
        }
      },
    };

    const iterator = holder.walk();
    const seen = [];
    for (const value of iterator) seen.push(value);

    expect(seen).toEqual(["paused", "filled", "paused", "filled"]);
  });

  test("an await inside the head does not corrupt the resumed iteration", async () => {
    const walk = async () => {
      const seen = [];
      for (const [first = (await Promise.resolve(0), churn(), "filled")] of [
        [undefined],
        [undefined],
      ]) {
        seen.push(first);
      }
      return seen;
    };

    expect(await walk()).toEqual(["filled", "filled"]);
  });
});
