/*---
description: Labeled break and continue targets require compat-label
features: [compat-label, compat-while-loops]
---*/

describe("labeled statements", () => {
  test("break targets a labeled do...while loop", () => {
    let iterations = 0;

    LABEL_DO_LOOP: do {
      iterations++;
      break LABEL_DO_LOOP;
      iterations = 99;
    } while (true);

    expect(iterations).toBe(1);
  });

  test("break exits a labeled block", () => {
    let value = 0;

    blockLabel: {
      value = 1;
      break blockLabel;
      value = 2;
    }

    expect(value).toBe(1);
  });

  test("break targets an outer labeled loop", () => {
    let value = 0;

    outer: while (true) {
      while (true) {
        value = 1;
        break outer;
      }
      value = 2;
    }

    expect(value).toBe(1);
  });

  test("continue targets an outer labeled loop", () => {
    let outerCount = 0;
    let visited = "";

    outer: while (outerCount < 3) {
      outerCount++;
      let innerCount = 0;

      while (innerCount < 3) {
        innerCount++;
        if (innerCount === 2) {
          continue outer;
        }
        visited += outerCount + ":" + innerCount + ";";
      }

      visited += "after;";
    }

    expect(visited).toBe("1:1;2:1;3:1;");
  });

  test("continue label must target an enclosing iteration statement", () => {
    expect(() => new Function("block: { continue block; }")).toThrow(SyntaxError);
  });

  test("break to an outer label closes the active iterator", () => {
    let closed = false;
    const iterable = {
      [Symbol.iterator]() {
        return {
          next() {
            return { value: 1, done: false };
          },
          return() {
            closed = true;
            return { done: true };
          },
        };
      },
    };

    outer: {
      for (const value of iterable) {
        break outer;
      }
    }

    expect(closed).toBe(true);
  });

  test("continue to an outer label closes the active iterator", () => {
    let closed = false;
    let outerCount = 0;
    const iterable = {
      [Symbol.iterator]() {
        return {
          next() {
            return { value: 1, done: false };
          },
          return() {
            closed = true;
            return { done: true };
          },
        };
      },
    };

    outer: while (outerCount < 1) {
      outerCount++;
      for (const value of iterable) {
        continue outer;
      }
    }

    expect(closed).toBe(true);
  });
});
