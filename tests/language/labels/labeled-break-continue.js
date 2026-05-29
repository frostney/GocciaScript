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
});
