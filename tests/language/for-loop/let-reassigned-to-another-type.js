/*---
description: A let that a traditional for loop reassigns to another type keeps JavaScript semantics at earlier reads
features: [compat-traditional-for-loop, let-declaration]
---*/

describe("traditional for loop", () => {
  test("an assignment later in the body", () => {
    let x = 1;
    const seen = [];
    for (let i = 0; i < 2; i++) {
      seen.push(x + 1, x - 1);
      x = "4";
    }
    expect(seen).toEqual([2, 0, "41", 3]);
  });

  test("an assignment in the update clause", () => {
    let x = 1;
    const seen = [];
    for (let i = 0; i < 2; i++, x = "s") {
      seen.push(x + 1);
    }
    expect(seen).toEqual([2, "s1"]);
  });

  test("a loop variable assigned another type", () => {
    const seen = [];
    for (let i = 0; seen.push(i) < 3; i = i + 1) {
      if (i === 0) {
        i = "1";
      }
    }
    expect(seen).toEqual([0, "11", "111"]);
  });

  test("a read in the condition", () => {
    let x = 0;
    const seen = [];
    for (let n = 0; x + 1 < 100 && n < 3; n++) {
      seen.push(x + 1);
      x = "9";
    }
    expect(seen).toEqual([1, "91", "91"]);
  });

  test("numeric counters and accumulators", () => {
    let total = 0;
    let product = 1;
    for (let i = 1; i <= 5; i = i + 1) {
      total += i;
      product = product * i;
    }
    expect([total, product, total + product]).toEqual([15, 120, 135]);
  });
});
