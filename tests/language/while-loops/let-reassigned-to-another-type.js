/*---
description: A let that a while or do...while loop reassigns to another type keeps JavaScript semantics at earlier reads
features: [while-loops, let-declaration]
---*/

describe("while and do...while loops", () => {
  test("a while loop", () => {
    let x = 1;
    let i = 0;
    const seen = [];
    while (i < 2) {
      seen.push(x + 1);
      x = "s";
      i++;
    }
    expect(seen).toEqual([2, "s1"]);
  });

  test("a do...while loop", () => {
    let x = 1;
    let i = 0;
    const seen = [];
    do {
      seen.push(x + 1);
      x = "s";
      i++;
    } while (i < 2);
    expect(seen).toEqual([2, "s1"]);
  });

  test("a read in the condition", () => {
    let x = 0;
    let n = 0;
    const seen = [];
    while (x + 1 < 100 && n < 3) {
      seen.push(x + 1);
      x = "9";
      n++;
    }
    expect(seen).toEqual([1, "91", "91"]);
  });
});
