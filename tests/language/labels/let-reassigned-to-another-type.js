/*---
description: A let reassigned to another type before a labeled continue or break keeps JavaScript semantics at earlier reads
features: [compat-label, let-declaration]
---*/

describe("labeled continue and break", () => {
  test("a labeled continue to the outer loop", () => {
    let x = 1;
    const seen = [];
    outer: for (const i of [0, 1]) {
      for (const j of [0, 1]) {
        seen.push(x + 1);
        x = "s";
        continue outer;
      }
    }
    expect(seen).toEqual([2, "s1"]);
  });

  test("a labeled break out of a block", () => {
    const run = (flag) => {
      let x = 1;
      block: {
        x = "s";
        if (flag) break block;
        x = 2;
      }
      return x + 1;
    };
    expect(run(true)).toBe("s1");
    expect(run(false)).toBe(3);
  });
});
