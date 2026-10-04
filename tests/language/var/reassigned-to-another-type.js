/*---
description: A var that starts as a number and is later given another type keeps JavaScript semantics at every read
features: [compat-var]
---*/

describe("var reassigned to another type", () => {
  test("an assignment later in a loop body", () => {
    const run = () => {
      var x = 1;
      const seen = [];
      for (const step of [0, 1]) {
        seen.push(x + 1);
        x = "s";
      }
      return seen;
    };
    expect(run()).toEqual([2, "s1"]);
  });

  test("a read before the declaration on a later iteration", () => {
    const run = () => {
      const seen = [];
      for (const step of [0, 1]) {
        seen.push(x + 1);
        var x = "s";
      }
      return seen;
    };
    const seen = run();
    expect(Number.isNaN(seen[0])).toBe(true);
    expect(seen[1]).toBe("s1");
  });

  test("a declaration in a branch that did not run", () => {
    const run = (flag) => {
      if (flag) {
        var x = 1;
      }
      return x + 1;
    };
    expect(run(true)).toBe(2);
    expect(Number.isNaN(run(false))).toBe(true);
  });
});
