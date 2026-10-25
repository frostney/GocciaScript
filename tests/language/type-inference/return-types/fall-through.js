/*---
description: A function that can fall off its end returns undefined, so its calls are not typed from its return statements
features: [arrow-function, labeled-statements, switch, try-statement, finally]
---*/

// `u === u` is true for undefined, and a typed Number equality would turn it
// into NaN and answer false.

describe("falling off the end returns undefined", () => {
  test("a return inside a conditional", () => {
    const h = (x) => {
      if (x) return 1;
    };
    const u = h(0);
    expect(u === u).toBe(true);
    expect(u).toBe(undefined);
    const e = (x) => {
      if (x) {
        return 1;
      } else {
        if (x === 2) return 2;
      }
    };
    const v = e(0);
    expect(v === v).toBe(true);
  });

  test("a labeled block that breaks out before the return", () => {
    const lab = (c) => {
      a: {
        if (c) break a;
        return 1;
      }
    };
    const v = lab(true);
    expect(v === v).toBe(true);
  });

  test("loops that return only in some iterations", () => {
    const loop = (a) => {
      for (const x of a) return x * 1;
    };
    const w = loop([]);
    expect(w === w).toBe(true);
    const brk = (a) => {
      for (const x of a) {
        break;
      }
    };
    const b = brk([1]);
    expect(b === b).toBe(true);
  });

  test("a switch with a case that breaks", () => {
    const sw = (x) => {
      switch (x) {
        case 1:
          return 1;
        case 2:
          break;
      }
    };
    const u = sw(2);
    expect(u === u).toBe(true);
    const all = (x) => {
      switch (x) {
        default:
          return 1;
      }
    };
    expect(all(0) + 1).toBe(2);
  });

  test("a try statement whose catch does not return", () => {
    const tr = (x) => {
      try {
        if (x) throw new Error("e");
        return 1;
      } catch (e) {}
    };
    const u = tr(true);
    expect(u === u).toBe(true);
    expect(tr(false) + 1).toBe(2);
  });

  test("a function whose every path returns a Number", () => {
    const f = (x) => {
      if (x) {
        return 1;
      } else {
        return 2;
      }
    };
    const r = f(0);
    expect([r === r, r + r]).toEqual([true, 4]);
    const g = (x) => {
      try {
        return x * 1;
      } finally {
        x = 0;
      }
    };
    const s = g("3");
    expect([s === s, s + s]).toEqual([true, 6]);
  });

  test("finally overrides the return", () => {
    const f = () => {
      try {
        return 1;
      } finally {
        return "s";
      }
    };
    expect(f() + f()).toBe("ss");
  });
});
