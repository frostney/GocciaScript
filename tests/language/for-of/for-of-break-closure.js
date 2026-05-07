/*---
description: break inside for...of closes per-iteration upvalues so closures pin correct values
features: [for-of]
---*/

describe("for...of break with closure capture", () => {
  test("closures capture correct values when break exits early", () => {
    const fns = [];
    for (const i of [0, 1, 2]) {
      fns.push(() => i);
      if (i === 1) break;
    }
    expect(fns.map(f => f())).toEqual([0, 1]);
  });

  test("closures survive register reuse after break", () => {
    const fns = [];
    for (const i of [10, 20, 30, 40]) {
      fns.push(() => i);
      if (i === 20) break;
    }
    let a = 100;
    let b = 200;
    let c = 300;
    let d = a + b + c;
    expect(fns.map(f => f())).toEqual([10, 20]);
    expect(d).toBe(600);
  });

  test("break on first iteration still closes upvalue", () => {
    const fns = [];
    for (const x of [42]) {
      fns.push(() => x);
      break;
    }
    let pad = 0;
    expect(fns[0]()).toBe(42);
  });

  test("destructured binding closed on break", () => {
    const fns = [];
    for (const [a, b] of [[1, 2], [3, 4], [5, 6]]) {
      fns.push(() => a + b);
      if (a === 3) break;
    }
    expect(fns.map(f => f())).toEqual([3, 7]);
  });

  test("nested for-of break closes inner iteration upvalue", () => {
    const outer = [];
    for (const x of [1, 2]) {
      const inner = [];
      for (const y of [10, 20, 30]) {
        inner.push(() => y);
        if (y === 20) break;
      }
      outer.push(inner.map(f => f()));
    }
    expect(outer).toEqual([[10, 20], [10, 20]]);
  });

});
