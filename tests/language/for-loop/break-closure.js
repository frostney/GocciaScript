/*---
description: break inside for-loop closes per-iteration upvalues so closures pin correct values
features: [compat-traditional-for-loop]
---*/

describe("for-loop break with closure capture", () => {
  test("let captures correct per-iteration value on break", () => {
    const fns = [];
    for (let i = 0; i < 5; i++) {
      fns.push(() => i);
      if (i === 2) break;
    }
    expect(fns.map(f => f())).toEqual([0, 1, 2]);
  });

  test("closures survive register reuse after break", () => {
    const fns = [];
    for (let i = 0; i < 5; i++) {
      fns.push(() => i);
      if (i === 1) break;
    }
    let a = 100;
    let b = 200;
    let c = a + b;
    expect(fns.map(f => f())).toEqual([0, 1]);
    expect(c).toBe(300);
  });

  test("break on first iteration still closes upvalue", () => {
    const fns = [];
    for (let i = 0; i < 10; i++) {
      fns.push(() => i);
      break;
    }
    let pad = 0;
    expect(fns[0]()).toBe(0);
  });

  test("nested for-loop break closes inner iteration upvalue", () => {
    const outer = [];
    for (let i = 0; i < 2; i++) {
      const inner = [];
      for (let j = 0; j < 5; j++) {
        inner.push(() => j);
        if (j === 2) break;
      }
      outer.push(inner.map(f => f()));
    }
    expect(outer).toEqual([[0, 1, 2], [0, 1, 2]]);
  });

});
