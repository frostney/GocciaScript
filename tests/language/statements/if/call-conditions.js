/*---
description: A call used as a branch condition is tested on the value the call returned, for every callee form
features: [if-statement, optional-chaining, class-private-methods]
---*/

const falsy = [false, 0, -0, NaN, "", 0n, null, undefined];
const truthy = [true, 1, "a", 1n, {}, []];

describe("call conditions", () => {
  test("a method call whose receiver is truthy decides on the call's result", () => {
    for (const v of [...falsy, ...truthy]) {
      const o = { m() { return v; }, k: "m" };
      let taken = false;
      if (o.m()) { taken = true; }
      expect(taken).toBe(Boolean(v));
      expect(o["m"]() ? 1 : 2).toBe(v ? 1 : 2);
      expect(o[o.k]() ? 1 : 2).toBe(v ? 1 : 2);
      expect(!o.m()).toBe(!v);
      let both = false;
      if (o.m() && o.m()) { both = true; }
      expect(both).toBe(Boolean(v));
      let either = false;
      if (o.m() || o.m()) { either = true; }
      expect(either).toBe(Boolean(v));
    }
  });

  test("an optional call that short-circuits is a false condition", () => {
    const nul = null;
    const o = { n: null };
    let taken = false;
    if (nul?.m()) { taken = true; }
    if (o.missing?.()) { taken = true; }
    if (o.n?.()) { taken = true; }
    if (nul?.a.b()) { taken = true; }
    if (nul?.["m"]()) { taken = true; }
    expect(taken).toBe(false);
    expect(nul?.m() ? 1 : 2).toBe(2);
    expect(!o.missing?.()).toBe(true);
  });

  test("an optional call that runs decides on its result", () => {
    for (const v of [...falsy, ...truthy]) {
      const o = { m() { return v; } };
      expect(o?.m() ? 1 : 2).toBe(v ? 1 : 2);
      expect(o.m?.() ? 1 : 2).toBe(v ? 1 : 2);
    }
  });

  test("private and super method calls decide on their result", () => {
    class A { m() { return 0; } t() { return 1; } }
    class B extends A {
      #p() { return 0; }
      run() {
        const r = [];
        r.push(this.#p() ? "T" : "F");
        r.push(super.m() ? "T" : "F");
        r.push(super.t() ? "T" : "F");
        r.push(super.missing?.() ? "T" : "F");
        if (super.m()) { r.push("T"); } else { r.push("F"); }
        return r.join("");
      }
    }
    expect(new B().run()).toBe("FFTFF");
  });
});
