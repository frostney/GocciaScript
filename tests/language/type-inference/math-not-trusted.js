/*---
description: A call on Math is not assumed to return a Number, because Math is a writable global binding
features: [Math, globalThis]
---*/

describe("Math must not be trusted", () => {
  test("a shadowed Math", () => {
    const Math = { PI: "x", min: () => "s", round: () => "r" };
    const v = Math.min(1, 2) + 1;
    expect(v).toBe("s1");
    const w = 3 * Math.PI;
    expect(Number.isNaN(w)).toBe(true);
    const z = Math.round(1.4) + "!";
    expect(z).toBe("r!");
    const y = Math.round(2) + 0.5;
    expect([y === y, y + y]).toEqual([true, "r0.5r0.5"]);
  });

  test("a parameter named Math", () => {
    const f = (Math) => Math.max(1, 2) + 0.5;
    expect(f({ max: () => "m" })).toBe("m0.5");
  });

  test("an overwritten Math.round", () => {
    const saved = globalThis.Math.round;
    globalThis.Math.round = () => "o";
    try {
      const y = globalThis.Math.round(2) + 0.5;
      expect(y).toBe("o0.5");
      const k = Math.round(2) + 0.5;
      expect(k).toBe("o0.5");
    } finally {
      globalThis.Math.round = saved;
    }
  });
});
