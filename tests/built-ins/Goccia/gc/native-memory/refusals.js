/*---
description: >
  Allocations whose size the script chose are refused before they are made
  when they cannot fit under the memory limit, and memory held outside the
  heap (property keys, symbols, a builder's text) is refused once it fills the
  limit. Each refusal is a catchable RangeError, and the program goes on.
features: [Goccia.gc]
---*/

const refusal = (fn) => {
  try {
    fn();
    return "none";
  } catch (error) {
    return error instanceof RangeError ? "RangeError" : String(error);
  }
};

describe("memory limit refusals", () => {
  test("an array's elements, before they are allocated", () => {
    expect(refusal(() => new Array(6e7))).toBe("RangeError");
    expect(refusal(() => { const a = []; a.length = 6e7; })).toBe("RangeError");
  });

  test("a built string, before it is allocated", () => {
    expect(refusal(() => "".padEnd(5e8, "ab"))).toBe("RangeError");
    expect(refusal(() => "ab".repeat(2.5e8))).toBe("RangeError");
  });

  test("a copy whose indices cannot fit, before it reads the source", () => {
    let reads = 0;
    const source = { length: 7e7, get 0() { reads++; return 0; } };
    expect(refusal(() => Array.prototype.toSpliced.call(source, 0, 0))).toBe("RangeError");
    expect(reads).toBe(0);
  });

  test("a builder's text, once it fills the limit", () => {
    expect(refusal(() => new Array(2e8).join("ab"))).toBe("RangeError");
  });

  test("fresh property keys, once they fill the limit", () => {
    expect(refusal(() => {
      const o = {};
      for (const i of Array.from({ length: 2e6 }, (_, i) => i)) o["k" + i] = 0;
    })).toBe("RangeError");
  });

  test("the program goes on after each refusal", () => {
    expect(Array.from({ length: 1000 }, (_, i) => i * 2)[999]).toBe(1998);
  });
});
