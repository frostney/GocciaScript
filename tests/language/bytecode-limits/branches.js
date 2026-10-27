/*---
description: Branches longer than a 16-bit jump offset run correctly
features: [ShadowRealm]
---*/

// Conditional jumps hold a signed 16-bit offset. One over a longer branch
// used to wrap around: `if (k)` over 40,000 statements took the body for a
// falsy k, and a switch with 20,000 or more cases returned wrong values,
// looped forever or crashed. Such a function now compiles with its
// conditional jumps in a long form.
const realm = new ShadowRealm();
const statements = (count) => "r = r + 1; ".repeat(count);
const operands = (count) => Array.from({ length: count }, () => "k").join(", ");
const cases = (count) =>
  Array.from({ length: count }, (_, i) => `case ${i}: return ${i};`).join(" ");

describe("long branches", () => {
  test("if bodies over 10,000 and 40,000 statements", () => {
    for (const count of [10000, 40000]) {
      expect(realm.evaluate(`const g = (k) => { let r = 0; if (k) { ${statements(count)} } return r; }; g(1) + ":" + g(0)`)).toBe(`${count}:0`);
      expect(realm.evaluate(`const g = (k) => { let r = 0; if (k === 1) { ${statements(count)} } return r; }; g(1) + ":" + g(2)`)).toBe(`${count}:0`);
    }
  });

  test("&&, || and ?: over 40,000 operands", () => {
    expect(realm.evaluate(`const g = (k) => k && (${operands(40000)}, 1); g(1) + ":" + g(0)`)).toBe("1:0");
    expect(realm.evaluate(`const g = (k) => k || (${operands(40000)}, 1); g(1) + ":" + g(0)`)).toBe("1:1");
    expect(realm.evaluate(`const g = (k) => k ? (${operands(40000)}, 1) : 2; g(1) + ":" + g(0)`)).toBe("1:2");
  });

  test("?? over 70,000 operands", () => {
    expect(realm.evaluate(`const g = (k) => k ?? (${operands(70000)}, 1); g(null) + ":" + g(0)`)).toBe("1:0");
  });

  test("switches with 10,000, 20,000 and 70,000 cases", () => {
    for (const count of [10000, 20000, 70000]) {
      expect(realm.evaluate(`const g = (k) => { switch (k) { ${cases(count)} } return -1; }; [g(0), g(1), g(${count - 1}), g(${count})].join()`)).toBe(`0,1,${count - 1},-1`);
    }
  });

  test("loop bodies over 40,000 statements", () => {
    expect(realm.evaluate(`const g = (a) => { let r = 0; for (const v of a) { ${statements(40000)} } return r; }; g([1, 2]) + ":" + g([])`)).toBe("80000:0");
    expect(realm.evaluate(`const g = (n) => { let r = 0; let i = 0; while (i < n) { i = i + 1; ${statements(40000)} } return r; }; g(2) + ":" + g(0)`)).toBe("80000:0");
    expect(realm.evaluate(`const g = (n) => { let r = 0; let i = 0; do { i = i + 1; ${statements(40000)} } while (i < n); return r; }; g(2) + ":" + g(0)`)).toBe("80000:40000");
  });

  test("try blocks over 70,000 statements", () => {
    expect(realm.evaluate(`const g = (k) => { let r = 0; try { ${statements(70000)} if (k) throw 1; } catch (e) { r = -r; } finally { r = r + 0.5; } return r; }; g(1) + ":" + g(0)`)).toBe("-69999.5:70000.5");
  });

  test("a function with a long branch keeps its short branches correct", () => {
    expect(realm.evaluate(`const g = (k) => { let r = 0; if (k > 1) { r = 7; } else if (k) { ${statements(40000)} } return r + (k ? 1 : 2); }; [g(2), g(1), g(0)].join()`)).toBe("8,40001,2");
  });
});
