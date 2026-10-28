/*---
description: Conditional branches longer than a 16-bit jump offset run correctly
features: [ShadowRealm]
---*/

// Conditional jumps hold a 16-bit offset. One over a longer branch used to
// wrap around: `if (k)` over 40,000 statements took the body for a falsy k.
// Such a jump now takes a long form, and the function's other jumps keep
// theirs.
const realm = new ShadowRealm();
const statements = (count) => "r = r + 1; ".repeat(count);
const operands = (count) => Array.from({ length: count }, () => "k").join(", ");

describe("long conditional branches", () => {
  test("if bodies over 40,000 statements", () => {
    expect(realm.evaluate(`const g = (k) => { let r = 0; if (k) { ${statements(40000)} } return r; }; g(1) + ":" + g(0)`)).toBe("40000:0");
    expect(realm.evaluate(`const g = (k) => { let r = 0; if (k === 1) { ${statements(40000)} } return r; }; g(1) + ":" + g(2)`)).toBe("40000:0");
  });

  test("&&, || and ?: over 40,000 operands", () => {
    expect(realm.evaluate(`const g = (k) => k && (${operands(40000)}, 1); g(1) + ":" + g(0)`)).toBe("1:0");
    expect(realm.evaluate(`const g = (k) => k || (${operands(40000)}, 2); g(1) + ":" + g(0)`)).toBe("1:2");
    expect(realm.evaluate(`const g = (k) => k ? (${operands(40000)}, 1) : 2; g(1) + ":" + g(0)`)).toBe("1:2");
  });

  test("?? over 70,000 operands", () => {
    expect(realm.evaluate(`const g = (k) => k ?? (${operands(70000)}, 1); g(null) + ":" + g(0)`)).toBe("1:0");
  });

  test("an if body over 40,000 statements at the top level of a script", () => {
    for (const k of [1, 0]) {
      expect(new ShadowRealm().evaluate(`let r = 0; const k = ${k}; if (k) { ${statements(40000)} } r`)).toBe(k ? 40000 : 0);
    }
  });

  test("a function with a long branch keeps its short branches correct", () => {
    expect(realm.evaluate(`const g = (k) => { let r = 0; if (k > 1) { r = 7; } else if (k) { ${statements(40000)} } return r + (k ? 1 : 2); }; [g(2), g(1), g(0)].join()`)).toBe("8,40001,2");
  });
});
