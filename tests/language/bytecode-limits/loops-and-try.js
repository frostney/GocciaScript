/*---
description: Loop bodies and try blocks longer than a 16-bit jump offset run correctly
features: [ShadowRealm]
---*/

const realm = new ShadowRealm();
const statements = (count) => "r = r + 1; ".repeat(count);

describe("long loop bodies and try blocks", () => {
  test("for...of, while and do...while bodies over 20,000 statements", () => {
    expect(realm.evaluate(`const g = (a) => { let r = 0; for (const v of a) { ${statements(20000)} } return r; }; g([1, 2]) + ":" + g([])`)).toBe("40000:0");
    expect(realm.evaluate(`const g = (n) => { let r = 0; let i = 0; while (i < n) { i = i + 1; ${statements(20000)} } return r; }; g(2) + ":" + g(0)`)).toBe("40000:0");
    expect(realm.evaluate(`const g = (n) => { let r = 0; let i = 0; do { i = i + 1; ${statements(40000)} } while (i < n); return r; }; g(2) + ":" + g(0)`)).toBe("80000:40000");
  });

  test("a do...while whose condition sits in a register past 255", () => {
    const lets = Array.from({ length: 300 }, (_, i) => `let v${i} = ${i};`).join(" ");
    expect(realm.evaluate(`const g = (n) => { ${lets} let i = 0; let r = 0; do { i = i + 1; r = r + v299; } while (i < n); return r; }; g(1) + ":" + g(3)`)).toBe("299:897");
  });

  test("try blocks over 40,000 statements", () => {
    expect(realm.evaluate(`const g = (k) => { let r = 0; try { ${statements(40000)} if (k) throw 1; } catch (e) { r = -r; } finally { r = r + 0.5; } return r; }; g(1) + ":" + g(0)`)).toBe("-39999.5:40000.5");
  });
});
