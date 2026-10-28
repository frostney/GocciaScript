/*---
description: A switch whose case tests span more than a 16-bit jump offset runs correctly
features: [ShadowRealm]
---*/

// A switch with 20,000 or more cases used to return wrong values, loop
// forever or crash, because its case jumps wrapped around.
const realm = new ShadowRealm();
const cases = (count) =>
  Array.from({ length: count }, (_, i) => `case ${i}: return ${i};`).join(" ");

describe("long switch statements", () => {
  test("switches with 10,000 and 20,000 cases", () => {
    for (const count of [10000, 20000]) {
      expect(realm.evaluate(`const g = (k) => { switch (k) { ${cases(count)} } return -1; }; [g(0), g(1), g(${count - 1}), g(${count})].join()`)).toBe(`0,1,${count - 1},-1`);
    }
  });

  // Each case body is longer than its test, so the later cases' jumps are
  // out of reach first, and the long forms they take lengthen the earlier
  // cases' jumps across them until those need it too.
  test("a switch whose case bodies are longer than its case tests", () => {
    const longBodies = Array.from({ length: 6000 }, (_, i) => `case ${i}: r = r + ${i}; r = r * 2; return r;`).join(" ");
    const keys = [0, 1, 2999, 3000, 5999, 6000];
    expect(realm.evaluate(`const g = (k) => { let r = 1; switch (k) { ${longBodies} } return -1; }; [${keys.map((k) => `g(${k})`).join(", ")}].join()`)).toBe(keys.map((k) => (k < 6000 ? (1 + k) * 2 : -1)).join());
  });

  test("switches with a declaration in each clause at the top level of a script, braced or not", () => {
    const flat = (count) => Array.from({ length: count }, (_, i) => `case ${i}: const s${i} = ${i}; out.push(s${i}); break;`).join(" ");
    const braced = (count) => Array.from({ length: count }, (_, i) => `case ${i}: { const s = ${i}; out.push(s); break; }`).join(" ");
    for (const clauses of [flat, braced]) {
      const count = 12000;
      // A realm of its own for each script, whose top-level bindings stay global.
      expect(new ShadowRealm().evaluate(`const out = []; for (const k of [0, 1, ${count - 1}, ${count}]) { switch (k) { ${clauses(count)} default: out.push(-1); } } out.join()`)).toBe(`0,1,${count - 1},-1`);
    }
  });
});
