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
});
