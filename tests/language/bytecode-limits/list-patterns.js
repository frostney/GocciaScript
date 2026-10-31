/*---
description: List patterns longer than a 16-bit immediate count every element
features: [pattern-matching, Function]
---*/

// A list pattern loads its length and its rest start as numbers; past 32,767
// they used to wrap around, so a 40,000-element pattern matched no array of
// that length and its rest began at the wrong element.
describe("long list patterns", () => {
  const holes = ",".repeat(40000);
  const range = (count) => Array.from({ length: count }, (_, i) => i);

  test("a 40,000-element pattern matches arrays of exactly that length", () => {
    const matches = new Function("v", `return v is [${holes}];`);
    expect(matches(range(40000))).toBe(true);
    expect(matches(range(40001))).toBe(false);
  });

  test("a rest pattern after 40,000 elements binds the remainder", () => {
    const tail = new Function("v", `if (v is [${holes} ...const t]) { return t; } return null;`);
    expect(tail(range(40002))).toEqual([40000, 40001]);
    expect(tail(range(39999))).toBe(null);
  });
});
