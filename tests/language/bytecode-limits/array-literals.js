/*---
description: An array literal has no element limit of its own, and a function holds more than 65,535 constants
features: [ShadowRealm]
---*/

const realm = new ShadowRealm();
const list = (count, item, separator = ", ") =>
  Array.from({ length: count }, (_, i) => item(i)).join(separator);

describe("array literals and the constant limit", () => {
  test("66,000 repeated elements evaluate", () => {
    expect(realm.evaluate(`[${list(66000, () => "0")}].length`)).toBe(66000);
  });

  // A function's constants are not limited to 65,535 (the constant index is
  // not a 16-bit operand): 65,600 distinct strings evaluate.
  test("65,600 distinct strings evaluate", () => {
    expect(realm.evaluate(`const a = [${list(65600, (i) => `"s${i}"`)}]; a.length + ":" + a[65599]`)).toBe("65600:s65599");
  });
});
