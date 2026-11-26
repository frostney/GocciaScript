/*---
description: delete obj[key] deletes from the object read before the key expression runs, even when the key reassigns the variable
features: [delete]
---*/

// ES2026 13.5.1.2 and 13.3.2.1: the member expression's object is evaluated
// before its key, so a key expression that reassigns the variable holding the
// object does not change which object the delete reaches.
describe("delete with a key expression that reassigns the object's variable", () => {
  test("the first object loses the property and the new one keeps it", () => {
    let o = { k: 1 };
    const first = o;
    const other = { k: 2 };
    expect(delete o[(o = other, "k")]).toBe(true);
    expect("k" in first).toBe(false);
    expect(other.k).toBe(2);
    expect(o).toBe(other);
  });

  test("an optional chain reads the object before the key too", () => {
    let o = { k: 1 };
    const first = o;
    const other = { k: 2 };
    expect(delete o?.[(o = other, "k")]).toBe(true);
    expect("k" in first).toBe(false);
    expect(other.k).toBe(2);
  });
});
