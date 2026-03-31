/*---
description: Iterator.prototype.drop helper behavior
features: [Iterator.prototype.drop]
---*/

describe("Iterator.prototype.drop()", () => {
  test("drops first N elements", () => {
    expect([1, 2, 3, 4, 5].values().drop(2).toArray()).toEqual([3, 4, 5]);
  });

  test("drop zero keeps all elements", () => {
    expect([1, 2, 3].values().drop(0).toArray()).toEqual([1, 2, 3]);
  });

  test("drop all elements can exhaust the iterator", () => {
    expect([1, 2, 3].values().drop(5).toArray()).toEqual([]);
  });

  test("drop rejects negative counts", () => {
    expect(() => [1].values().drop(-1)).toThrow(RangeError);
  });

  test("drop skips values lazily on the first next() call", () => {
    const source = [1, 2, 3, 4, 5].values();
    const dropped = source.drop(2);

    expect(dropped.next().value).toBe(3);
    expect(dropped.next().value).toBe(4);
    expect(dropped.next().value).toBe(5);
    expect(dropped.next().done).toBe(true);
  });
});
