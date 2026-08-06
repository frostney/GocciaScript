/*---
description: structuredClone deep-clones objects
features: [structuredClone]
---*/

test("clones a simple object", () => {
  const original = { a: 1, b: "hello", c: true };
  const clone = structuredClone(original);
  expect(clone.a).toBe(1);
  expect(clone.b).toBe("hello");
  expect(clone.c).toBe(true);
});

test("clone is a distinct object", () => {
  const original = { x: 1 };
  const clone = structuredClone(original);
  clone.x = 2;
  expect(original.x).toBe(1);
  expect(clone.x).toBe(2);
});

test("clones nested objects", () => {
  const original = { a: { b: { c: 42 } } };
  const clone = structuredClone(original);
  expect(clone.a.b.c).toBe(42);
  clone.a.b.c = 99;
  expect(original.a.b.c).toBe(42);
});

test("clones object with null prototype value", () => {
  const original = { a: null, b: undefined };
  const clone = structuredClone(original);
  expect(clone.a).toBe(null);
  expect(clone.b).toBe(undefined);
});

test("clones object with mixed value types", () => {
  const original = {
    num: 42,
    str: "hello",
    bool: true,
    nil: null,
    undef: undefined,
    nested: { x: 1 },
    arr: [1, 2, 3],
  };
  const clone = structuredClone(original);
  expect(clone.num).toBe(42);
  expect(clone.str).toBe("hello");
  expect(clone.bool).toBe(true);
  expect(clone.nil).toBe(null);
  expect(clone.undef).toBe(undefined);
  expect(clone.nested.x).toBe(1);
  expect(clone.arr.length).toBe(3);
  expect(clone.arr[0]).toBe(1);
});

// Accessor properties are read through their getter, which is a GC safe point.
// The half-built clone is reachable only from a native local and the internal
// memory map the collector cannot see.
describe.runIf(typeof Goccia !== "undefined")("structuredClone under explicit GC", () => {
  // A bare gc() usually leaves the freed slot readable; the allocation churn
  // afterwards is what makes a collected value observable.
  const gcChurn = () => {
    Goccia.gc();
    let total = 0;
    for (const i of [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]) {
      const scratch = { a: i * 7.5, b: [i, i + 1], c: "x" + i };
      total += scratch.a + scratch.b[0];
    }
    return total;
  };

  test("keeps the in-progress clone alive when a getter collects", () => {
    const source = {
      head: 1,
      get middle() {
        gcChurn();
        return { nested: "value", arr: [1, 2, 3] };
      },
      tail: "end",
    };

    const clone = structuredClone(source);
    expect(clone.head).toBe(1);
    expect(clone.middle.nested).toBe("value");
    expect(clone.middle.arr).toEqual([1, 2, 3]);
    expect(clone.tail).toBe("end");
  });

  test("keeps nested clones alive when a deep getter collects", () => {
    const source = {
      a: {
        b: {
          get c() {
            gcChurn();
            return { d: [1, 2, 3] };
          },
        },
      },
      e: 3,
    };

    const clone = structuredClone(source);
    expect(clone.a.b.c.d).toEqual([1, 2, 3]);
    expect(clone.e).toBe(3);
  });
});
