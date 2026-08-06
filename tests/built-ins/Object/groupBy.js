describe("Object.groupBy", () => {
  test("groups array elements by callback result", () => {
    const result = Object.groupBy([1, 2, 3, 4, 5], (n) => n % 2 === 0 ? "even" : "odd");
    expect(result.odd).toEqual([1, 3, 5]);
    expect(result.even).toEqual([2, 4]);
  });

  test("empty array returns empty object", () => {
    const result = Object.groupBy([], () => "a");
    expect(Object.keys(result).length).toBe(0);
  });

  test("all items in same group", () => {
    const result = Object.groupBy([1, 2, 3], () => "all");
    expect(result.all).toEqual([1, 2, 3]);
  });

  test("callback receives element and index", () => {
    const indices = [];
    Object.groupBy(["a", "b"], (el, idx) => {
      indices.push(idx);
      return "group";
    });
    expect(indices).toEqual([0, 1]);
  });

  test("result has null prototype", () => {
    const result = Object.groupBy([1], () => "a");
    expect(Object.getPrototypeOf(result)).toBe(null);
  });
});

// ToPropertyKey runs the key's @@toPrimitive or toString, which is a GC safe
// point. Between the callback returning and the item being stored into its
// group, both the item and the key live only in native locals.
describe.runIf(typeof Goccia !== "undefined")("Object.groupBy under explicit GC", () => {
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

  test("keeps the item alive when the key's toPrimitive collects", () => {
    const freshItems = {
      [Symbol.iterator]() {
        let index = 0;
        return {
          next() {
            index++;
            if (index > 3) {
              return { done: true };
            }
            return {
              value: { v: index, pad: [index, index + 1], tag: "t" + index },
              done: false,
            };
          },
        };
      },
    };

    let counter = 0;
    const groups = Object.groupBy(freshItems, () => ({
      [Symbol.toPrimitive]() {
        gcChurn();
        return "g" + ++counter;
      },
    }));

    expect(groups.g1[0].v).toBe(1);
    expect(groups.g2[0].v).toBe(2);
    expect(groups.g3[0].tag).toBe("t3");
  });
});
