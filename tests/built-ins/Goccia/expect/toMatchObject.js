class Point {
  constructor(x) {
    this.x = x;
  }
}

describe("toMatchObject", () => {
  test("matches a subset of object keys", () => {
    expect({ a: 1, b: 2 }).toMatchObject({ a: 1 });
    expect({ a: 1, b: 2 }).toMatchObject({ a: 1, b: 2 });
    expect({ a: 1 }).not.toMatchObject({ a: 1, b: 2 });
  });

  test("recurses into nested objects", () => {
    expect({ a: { b: { c: 1, d: 2 } } }).toMatchObject({ a: { b: { c: 1 } } });
    expect({ a: { b: { c: 1 } } }).not.toMatchObject({ a: { b: { c: 2 } } });
  });

  test("recurses per index through arrays of objects", () => {
    expect({ list: [{ a: 1, b: 2 }, { a: 3 }] }).toMatchObject({
      list: [{ a: 1 }, { a: 3 }],
    });
    expect([{ a: 1, b: 2 }]).toMatchObject([{ a: 1 }]);
  });

  test("requires arrays to match in length", () => {
    expect({ list: [{ a: 1 }, { a: 3 }] }).not.toMatchObject({
      list: [{ a: 1 }],
    });
    expect({ list: [{ a: 1 }] }).not.toMatchObject({
      list: [{ a: 1 }, { a: 3 }],
    });
  });

  test("compares arrays of primitives element for element", () => {
    expect({ l: [1, 2] }).toMatchObject({ l: [1, 2] });
    expect({ l: [1, 2] }).not.toMatchObject({ l: [1, 3] });
  });

  test("requires an expected key to be present", () => {
    expect({ a: 1 }).not.toMatchObject({ b: undefined });
  });

  test("ignores the object type", () => {
    expect(new Point(1)).toMatchObject({ x: 1 });
  });

  test("lets an object shape describe an array, but not the reverse", () => {
    expect({ l: [1] }).toMatchObject({ l: { 0: 1 } });
    expect({ l: [1, 2] }).toMatchObject({ l: { 0: 1 } });
    expect({ l: { 0: 1 } }).not.toMatchObject({ l: [1] });
  });

  test("matches Set and Map shapes", () => {
    expect({ s: new Set([1, 2]) }).toMatchObject({ s: new Set([2, 1]) });
    expect({ m: new Map([["a", 1]]) }).toMatchObject({
      m: new Map([["a", 1]]),
    });
  });

  test("matches asymmetric matchers inside arrays", () => {
    expect([{ a: 1 }]).toMatchObject([{ a: expect.any(Number) }]);
    expect([{ a: 1 }]).not.toMatchObject([{ a: expect.any(String) }]);
  });

  test("handles cyclic values", () => {
    // DELIBERATE DIVERGENCE for the cyclic ARRAY case, documented in
    // docs/testing-api.md alongside the DOMException one.
    //
    // Vitest 4.1.10 does not hang here — it dies with "RangeError: Maximum
    // call stack size exceeded" in a few milliseconds, and it does so in BOTH
    // polarities (`toMatchObject` and `not.toMatchObject` alike), so there is
    // no vitest verdict to copy. Only its top-level array walk lacks the cycle
    // guard: the same cyclic array nested one level inside an object matches
    // cleanly, as do cyclic objects, Sets and Maps.
    //
    // Goccia terminates and reports a match, which is exactly the answer
    // vitest itself gives whenever it manages to complete. Reproducing a stack
    // overflow to be bug-compatible would be strictly worse for users.
    const leftArray = [];
    leftArray.push(leftArray);
    const rightArray = [];
    rightArray.push(rightArray);
    expect(leftArray).toMatchObject(rightArray);

    const leftSet = new Set();
    leftSet.add(leftSet);
    const rightSet = new Set();
    rightSet.add(rightSet);
    expect(leftSet).toMatchObject(rightSet);

    const leftMap = new Map();
    leftMap.set("k", leftMap);
    const rightMap = new Map();
    rightMap.set("k", rightMap);
    expect(leftMap).toMatchObject(rightMap);

    const leftObject = {};
    leftObject.self = leftObject;
    const rightObject = {};
    rightObject.self = rightObject;
    expect(leftObject).toMatchObject(rightObject);
  });

  test("terminates on a cyclic array nested inside an object", () => {
    // The nested shape is the one vitest also completes, and it agrees: match.
    const leftArray = [];
    leftArray.push(leftArray);
    const rightArray = [];
    rightArray.push(rightArray);
    expect({ v: leftArray }).toMatchObject({ v: rightArray });
  });

  test("requires an expected undefined key to be present", () => {
    // Vitest is the oracle: an expected key holding undefined still has to
    // exist on the actual side.
    expect({ a: 1 }).not.toMatchObject({ a: 1, b: undefined });
    expect({ a: 1, b: undefined }).toMatchObject({ a: 1, b: undefined });
  });

  test("requires full equality for expected Sets and Maps", () => {
    expect({ s: new Set([1, 2]) }).not.toMatchObject({ s: new Set([1]) });
    expect({ s: new Set([1, 2]) }).toMatchObject({ s: new Set([2, 1]) });
    expect({ m: new Map([["a", 1], ["b", 2]]) }).not.toMatchObject({
      m: new Map([["a", 1]]),
    });
    expect({ m: new Map([["a", 1]]) }).toMatchObject({
      m: new Map([["a", 1]]),
    });
  });

  test("compares expected errors by their fields", () => {
    expect({ e: new Error("x") }).toMatchObject({ e: new Error("x") });
    expect({ e: new Error("x") }).not.toMatchObject({ e: new Error("y") });
    // A plain expected object still describes a subset of the error.
    expect({ e: new Error("x") }).toMatchObject({ e: {} });
  });

  test("supports negation", () => {
    expect({ a: 1, b: 2 }).not.toMatchObject({ a: 9 });
  });
});

// Same exposure as toEqual: see that file for the rationale.
describe.runIf(typeof Goccia !== "undefined")("toMatchObject under explicit GC", () => {
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

  // Reachable only from the expectation while the comparison runs.
  const freshExpected = () => ({
    get k() {
      gcChurn();
      return { x: 1, arr: [1, 2, 3] };
    },
  });

  test("matches through getters that collect", () => {
    expect({ k: { x: 1, arr: [1, 2, 3] }, extra: "tail" }).toMatchObject(
      freshExpected(),
    );
  });

  test("keeps a temporary actual alive across the partial walk", () => {
    expect([{ k: { x: 1, arr: [1, 2, 3] } }]).toContainEqual(freshExpected());
  });
});
