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

  test("supports negation", () => {
    expect({ a: 1, b: 2 }).not.toMatchObject({ a: 9 });
  });
});
