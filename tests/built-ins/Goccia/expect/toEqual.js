class Point {
  constructor(x) {
    this.x = x;
  }
}

class Other {
  constructor(x) {
    this.x = x;
  }
}

describe("toEqual", () => {
  test("compares primitives with Object.is semantics", () => {
    expect(1).toEqual(1);
    expect("a").toEqual("a");
    expect({ v: NaN }).toEqual({ v: NaN });
    expect([-0]).not.toEqual([0]);
    expect([0]).not.toEqual([-0]);
  });

  test("ignores object keys whose value is undefined", () => {
    expect({ a: 1, b: undefined }).toEqual({ a: 1 });
    expect({ a: 1 }).toEqual({ a: 1, b: undefined });
    expect({ a: 1, b: undefined }).toEqual({ a: 1, b: undefined });
  });

  test("ignores undefined keys recursively", () => {
    expect({ n: { a: 1, b: undefined } }).toEqual({ n: { a: 1 } });
    expect([{ a: 1, b: undefined }]).toEqual([{ a: 1 }]);
  });

  test("still compares keys that hold a defined value", () => {
    expect({ a: 1, b: 2 }).not.toEqual({ a: 1 });
    expect({ a: 1 }).not.toEqual({ a: 1, b: 2 });
    expect({ a: 1, b: null }).not.toEqual({ a: 1 });
  });

  test("ignores undefined array items past the shorter length", () => {
    expect([1, undefined]).toEqual([1]);
    expect([2]).toEqual([2, undefined]);
    expect([]).toEqual([undefined]);
    expect([1, 2]).toEqual([1, 2, undefined, undefined]);
  });

  test("does not shift array items to absorb an undefined", () => {
    expect([undefined, 1]).not.toEqual([1]);
    expect([1, undefined, 2]).not.toEqual([1, 2]);
  });

  test("treats an array hole as undefined", () => {
    expect([1, , 3]).toEqual([1, undefined, 3]);
    expect([1, , 3]).toEqual([1, , 3]);
  });

  test("ignores the object type", () => {
    expect(new Point(1)).toEqual({ x: 1 });
    expect({ x: 1 }).toEqual(new Point(1));
    expect(new Point(1)).toEqual(new Other(1));
  });

  test("compares Sets without regard to insertion order", () => {
    expect(new Set([1, 2])).toEqual(new Set([2, 1]));
    expect(new Set([{ a: 1 }, { b: 2 }])).toEqual(new Set([{ b: 2 }, { a: 1 }]));
    expect(new Set([1, 2])).not.toEqual(new Set([1]));
    expect(new Set([1, 2])).not.toEqual(new Set([1, 3]));
    expect(new Set([undefined])).not.toEqual(new Set());
  });

  test("compares Maps without regard to insertion order", () => {
    expect(new Map([["k", [1, 2]]])).toEqual(new Map([["k", [1, 2]]]));
    expect(new Map([["a", 1], ["b", 2]])).toEqual(
      new Map([["b", 2], ["a", 1]]),
    );
    expect(new Map([["a", { z: 1 }], ["b", 2]])).toEqual(
      new Map([["b", 2], ["a", { z: 1 }]]),
    );
    expect(new Map([[{ id: 1 }, "v"]])).toEqual(new Map([[{ id: 1 }, "v"]]));
    expect(new Map([["a", 1]])).not.toEqual(new Map([["a", 2]]));
    expect(new Map([["a", 1]])).not.toEqual(new Map());
    expect(new Map([["a", undefined]])).not.toEqual(new Map());
  });

  test("never equates different kinds of container", () => {
    expect(new Map([["a", 1]])).not.toEqual({ a: 1 });
    expect(new Set([1, 2])).not.toEqual([1, 2]);
    expect({ 0: 1, length: 1 }).not.toEqual([1]);
    expect(new Set([1])).not.toEqual(new Map([[1, 1]]));
  });

  test("handles cyclic structures", () => {
    const left = { name: "root" };
    left.self = left;
    const right = { name: "root" };
    right.self = right;

    expect(left).toEqual(right);
  });

  test("supports negation", () => {
    expect({ a: 1 }).not.toEqual({ a: 2 });
    expect([1]).not.toEqual([1, 2]);
  });
});
