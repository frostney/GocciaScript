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

  test("pairs members off so a matcher cannot strand a literal", () => {
    // The literal 1 must claim its partner before expect.any(Number) takes it.
    expect(new Set([1, 2])).toEqual(new Set([expect.any(Number), 1]));
    expect(new Set([1, 2])).toEqual(new Set([1, expect.any(Number)]));
    expect(new Set([1, 2])).toEqual(
      new Set([expect.any(Number), expect.any(Number)]),
    );
    expect(new Set([{ a: 1 }, { a: 2 }])).toEqual(
      new Set([expect.objectContaining({ a: 2 }), { a: 1 }]),
    );
    expect(new Set([1, 2])).not.toEqual(new Set([expect.any(String), 1]));
  });

  test("pairs Map entries off the same way", () => {
    expect(new Map([[1, "x"], [2, "x"]])).toEqual(
      new Map([[expect.any(Number), "x"], [1, "x"]]),
    );
    expect(new Map([[1, "x"], [2, "x"]])).toEqual(
      new Map([[1, "x"], [expect.any(Number), "x"]]),
    );
    expect(new Map([["a", 1], ["b", 2]])).toEqual(
      new Map([["b", expect.any(Number)], ["a", 1]]),
    );
    expect(new Map([[1, "x"]])).not.toEqual(
      new Map([[expect.any(String), "x"]]),
    );
  });

  test("requires every member to find its own partner", () => {
    // bun accepts these because it never marks an expected member as claimed;
    // requiring a distinct partner per member is the stricter reading.
    expect(new Set([1, 2])).not.toEqual(new Set([expect.any(Number), 3]));
    expect(new Set([{ a: 1 }, { a: 1 }])).not.toEqual(
      new Set([{ a: 1 }, { b: 2 }]),
    );
  });

  test("ignores a trailing hole", () => {
    expect([1, ,]).toEqual([1]);
  });

  test("handles cyclic arrays", () => {
    const left = [];
    left.push(left);
    const right = [];
    right.push(right);

    expect(left).toEqual(right);

    const leftNested = [1, []];
    leftNested[1].push(leftNested);
    const rightNested = [1, []];
    rightNested[1].push(rightNested);

    expect(leftNested).toEqual(rightNested);
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
