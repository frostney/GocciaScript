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

class CustomError extends Error {}

describe("toStrictEqual", () => {
  test("compares plain values like toEqual", () => {
    expect({ a: 1, b: [2, 3] }).toStrictEqual({ a: 1, b: [2, 3] });
    expect({ v: NaN }).toStrictEqual({ v: NaN });
    expect([-0]).not.toStrictEqual([0]);
  });

  test("enforces keys whose value is undefined", () => {
    expect({ a: 1, b: undefined }).not.toStrictEqual({ a: 1 });
    expect({ a: 1 }).not.toStrictEqual({ a: 1, b: undefined });
    expect({ a: 1, b: undefined }).toStrictEqual({ a: 1, b: undefined });
  });

  test("enforces undefined keys recursively", () => {
    expect({ n: { a: 1, b: undefined } }).not.toStrictEqual({ n: { a: 1 } });
  });

  test("enforces array length against undefined items", () => {
    expect([2]).not.toStrictEqual([2, undefined]);
    expect([2, undefined]).not.toStrictEqual([2]);
    expect([2, undefined]).toStrictEqual([2, undefined]);
  });

  test("preserves array sparseness", () => {
    expect([1, , 3]).not.toStrictEqual([1, undefined, 3]);
    expect([1, undefined, 3]).not.toStrictEqual([1, , 3]);
    expect([1, , 3]).toStrictEqual([1, , 3]);
  });

  test("distinguishes a class instance from a plain object", () => {
    expect(new Point(1)).not.toStrictEqual({ x: 1 });
    expect({ x: 1 }).not.toStrictEqual(new Point(1));
  });

  test("distinguishes instances of different classes", () => {
    expect(new Point(1)).not.toStrictEqual(new Other(1));
  });

  test("accepts instances of the same class", () => {
    expect(new Point(1)).toStrictEqual(new Point(1));
    expect(new Point(1)).not.toStrictEqual(new Point(2));
  });

  test("treats every non-class object as plain", () => {
    const nullPrototype = Object.create(null);
    nullPrototype.x = 1;
    expect(nullPrototype).toStrictEqual({ x: 1 });

    const shared = { marker: true };
    const left = Object.create(shared);
    left.x = 1;
    const right = Object.create(shared);
    right.x = 1;
    expect(left).toStrictEqual(right);
    expect(left).toStrictEqual({ x: 1 });
    expect(new Point(1)).not.toStrictEqual(nullPrototype);
  });

  test("keeps Set and Map order-insensitive", () => {
    expect(new Set([1, 2])).toStrictEqual(new Set([2, 1]));
    expect(new Map([["a", 1], ["b", 2]])).toStrictEqual(
      new Map([["b", 2], ["a", 1]]),
    );
  });

  test("applies the type check to Set members", () => {
    expect(new Set([new Point(1)])).not.toStrictEqual(new Set([{ x: 1 }]));
    expect(new Set([new Point(1)])).toStrictEqual(new Set([new Point(1)]));
  });

  test("handles cyclic arrays", () => {
    const left = [];
    left.push(left);
    const right = [];
    right.push(right);

    expect(left).toStrictEqual(right);
  });

  test("applies error identity without adding a class check", () => {
    expect(new Error("a")).toStrictEqual(new Error("a"));
    expect(new Error("a")).not.toStrictEqual(new Error("b"));
    expect(new TypeError("m")).not.toStrictEqual(new Error("m"));
    expect(new Error("a")).not.toStrictEqual({});

    // Unlike plain objects, a subclass instance is not distinguished from a
    // base Error: the name is what identifies an error.
    expect(new CustomError("m")).toStrictEqual(new Error("m"));
    expect(new CustomError("m")).toStrictEqual(new CustomError("m"));
  });

  test("compares the cause strictly too", () => {
    expect(new Error("m", { cause: "c" })).toStrictEqual(
      new Error("m", { cause: "c" }),
    );
    expect(new Error("m", { cause: "c" })).not.toStrictEqual(
      new Error("m", { cause: "d" }),
    );
  });

  test("keeps a present-but-undefined cause distinct", () => {
    expect(new Error("m", { cause: undefined })).not.toStrictEqual(
      new Error("m"),
    );
    expect(new Error("m")).not.toStrictEqual(
      new Error("m", { cause: undefined }),
    );
  });

  test("supports negation", () => {
    expect({ a: 1 }).not.toStrictEqual({ a: 2 });
    expect([1]).not.toStrictEqual([1, 2]);
  });
});
