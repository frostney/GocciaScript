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

  test("keys the type check on the resolved constructor", () => {
    // Vitest is the oracle here: an object whose prototype is an ordinary
    // object still resolves to Object, so it stays strictly equal to a
    // literal, while a null prototype or a built-in one does not.
    const shared = { marker: true };
    const left = Object.create(shared);
    left.x = 1;
    const right = Object.create(shared);
    right.x = 1;
    expect(left).toStrictEqual(right);
    expect(left).toStrictEqual({ x: 1 });

    const nullPrototype = Object.create(null);
    nullPrototype.x = 1;
    expect(nullPrototype).not.toStrictEqual({ x: 1 });
    expect(nullPrototype).toStrictEqual(Object.create(null, {
      x: { value: 1, enumerable: true },
    }));

    const arrayPrototype = Object.create(Array.prototype);
    expect(arrayPrototype).not.toStrictEqual({});

    const pointPrototype = Object.create(Point.prototype);
    pointPrototype.x = 1;
    expect(pointPrototype).not.toStrictEqual({ x: 1 });
    // Both resolve to Point, so they are strictly equal.
    expect(new Point(1)).toStrictEqual(pointPrototype);
  });

  test("keeps same-named anonymous classes distinct", () => {
    // Protected divergence: goccia and vitest agree, bun does not.
    const First = class Same {};
    const Second = class Same {};
    expect(new First()).not.toStrictEqual(new Second());
    expect(new First()).toEqual(new Second());
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

  test("applies error identity and the constructor check", () => {
    expect(new Error("a")).toStrictEqual(new Error("a"));
    expect(new Error("a")).not.toStrictEqual(new Error("b"));
    expect(new TypeError("m")).not.toStrictEqual(new Error("m"));
    expect(new Error("a")).not.toStrictEqual({});

    // Vitest is the oracle: unlike toEqual, the strict matcher separates a
    // subclass instance from a base Error even when name and message match.
    expect(new CustomError("m")).not.toStrictEqual(new Error("m"));
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

  test("reads cause from the expected side only", () => {
    // An expected error without a cause does not constrain the actual one.
    expect(new Error("m", { cause: undefined })).toStrictEqual(new Error("m"));
    expect(new Error("m", { cause: "c" })).toStrictEqual(new Error("m"));
    expect(new Error("m")).toStrictEqual(new Error("m", { cause: undefined }));
    expect(new Error("m")).not.toStrictEqual(new Error("m", { cause: "c" }));
  });

  test("supports negation", () => {
    expect({ a: 1 }).not.toStrictEqual({ a: 2 });
    expect([1]).not.toStrictEqual([1, 2]);
  });
});

// Same exposure as toEqual: see that file for the rationale.
describe.runIf(typeof Goccia !== "undefined")("toStrictEqual under explicit GC", () => {
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

  test("compares through getters that collect", () => {
    expect({ k: { x: 1, arr: [1, 2, 3] } }).toStrictEqual(freshExpected());
  });
});
