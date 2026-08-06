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

class NamedError extends Error {
  constructor(message) {
    super(message);
    this.name = "NamedError";
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

  test("requires arrays to match in length", () => {
    // Vitest compares arrays length-first: a trailing undefined is a real
    // element, not padding.
    expect([1, undefined]).not.toEqual([1]);
    expect([2]).not.toEqual([2, undefined]);
    expect([]).not.toEqual([undefined]);
    expect([1, 2]).not.toEqual([1, 2, undefined, undefined]);
    expect([[1, undefined]]).not.toEqual([[1]]);
    expect([1, undefined]).toEqual([1, undefined]);
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

  test("scans membership existentially rather than pairing off", () => {
    // Vitest is the oracle: sizes must match and every ACTUAL member must
    // deep-equal SOME expected member, but an expected member may match
    // several actual members or none at all.
    expect(new Set([1, 2])).toEqual(new Set([expect.any(Number), 3]));
    expect(new Set([1, 2])).toEqual(
      new Set([expect.any(Number), expect.any(String)]),
    );
    expect(new Set([{ a: 1 }, { a: 1 }])).toEqual(
      new Set([{ a: 1 }, { b: 2 }]),
    );

    // An actual member matching nothing still fails, in either direction.
    expect(new Set([{ a: 1 }, { b: 2 }])).not.toEqual(
      new Set([{ a: 1 }, { a: 1 }]),
    );
    expect(new Set([1, "x"])).not.toEqual(new Set([expect.any(Number), 1]));
    expect(new Map([[1, "x"], [2, "y"]])).not.toEqual(
      new Map([[expect.any(Number), "x"], [3, "x"]]),
    );
  });

  test("counts a trailing hole as an element", () => {
    // [1, ,] has length 2, so it does not equal [1].
    expect([1, ,]).not.toEqual([1]);
    expect([1, ,]).toEqual([1, ,]);
    expect([1, ,]).toEqual([1, undefined]);
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

  test("compares errors on name and message", () => {
    expect(new Error("a")).toEqual(new Error("a"));
    expect(new Error("a")).not.toEqual(new Error("b"));
    expect(new TypeError("m")).toEqual(new TypeError("m"));
    expect(new TypeError("m")).not.toEqual(new Error("m"));
  });

  test("compares an error's own enumerable properties", () => {
    const withCode = new Error("m");
    withCode.code = 1;
    const sameCode = new Error("m");
    sameCode.code = 1;
    const otherCode = new Error("m");
    otherCode.code = 2;

    expect(withCode).toEqual(sameCode);
    expect(withCode).not.toEqual(otherCode);
    expect(withCode).not.toEqual(new Error("m"));

    // The undefined-key rule still applies inside errors.
    const undefinedCode = new Error("m");
    undefinedCode.code = undefined;
    expect(undefinedCode).toEqual(new Error("m"));
  });

  test("never equates an error with a plain object", () => {
    expect(new Error("a")).not.toEqual({});
    expect(new Error("a")).not.toEqual({ message: "a" });
    expect(new Error("a")).not.toEqual({ name: "Error", message: "a" });
    expect({}).not.toEqual(new Error("a"));
  });

  test("keys error subclasses on name rather than the class", () => {
    // A subclass that leaves name alone inherits "Error" and matches one.
    expect(new CustomError("m")).toEqual(new Error("m"));
    expect(new CustomError("m")).toEqual(new CustomError("m"));
    expect(new NamedError("m")).not.toEqual(new Error("m"));

    const renamed = new Error("m");
    renamed.name = "Weird";
    const sameName = new Error("m");
    sameName.name = "Weird";
    expect(renamed).toEqual(sameName);
    expect(renamed).not.toEqual(new Error("m"));
  });

  test("compares the cause, driven by the expected side", () => {
    expect(new Error("m", { cause: "c" })).toEqual(
      new Error("m", { cause: "c" }),
    );
    expect(new Error("m", { cause: "c" })).not.toEqual(
      new Error("m", { cause: "d" }),
    );
    expect(new Error("m")).not.toEqual(new Error("m", { cause: "c" }));
    expect(new Error("m", { cause: { a: 1 } })).toEqual(
      new Error("m", { cause: { a: 1 } }),
    );
    expect(new Error("m", { cause: { a: 1 } })).not.toEqual(
      new Error("m", { cause: { a: 2 } }),
    );

    // A cause the expectation does not mention is ignored.
    expect(new Error("m", { cause: "c" })).toEqual(new Error("m"));
  });

  test("terminates on a cyclic cause chain", () => {
    const left = new Error("m");
    left.cause = left;
    const right = new Error("m");
    right.cause = right;
    expect(left).toEqual(right);

    const outerLeft = new Error("outer");
    const innerLeft = new Error("inner");
    outerLeft.cause = innerLeft;
    innerLeft.cause = outerLeft;
    const outerRight = new Error("outer");
    const innerRight = new Error("inner");
    outerRight.cause = innerRight;
    innerRight.cause = outerRight;
    expect(outerLeft).toEqual(outerRight);
  });

  test("compares AggregateError contents", () => {
    expect(new AggregateError([new Error("x")], "agg")).toEqual(
      new AggregateError([new Error("x")], "agg"),
    );
    expect(new AggregateError([new Error("x")], "agg")).not.toEqual(
      new AggregateError([new Error("y")], "agg"),
    );
    expect(
      new AggregateError([new Error("x"), new Error("y")], "agg"),
    ).not.toEqual(new AggregateError([new Error("x")], "agg"));
    expect(new AggregateError([new Error("x")], "agg")).not.toEqual(
      new AggregateError([new Error("x")], "other"),
    );
  });

  test("ignores stack entirely", () => {
    const left = new Error("m");
    left.stack = "one";
    const right = new Error("m");
    right.stack = "two";
    expect(left).toEqual(right);

    const restacked = new Error("m");
    restacked.stack = "different";
    expect(new Error("m")).toEqual(restacked);
  });

  test("keys error-ness on the error slot, not the prototype chain", () => {
    // Inheriting from Error.prototype does not make an object an error; only
    // the slot an error constructor installs does.
    expect(Object.create(Error.prototype)).toEqual({});
    expect(Object.setPrototypeOf({}, Error.prototype)).toEqual({});
    expect(Object.create(Error.prototype)).not.toEqual(new Error(""));
    expect(new CustomError("m")).not.toEqual({});
  });

  test("forgives a cause that is present but undefined", () => {
    expect(new Error("m", { cause: undefined })).toEqual(new Error("m"));
    expect(new Error("m")).toEqual(new Error("m", { cause: undefined }));
    expect(new Error("m", { cause: undefined })).toStrictEqual(new Error("m"));
  });

  test("compares name by value rather than by its string form", () => {
    const undefinedName = new Error("m");
    Object.setPrototypeOf(
      undefinedName,
      Object.create(Error.prototype, { name: { value: undefined } }),
    );
    const emptyName = new Error("m");
    Object.setPrototypeOf(
      emptyName,
      Object.create(Error.prototype, { name: { value: "" } }),
    );

    expect(undefinedName).not.toEqual(emptyName);
  });

  test("never stringifies name, so a throwing toString stays put", () => {
    let calls = 0;
    const shared = Object.create(Error.prototype, {
      name: {
        value: {
          toString: () => {
            calls = calls + 1;
            throw new RangeError("boom");
          },
        },
      },
    });
    const left = new Error("m");
    Object.setPrototypeOf(left, shared);
    const right = new Error("m");
    Object.setPrototypeOf(right, shared);

    expect(left).toEqual(right);
    expect(calls).toBe(0);
  });

  test("compares an AggregateError nested inside a cause", () => {
    expect(
      new Error("m", { cause: new AggregateError([new Error("x")], "agg") }),
    ).toEqual(
      new Error("m", { cause: new AggregateError([new Error("x")], "agg") }),
    );
    expect(
      new Error("m", { cause: new AggregateError([new Error("x")], "agg") }),
    ).not.toEqual(
      new Error("m", { cause: new AggregateError([new Error("y")], "agg") }),
    );
  });

  test("distinguishes DOMExceptions by their enumerable fields", () => {
    // Deliberate divergence from the Vitest oracle, decided by the project:
    // a DOMException carries name/message/code as ordinary enumerable
    // properties here, so the object walk separates them. Vitest equates all
    // DOMExceptions regardless of name or message; goccia keeps them apart.
    expect(new DOMException("a", "AbortError")).toEqual(
      new DOMException("a", "AbortError"),
    );
    expect(new DOMException("a", "AbortError")).not.toEqual(
      new DOMException("b", "AbortError"),
    );
  });

  test("compares errors nested in containers", () => {
    expect([new Error("x")]).toEqual([new Error("x")]);
    expect([new Error("x")]).not.toEqual([new Error("y")]);
    expect(new Set([new Error("x")])).toEqual(new Set([new Error("x")]));
    expect(new Set([new Error("x")])).not.toEqual(new Set([new Error("y")]));
    expect(new Map([["k", new Error("x")]])).toEqual(
      new Map([["k", new Error("x")]]),
    );
    expect(new Map([["k", new Error("x")]])).not.toEqual(
      new Map([["k", new Error("y")]]),
    );
    expect({ e: new Error("x") }).toEqual({ e: new Error("x") });
    expect({ e: new Error("x") }).not.toEqual({ e: new Error("y") });
    expect([new Error("x")]).toContainEqual(new Error("x"));
    expect([new Error("x")]).not.toContainEqual(new Error("y"));
  });

  test("matches objectContaining against Map keys", () => {
    // Protected parity: goccia and vitest agree here, bun does not.
    expect(
      new Map([
        [{ a: 1 }, "v"],
        [{ a: 1, b: 2 }, "w"],
      ]),
    ).toEqual(
      new Map([
        [expect.objectContaining({ a: 1 }), "v"],
        [expect.objectContaining({ a: 1 }), "w"],
      ]),
    );
  });

  test("lets a throwing name getter propagate", () => {
    // Protected parity with vitest: reading name runs the getter, so its
    // error escapes the matcher rather than being swallowed. bun differs.
    const thrower = new Error("m");
    Object.defineProperty(thrower, "name", {
      get: () => {
        throw new Error("boom-getter");
      },
      configurable: true,
    });

    expect(() => expect(thrower).toEqual(new Error("m"))).toThrow(
      "boom-getter",
    );
  });

  test("finds Set members with toContainEqual", () => {
    expect(new Set([{ a: 1 }])).toContainEqual({ a: 1 });
    expect(new Set([1, 2])).toContainEqual(1);
    expect(new Set([{ a: 1 }])).not.toContainEqual({ a: 2 });
  });

  test("supports negation", () => {
    expect({ a: 1 }).not.toEqual({ a: 2 });
    expect([1]).not.toEqual([1, 2]);
  });
});
