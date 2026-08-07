// Battery D — matcher semantics. Vitest is the ground truth here: goccia's
// expect targets it as an exact drop-in, and a 223-probe audit found bun
// disagreeing with vitest on 30 of 178 matcher probes, so bun is advisory for
// this battery and never decides what an assertion below should say.

describe("deep equality semantics", () => {
  test("toEqual ignores explicit-undefined properties", () => {
    expect({ a: 1, b: undefined }).toEqual({ a: 1 });
  });

  test("toStrictEqual distinguishes explicit-undefined properties", () => {
    expect({ a: 1, b: undefined }).not.toStrictEqual({ a: 1 });
  });

  test("toEqual on Map and Set contents", () => {
    expect(new Map([["k", [1, 2]]])).toEqual(new Map([["k", [1, 2]]]));
    expect(new Set([1, 2])).toEqual(new Set([2, 1]));
  });

  test("toStrictEqual reads an accessor-backed constructor", () => {
    // The strict type check compares `actual.constructor` against
    // `expected.constructor` as ordinary property reads, so a prototype
    // accessor runs inside the matcher. Pinned here because the alternative
    // (a type identity resolved without reading the property) is a
    // plausible-looking change that would silently diverge from vitest.
    class Fresh {
      constructor(x) {
        this.x = x;
      }
    }
    Object.defineProperty(Fresh.prototype, "constructor", {
      get() {
        return { fresh: true };
      },
      configurable: true,
    });
    expect(new Fresh(1)).not.toStrictEqual(new Fresh(1));
    expect(new Fresh(1)).toEqual({ x: 1 });

    class Stable {
      constructor(x) {
        this.x = x;
      }
    }
    Object.defineProperty(Stable.prototype, "constructor", {
      get() {
        return Stable;
      },
      configurable: true,
    });
    expect(new Stable(1)).toStrictEqual(new Stable(1));
    expect(new Stable(1)).not.toStrictEqual({ x: 1 });
  });

  test("toEqual distinguishes Map from plain object", () => {
    expect(new Map([["a", 1]])).not.toEqual({ a: 1 });
  });

  test("toEqual distinguishes -0 from 0", () => {
    expect([-0]).not.toEqual([0]);
  });

  test("toEqual on NaN", () => {
    expect({ v: NaN }).toEqual({ v: NaN });
  });

  test("toEqual across class instances vs literals", () => {
    class P {
      constructor(x) {
        this.x = x;
      }
    }
    expect(new P(1)).toEqual({ x: 1 });
    expect(new P(1)).not.toStrictEqual({ x: 1 });
  });

  test("sparse vs dense arrays under toStrictEqual", () => {
    // eslint-disable-next-line no-sparse-arrays
    expect([1, , 3]).not.toStrictEqual([1, undefined, 3]);
  });
});

describe("error equality", () => {
  test("errors compare on name and message", () => {
    expect(new Error("a")).toEqual(new Error("a"));
    expect(new Error("a")).not.toEqual(new Error("b"));
    expect(new TypeError("m")).not.toEqual(new Error("m"));
  });

  test("errors compare on own enumerable properties", () => {
    const withCode = new Error("m");
    withCode.code = 1;
    const sameCode = new Error("m");
    sameCode.code = 1;
    const otherCode = new Error("m");
    otherCode.code = 2;

    expect(withCode).toEqual(sameCode);
    expect(withCode).not.toEqual(otherCode);
    expect(withCode).not.toEqual(new Error("m"));
  });

  test("an error never equals a plain object", () => {
    expect(new Error("a")).not.toEqual({});
    expect(new Error("a")).not.toEqual({ message: "a" });
    expect(new Error("a")).not.toEqual({ name: "Error", message: "a" });
    expect({}).not.toEqual(new Error("a"));
  });

  test("error subclasses key on name, not on the class", () => {
    class CustomError extends Error {}
    class NamedError extends Error {
      constructor(message) {
        super(message);
        this.name = "NamedError";
      }
    }

    // Under .toEqual() an error is keyed on `name`, so a subclass that leaves
    // `name` alone is indistinguishable from a plain Error. .toStrictEqual()
    // makes no exception for errors: its constructor check applies to them too.
    expect(new CustomError("m")).toEqual(new Error("m"));
    expect(new CustomError("m")).not.toStrictEqual(new Error("m"));
    expect(new NamedError("m")).not.toEqual(new Error("m"));
    expect(new TypeError("m")).not.toStrictEqual(new Error("m"));
  });

  test("cause participates in equality, asymmetrically", () => {
    expect(new Error("m", { cause: "c" })).toEqual(new Error("m", { cause: "c" }));
    expect(new Error("m", { cause: "c" })).not.toEqual(new Error("m", { cause: "d" }));
    // `cause` is only compared when the *expected* error has one, so an
    // actual-only cause is ignored and the same pair reversed is not equal.
    expect(new Error("m", { cause: "c" })).toEqual(new Error("m"));
    expect(new Error("m")).not.toEqual(new Error("m", { cause: "c" }));
    expect(new Error("m", { cause: { a: 1 } })).toEqual(
      new Error("m", { cause: { a: 1 } }),
    );
  });

  test("a cyclic cause chain terminates", () => {
    const left = new Error("m");
    left.cause = left;
    const right = new Error("m");
    right.cause = right;

    expect(left).toEqual(right);
  });

  test("errors nested in containers", () => {
    expect([new Error("x")]).toEqual([new Error("x")]);
    expect([new Error("x")]).not.toEqual([new Error("y")]);
    expect(new Set([new Error("x")])).toEqual(new Set([new Error("x")]));
    expect(new Map([["k", new Error("x")]])).toEqual(
      new Map([["k", new Error("x")]]),
    );
    expect([new Error("x")]).toContainEqual(new Error("x"));
  });

  test("error-ness comes from the error slot, not the prototype chain", () => {
    expect(Object.create(Error.prototype)).toEqual({});
    expect(Object.setPrototypeOf({}, Error.prototype)).toEqual({});
    expect(Object.create(Error.prototype)).not.toEqual(new Error(""));
  });

  test("a present-but-undefined cause reads as absent to both matchers", () => {
    expect(new Error("m", { cause: undefined })).toEqual(new Error("m"));
    expect(new Error("m")).toEqual(new Error("m", { cause: undefined }));
    // An expected cause of `undefined` is not a defined cause, so the
    // asymmetric check skips it under .toStrictEqual() as well.
    expect(new Error("m", { cause: undefined })).toStrictEqual(new Error("m"));
  });

  test("name is compared by value, never stringified", () => {
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

  test("an AggregateError nested inside a cause", () => {
    // `errors` participates whenever both sides are AggregateErrors, and a
    // cause is compared with the same rule as a top-level error.
    expect(
      new Error("m", { cause: new AggregateError([new Error("x")], "agg") }),
    ).not.toEqual(
      new Error("m", { cause: new AggregateError([new Error("y")], "agg") }),
    );
    expect(
      new Error("m", { cause: new AggregateError([new Error("x")], "agg") }),
    ).toEqual(
      new Error("m", { cause: new AggregateError([new Error("x")], "agg") }),
    );
  });

  test("toMatchObject subsets the container, not the errors inside it", () => {
    // An empty expected object constrains nothing, but two errors are compared
    // by the error rule rather than by subset.
    expect({ e: new Error("x") }).not.toMatchObject({ e: new Error("y") });
    expect({ e: new Error("x") }).toMatchObject({ e: {} });
    expect({ e: new Error("x") }).toMatchObject({ e: new Error("x") });
  });
});

describe("asymmetric matchers", () => {
  test("expect.any / expect.anything", () => {
    expect({ id: "x", n: 3 }).toEqual({ id: expect.any(String), n: expect.any(Number) });
    expect({ v: 0 }).toEqual({ v: expect.anything() });
  });

  test("objectContaining / arrayContaining / stringMatching", () => {
    expect({ a: 1, b: 2 }).toEqual(expect.objectContaining({ a: 1 }));
    expect([1, 2, 3]).toEqual(expect.arrayContaining([3, 1]));
    expect("hello world").toEqual(expect.stringMatching(/world$/));
  });

  test("stringContaining and nested asymmetric", () => {
    expect({ msg: "abc-def", list: [5, 6] }).toEqual({
      msg: expect.stringContaining("def"),
      list: expect.arrayContaining([6]),
    });
  });
});

describe("toThrow forms", () => {
  test("regex matches Error message", () => {
    expect(() => {
      throw new Error("expected an items array in the response");
    }).toThrow(/items array/);
  });

  test("regex matches Error subclass message", () => {
    class DriftError extends Error {}
    expect(() => {
      throw new DriftError("drift detected");
    }).toThrow(/drift/);
  });

  test("Error-instance argument matches by message", () => {
    expect(() => {
      throw new Error("exact text");
    }).toThrow(new Error("exact text"));
  });

  test("constructor + not-throwing negation", () => {
    expect(() => {
      throw new TypeError("t");
    }).toThrow(TypeError);
    expect(() => 42).not.toThrow();
  });
});

describe("property and containment matchers", () => {
  test("toHaveProperty with dotted path and array index", () => {
    const o = { a: { b: [{ c: 7 }] } };
    expect(o).toHaveProperty("a.b");
    expect(o).toHaveProperty(["a", "b", 0, "c"], 7);
  });

  test("toContainEqual with objects", () => {
    expect([{ x: 1 }, { x: 2 }]).toContainEqual({ x: 2 });
  });

  test("toMatchObject with nested arrays", () => {
    expect({ list: [{ a: 1, b: 2 }, { a: 3 }] }).toMatchObject({ list: [{ a: 1 }, { a: 3 }] });
  });
});

describe("promise matchers", () => {
  test("resolves chains through matchers", async () => {
    await expect(Promise.resolve({ ok: true })).resolves.toEqual({ ok: true });
  });

  test("rejects.toThrow with regex", async () => {
    const failing = async () => {
      throw new Error("async boom");
    };
    await expect(failing()).rejects.toThrow(/boom/);
  });
});
