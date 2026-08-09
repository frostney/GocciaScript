// Battery D — matcher semantics vs Vitest/Jest. Bun's expect is the ground truth here.

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
