/*---
description: The testing prelude's expect, its matchers and asymmetric matchers, mocks and test.each look up nothing a script can replace on the values they compare
features: [Goccia]
---*/

import { expectClean, installTripwire } from "./helpers/tripwire.js";

const wire = installTripwire();
afterAll(() => wire.uninstall());

// the matcher methods are the test framework's own objects, not intrinsics: calling them is not a lookup the
// tripwire records, but whatever they call on the realm's intrinsics is
const watched = (f) => expectClean(wire.watch(f));
// what the matchers read from the values they compare, as data: a strict comparison's constructors, an error's name
// and message, a RegExp's source, flags and test (a pattern compared, or a user's pattern a matcher runs)
// a constructor's name and a value's Symbol.toStringTag, which messages and snapshots print
const VALUE_READS = /\.prototype\.(constructor|name|message)$|^RegExp\.prototype\.|\.name$|\[Symbol\.toStringTag\]$/;
const valueChecked = (f) => {
  const r = wire.watch(f);
  if (r.threw) throw r.error;
  return r.hits.filter((h) => !VALUE_READS.test(h));
};
const TypeErrorCtor = TypeError, NumberCtor = Number, RangeErrorCtor = RangeError, ErrorCtor = Error;

test("the equality and value matchers", () => {
  const values = {
    map: new Map([[1, { a: [1, 2] }]]), set: new Set([1, "x"]), date: new Date(0), re: /a/g, err: new TypeError("t"),
    nested: { a: [1, { b: 2 }], s: Symbol.for("k") }, typed: new Uint8Array([1, 2]), str: "hello world",
  };
  const twin = {
    map: new Map([[1, { a: [1, 2] }]]), set: new Set(["x", 1]), date: new Date(0), re: /a/g, err: new TypeError("t"),
    nested: { a: [1, { b: 2 }], s: Symbol.for("k") }, typed: new Uint8Array([1, 2]), str: "hello world",
  };
  expect(valueChecked(() => {
    expect(values).toEqual(twin);
    expect(values).toStrictEqual(twin);
    expect(values).toMatchObject({ nested: { a: [1, { b: 2 }] } });
    expect(values.str).toMatch("world");
    expect(values.str).toContain("lo w");
    expect([1, 2, 3]).toContain(2);
    expect([{ a: 1 }]).toContainEqual({ a: 1 });
    expect(values.nested).toHaveProperty("a.1.b", 2);
    expect(values.nested).toHaveProperty(["a", 0]);
    expect([1, 2]).toHaveLength(2);
    expect(0.1 + 0.2).toBeCloseTo(0.3);
    expect(1).toBeOneOf([1, 2]);
    expect(2).toSatisfy((x) => x > 1);
    expect(values.err).toBeInstanceOf(TypeErrorCtor);
    expect(NaN).toBeNaN();
    expect(1).toBeGreaterThan(0);
    expect(1).toBeLessThanOrEqual(1);
    expect(null).toBeNull();
    expect(undefined).toBeUndefined();
    expect(1).toBeDefined();
    expect(0).toBeFalsy();
    expect(1).toBeTruthy();
    expect({ a: 1, b: "x" }).toEqual({ a: expect.any(NumberCtor), b: expect.stringContaining("x") });
    expect([1, 2, 3]).toEqual(expect.arrayContaining([3, 1]));
    expect({ a: 1, b: 2 }).toEqual(expect.objectContaining({ a: 1 }));
    expect("abc").toEqual(expect.stringMatching(/b/));
    expect({ a: 1 }).not.toEqual({ a: 2 });
  })).toEqual([]);
});

test("toThrow, snapshots, mocks and expect.extend", () => {
  const mockFn = typeof vi === "object" ? vi.fn : mock;
  const snapshotValue = { a: [1, new Map([["k", new Set([1])]])] };
  expect(valueChecked(() => {
    expect(() => { throw new RangeErrorCtor("bad thing"); }).toThrow(RangeErrorCtor);
    expect(() => { throw new ErrorCtor("bad thing"); }).toThrow("thing");
    expect(snapshotValue).toMatchInlineSnapshot(`
      {
        "a": [
          1,
          Map {
            "k" => Set {
              1,
            },
          },
        ],
      }
    `);
    const fn = mockFn((x) => x * 2);
    fn.mockImplementationOnce((x) => x);
    fn(1);
    fn(2);
    expect(fn).toHaveBeenCalled();
    expect(fn).toHaveBeenCalledTimes(2);
    expect(fn).toHaveBeenCalledWith(2);
    expect(fn).toHaveBeenNthCalledWith(1, 1);
    expect(fn).toHaveBeenLastCalledWith(2);
    expect(fn).toHaveReturnedWith(4);
    expect(fn).toHaveReturnedTimes(2);
    expect(fn).toHaveLastReturnedWith(4);
    expect(fn).toHaveNthReturnedWith(1, 1);
    expect.extend({ toBeTwo(actual) { return { pass: actual === 2, message: () => "not two" }; } });
    expect(2).toBeTwo();
  })).toEqual([]);
});

test("expect.stringMatching with a string runs its own pattern", () => {
  // the matcher's pattern is its own RegExp: matching and comparing it look up nothing on RegExp.prototype
  watched(() => expect("abc").toEqual(expect.stringMatching("b")));
  watched(() => expect(expect.stringMatching("b")).toEqual(expect.stringMatching("b")));
});

test("expect.stringMatching reads a pattern's source before its flags and compares canonical flags", () => {
  const reads = [];
  const sample = /x/g;
  Object.defineProperty(sample, "source", { get() { reads[reads.length] = "source"; return "x"; } });
  Object.defineProperty(sample, "flags", { get() { reads[reads.length] = "flags"; return "ig"; } });
  const m = expect.stringMatching(sample);
  expect(reads).toEqual(["source", "flags"]);
  expect("X").toEqual(m);
  expect(m).toEqual(expect.stringMatching(/x/gi));
});

test.each([[1, 2, 3], [2, 3, 5]])("test.each %i + %i is %i", (a, b, sum) => {
  expect(watched(() => a + b)).toBe(sum);
});
