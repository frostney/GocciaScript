/*---
description: >
  A catch clause or a for await loop with a pattern copies the value into the
  pattern's bindings once the pattern matches. Only a mismatch reads the
  register the value arrived in again (to rethrow it, or to move on), so a
  match clears it: a value the clause or the body then drops is reclaimable by
  a collection there.
features: [pattern-matching, try-catch, for-await-of, Goccia.gc]
---*/

const SIZE = 32 * 1024 * 1024;

const collect = () => {
  Goccia.gc();
  return Goccia.gc.bytesAllocated;
};

describe("a value a matched pattern took and the program dropped", () => {
  test("is reclaimed in a catch clause whose pattern matched", () => {
    const run = () => {
      const baseline = collect();
      let code = 0;
      try {
        throw { code: 5, payload: new ArrayBuffer(SIZE) };
      } catch (error is { code: const matched }) {
        code = matched;
        error = null;
        Goccia.gc();
        return [code, Goccia.gc.bytesAllocated - baseline];
      }
    };
    const [code, grown] = run();
    expect(code).toBe(5);
    expect(grown).toBeLessThan(SIZE / 2);
  });

  test("is reclaimed in a catch clause whose array pattern matched", () => {
    const run = () => {
      const baseline = collect();
      try {
        throw [new ArrayBuffer(SIZE)];
      } catch (error is [let first]) {
        first = null;
        error = null;
        Goccia.gc();
        return Goccia.gc.bytesAllocated - baseline;
      }
    };
    expect(run()).toBeLessThan(SIZE / 2);
  });

  test("is reclaimed in a catch clause whose nested pattern matched", () => {
    const run = () => {
      const baseline = collect();
      try {
        throw { items: [new ArrayBuffer(SIZE)] };
      } catch (error is { items: [let first] }) {
        first = null;
        error = null;
        Goccia.gc();
        return Goccia.gc.bytesAllocated - baseline;
      }
    };
    expect(run()).toBeLessThan(SIZE / 2);
  });

  test("is reclaimed in a for await body whose array pattern matched", async () => {
    const source = {
      async *[Symbol.asyncIterator]() {
        yield [new ArrayBuffer(SIZE)];
      },
    };
    const run = async () => {
      const baseline = collect();
      const grown = [];
      for await (let item is [let first] of source) {
        first = null;
        item = null;
        Goccia.gc();
        grown.push(Goccia.gc.bytesAllocated - baseline);
      }
      return grown;
    };
    const grown = await run();
    expect(grown.length).toBe(1);
    expect(grown[0]).toBeLessThan(SIZE / 2);
  });

  test("still rethrows the exception a catch pattern did not match", () => {
    let caught = null;
    try {
      try {
        throw { other: 1 };
      } catch (error is { code: const code }) {
        caught = "inner";
      }
    } catch (outer) {
      caught = outer;
    }
    expect(caught).toEqual({ other: 1 });
  });

  test("is reclaimed in a for await body whose pattern matched", async () => {
    const source = {
      async *[Symbol.asyncIterator]() {
        yield { code: 1, payload: new ArrayBuffer(SIZE) };
      },
    };
    const run = async () => {
      const baseline = collect();
      const grown = [];
      for await (let item is { code: const code } of source) {
        item = null;
        Goccia.gc();
        grown.push([code, Goccia.gc.bytesAllocated - baseline]);
      }
      return grown;
    };
    const grown = await run();
    expect(grown.length).toBe(1);
    expect(grown[0][0]).toBe(1);
    expect(grown[0][1]).toBeLessThan(SIZE / 2);
  });

  test("skips a for await value its pattern did not match", async () => {
    const values = [Promise.resolve({ code: 1 }), Promise.resolve({ other: 2 })];
    const seen = [];
    for await (const item is { code: const code } of values) {
      seen.push(code);
    }
    expect(seen).toEqual([1]);
  });
});
