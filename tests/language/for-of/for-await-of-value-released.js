/*---
description: >
  A for-await-of body that drops its binding lets a collection reclaim the
  value before the next iteration: neither the loop's own value register nor
  an async generator suspended at the yield that produced the value keeps it
  reachable (ES2026 §14.7.5.7 ForIn/OfBodyEvaluation binds the value, and the
  binding is the only reference the loop holds).
features: [for-of, async-await, async-generators, Goccia.gc]
---*/

const SIZE = 32 * 1024 * 1024;

const collect = () => {
  Goccia.gc();
  return Goccia.gc.bytesAllocated;
};

describe("a for-await-of value the body has dropped", () => {
  test("is reclaimed when an async iterator produced it", async () => {
    const iterable = {
      [Symbol.asyncIterator]() {
        let step = 0;
        return {
          next() {
            step = step + 1;
            return Promise.resolve(step === 1
              ? { value: new ArrayBuffer(SIZE), done: false }
              : { value: undefined, done: true });
          },
        };
      },
    };
    const run = async () => {
      const baseline = collect();
      const grown = [];
      for await (let buffer of iterable) {
        buffer = null;
        Goccia.gc();
        grown.push(Goccia.gc.bytesAllocated - baseline);
      }
      return grown;
    };
    const grown = await run();
    expect(grown.length).toBe(1);
    expect(grown[0]).toBeLessThan(SIZE / 2);
  });

  test("is reclaimed when an async generator yielded it", async () => {
    const source = {
      async *[Symbol.asyncIterator]() {
        yield new ArrayBuffer(SIZE);
        yield new ArrayBuffer(SIZE);
      },
    };
    const run = async () => {
      const baseline = collect();
      const grown = [];
      for await (let buffer of source) {
        buffer = null;
        Goccia.gc();
        grown.push(Goccia.gc.bytesAllocated - baseline);
      }
      return grown;
    };
    const grown = await run();
    expect(grown.length).toBe(2);
    expect(grown[0]).toBeLessThan(SIZE / 2);
    expect(grown[1]).toBeLessThan(SIZE / 2);
  });

  test("is still the bound value while the body holds it", async () => {
    const source = {
      async *[Symbol.asyncIterator]() {
        yield new Uint8Array(1024).fill(1);
        yield new Uint8Array(1024).fill(2);
      },
    };
    const run = async () => {
      const seen = [];
      for await (const bytes of source) {
        Goccia.gc();
        seen.push(bytes[0], bytes.length);
      }
      return seen;
    };
    expect(await run()).toEqual([1, 1024, 2, 1024]);
  });
});
