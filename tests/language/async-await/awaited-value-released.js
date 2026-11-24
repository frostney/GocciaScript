/*---
description: >
  An async function that resumes with an awaited value and then drops its
  last reference to it lets a collection reclaim it before the function next
  suspends (ES2026 §27.7.5.3 Await). The job that resumed the function and the
  suspended frame's resume slot no longer keep the value reachable once the
  frame's register holds it.
features: [async-await, Goccia.gc]
---*/

const SIZE = 32 * 1024 * 1024;

const collect = () => {
  Goccia.gc();
  return Goccia.gc.bytesAllocated;
};

describe("an awaited value the function has dropped", () => {
  test("is reclaimed by a collection in the same step", async () => {
    const make = async () => new ArrayBuffer(SIZE);
    const run = async () => {
      const baseline = collect();
      let buffer = await make();
      const length = buffer.byteLength;
      buffer = null;
      Goccia.gc();
      return [length, Goccia.gc.bytesAllocated - baseline];
    };
    const [length, grown] = await run();
    expect(length).toBe(SIZE);
    expect(grown).toBeLessThan(SIZE / 2);
  });

  test("is reclaimed when it comes from an async method", async () => {
    const source = {
      async make() {
        return new ArrayBuffer(SIZE);
      },
      async run() {
        const baseline = collect();
        let buffer = await this.make();
        buffer = null;
        Goccia.gc();
        return Goccia.gc.bytesAllocated - baseline;
      },
    };
    expect(await source.run()).toBeLessThan(SIZE / 2);
  });

  test("is reclaimed when it comes from a thenable", async () => {
    const thenable = {
      then(resolve) {
        resolve(new ArrayBuffer(SIZE));
      },
    };
    const run = async () => {
      const baseline = collect();
      let buffer = await thenable;
      buffer = null;
      Goccia.gc();
      return Goccia.gc.bytesAllocated - baseline;
    };
    expect(await run()).toBeLessThan(SIZE / 2);
  });

  test("is still the value the function sees while it holds it", async () => {
    const make = async (fill) => new Uint8Array(1024).fill(fill);
    const run = async () => {
      const first = await make(1);
      Goccia.gc();
      const second = await make(2);
      Goccia.gc();
      return [first[0], first.length, second[1023]];
    };
    expect(await run()).toEqual([1, 1024, 2]);
  });
});
