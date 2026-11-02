/*---
description: >
  An async generator suspended at a yield does not keep the value it yielded
  reachable: once the caller of next() drops the iterator result, a
  collection reclaims the value while the generator stays suspended (ES2026
  §27.6.3.8 AsyncGeneratorYield hands the value out through the request's
  promise). The value is still intact while it is held, and the generator
  resumes normally, including through throw() and return() at that yield.
features: [async-generators, Goccia.gc]
---*/

const SIZE = 32 * 1024 * 1024;

const collect = () => {
  Goccia.gc();
  return Goccia.gc.bytesAllocated;
};

describe("a value a suspended async generator has yielded", () => {
  test("is reclaimed once the caller drops the iterator result", async () => {
    const source = {
      async *produce() {
        yield new ArrayBuffer(SIZE);
        yield 2;
      },
    };
    const iterator = source.produce();
    const run = async () => {
      const baseline = collect();
      let result = await iterator.next();
      const length = result.value.byteLength;
      result = null;
      Goccia.gc();
      return [length, Goccia.gc.bytesAllocated - baseline];
    };
    const [length, grown] = await run();
    expect(length).toBe(SIZE);
    expect(grown).toBeLessThan(SIZE / 2);
    expect(await iterator.next()).toEqual({ value: 2, done: false });
    expect((await iterator.next()).done).toBe(true);
  });

  test("is reclaimed when the yield awaited a promise for it", async () => {
    const make = async () => new ArrayBuffer(SIZE);
    const source = {
      async *produce() {
        yield make();
        yield 2;
      },
    };
    const iterator = source.produce();
    const run = async () => {
      const baseline = collect();
      let result = await iterator.next();
      result = null;
      Goccia.gc();
      return Goccia.gc.bytesAllocated - baseline;
    };
    expect(await run()).toBeLessThan(SIZE / 2);
    expect((await iterator.next()).value).toBe(2);
  });

  test("is intact while the caller holds it across collections", async () => {
    const source = {
      async *produce() {
        yield new Uint8Array(1024).fill(1);
        Goccia.gc();
        yield new Uint8Array(1024).fill(2);
      },
    };
    const iterator = source.produce();
    const first = (await iterator.next()).value;
    Goccia.gc();
    const second = (await iterator.next()).value;
    Goccia.gc();
    expect([first[0], first.length, second[1023]]).toEqual([1, 1024, 2]);
  });

  test("is intact when the body awaits before it yields", async () => {
    const tick = () => Promise.resolve();
    const source = {
      async *produce() {
        await tick();
        yield { id: 1 };
        await tick();
        Goccia.gc();
        yield { id: 2 };
      },
    };
    const ids = [];
    for await (const item of source.produce()) {
      Goccia.gc();
      ids.push(item.id);
    }
    expect(ids).toEqual([1, 2]);
  });

  test("does not change what a throw() or return() at the yield sees", async () => {
    const seen = [];
    const source = {
      async *produce() {
        try {
          const sent = yield { id: 1 };
          seen.push(sent);
        } catch (error) {
          seen.push(`caught ${error}`);
        }
        try {
          yield { id: 2 };
        } finally {
          seen.push("finally");
        }
      },
    };
    const iterator = source.produce();
    expect((await iterator.next()).value.id).toBe(1);
    Goccia.gc();
    expect((await iterator.throw("boom")).value.id).toBe(2);
    Goccia.gc();
    expect(await iterator.return(7)).toEqual({ value: 7, done: true });
    expect(seen).toEqual(["caught boom", "finally"]);
  });
});
