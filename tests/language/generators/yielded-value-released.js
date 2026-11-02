/*---
description: >
  A generator suspended at a yield does not keep the value it yielded
  reachable: once the consumer drops the value, a collection reclaims it while
  the generator stays suspended (ES2026 §27.5.3.7 Yield hands the value out;
  the suspended context holds no reference to it). The value is still intact
  for as long as the consumer holds it, and the generator resumes normally.
features: [generators, Goccia.gc]
---*/

const SIZE = 32 * 1024 * 1024;

const collect = () => {
  Goccia.gc();
  return Goccia.gc.bytesAllocated;
};

describe("a value a suspended generator has yielded", () => {
  test("is reclaimed once the consumer drops it", () => {
    const source = {
      *produce() {
        yield new ArrayBuffer(SIZE);
        yield 2;
      },
    };
    const iterator = source.produce();
    const run = () => {
      const baseline = collect();
      let result = iterator.next();
      const length = result.value.byteLength;
      result = null;
      Goccia.gc();
      return [length, Goccia.gc.bytesAllocated - baseline];
    };
    const [length, grown] = run();
    expect(length).toBe(SIZE);
    expect(grown).toBeLessThan(SIZE / 2);
    expect(iterator.next()).toEqual({ value: 2, done: false });
    expect(iterator.next().done).toBe(true);
  });

  test("is intact while the consumer holds it across collections", () => {
    const source = {
      *produce() {
        yield new Uint8Array(1024).fill(1);
        yield new Uint8Array(1024).fill(2);
      },
    };
    const iterator = source.produce();
    const first = iterator.next().value;
    Goccia.gc();
    const second = iterator.next().value;
    Goccia.gc();
    expect([first[0], first.length, second[1023]]).toEqual([1, 1024, 2]);
  });

  test("is intact when native consumers collect what the generator yields", () => {
    const source = {
      *produce() {
        yield { id: 1 };
        Goccia.gc();
        yield { id: 2 };
        Goccia.gc();
        yield { id: 3 };
      },
    };
    expect(Array.from(source.produce()).map((item) => item.id)).toEqual([1, 2, 3]);
    expect([...source.produce()].map((item) => item.id)).toEqual([1, 2, 3]);
    expect(source.produce().toArray().map((item) => item.id)).toEqual([1, 2, 3]);
    const map = new Map(source.produce().map((item) => [item.id, item]));
    expect([...map.values()].map((item) => item.id)).toEqual([1, 2, 3]);
  });

  test("does not change what a throw() or return() at the yield sees", () => {
    const seen = [];
    const source = {
      *produce() {
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
    expect(iterator.next().value.id).toBe(1);
    Goccia.gc();
    expect(iterator.throw("boom").value.id).toBe(2);
    Goccia.gc();
    expect(iterator.return(7)).toEqual({ value: 7, done: true });
    expect(seen).toEqual(["caught boom", "finally"]);
  });
});
