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

  test("is reclaimed when the yield's result is bound to a local", () => {
    const source = {
      *produce() {
        const sent = yield new ArrayBuffer(SIZE);
        yield sent;
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
    expect(iterator.next(5)).toEqual({ value: 5, done: false });
  });

  test("is reclaimed when it was an argument of the call that made the operand", () => {
    const identity = (value) => value;
    const source = {
      *produce() {
        yield identity(new ArrayBuffer(SIZE));
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
  });

  test("does not leave a value an exited block dropped in the slot a yield initializer reuses", () => {
    const source = {
      *produce() {
        {
          const discarded = new ArrayBuffer(SIZE);
          discarded.byteLength;
        }
        {
          const sent = yield 1;
          sent;
        }
      },
    };
    const iterator = source.produce();
    const baseline = collect();
    const first = iterator.next();
    Goccia.gc();
    const grown = Goccia.gc.bytesAllocated - baseline;
    expect(first.value).toBe(1);
    expect(grown).toBeLessThan(SIZE / 2);
  });

  test("does not leave a value an exited block dropped in the slot of a binding whose initializer nests a yield", () => {
    const source = {
      *produce(pick) {
        {
          const discarded = new ArrayBuffer(SIZE);
          discarded.byteLength;
        }
        {
          const sent = pick ? yield 1 : 0;
          return sent;
        }
      },
    };
    const iterator = source.produce(true);
    const baseline = collect();
    const first = iterator.next();
    Goccia.gc();
    const grown = Goccia.gc.bytesAllocated - baseline;
    expect(first.value).toBe(1);
    expect(grown).toBeLessThan(SIZE / 2);
    expect(iterator.next("resumed")).toEqual({ value: "resumed", done: true });
  });

  test("does not leave a value an exited block dropped in the slot of a binding declared after a yield initializer", () => {
    const source = {
      *produce() {
        {
          const first = new ArrayBuffer(SIZE);
          const second = new ArrayBuffer(SIZE);
          first.byteLength;
          second.byteLength;
        }
        {
          const sent = yield 1, later = 2;
          return [sent, later];
        }
      },
    };
    const iterator = source.produce();
    const baseline = collect();
    const first = iterator.next();
    Goccia.gc();
    const grown = Goccia.gc.bytesAllocated - baseline;
    expect(first.value).toBe(1);
    expect(grown).toBeLessThan(SIZE / 2);
    expect(iterator.next("resumed")).toEqual({ value: ["resumed", 2], done: true });
  });

  test("does not leave an initializer's operand in the binding a yield initializes", () => {
    const source = {
      *produce() {
        const sent = new ArrayBuffer(SIZE) && (yield 1);
        return sent;
      },
    };
    const iterator = source.produce();
    const baseline = collect();
    const first = iterator.next();
    Goccia.gc();
    const grown = Goccia.gc.bytesAllocated - baseline;
    expect(first.value).toBe(1);
    expect(grown).toBeLessThan(SIZE / 2);
    expect(iterator.next("resumed")).toEqual({ value: "resumed", done: true });
  });

  test("does not keep an operand's arguments when the operand throws into a catch", () => {
    const take = (buffer, value) => value;
    const fail = () => {
      throw new Error("operand failed");
    };
    const source = {
      *produce() {
        try {
          yield take(new ArrayBuffer(SIZE), fail());
        } catch (error) {
          Goccia.gc();
          yield Goccia.gc.bytesAllocated;
        }
      },
    };
    const baseline = collect();
    const after = source.produce().next().value;
    expect(after - baseline).toBeLessThan(SIZE / 2);
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
