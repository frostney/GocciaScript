/*---
description: Native callback APIs keep callback state reachable during explicit GC
features: [Array.prototype.map, Array.from, TypedArray, Iterator, Map, Set, Object.groupBy, fetch, URLSearchParams]
---*/

const hasGoccia = typeof Goccia !== "undefined";

describe.runIf(hasGoccia)("native callback GC roots", () => {
  test("Array prototype callbacks survive explicit GC across iterations", () => {
    let sum = 0;
    [1, 2].forEach((value) => {
      Goccia.gc();
      sum += value;
    });

    const mapped = [1, 2].map((value) => {
      Goccia.gc();
      return value * 2;
    });
    const filtered = [1, 2].filter((value) => {
      Goccia.gc();
      return value > 0;
    });
    const flatMapped = [1].flatMap((value) => {
      Goccia.gc();
      return [value, value + 1];
    });
    const reduced = [1, 2].reduce((accumulator, value) => {
      Goccia.gc();
      return accumulator + value;
    }, 0);
    const sorted = [2, 1].sort((left, right) => {
      Goccia.gc();
      return left - right;
    });
    const toSorted = [2, 1].toSorted((left, right) => {
      Goccia.gc();
      return left - right;
    });
    const some = [0, 1].some((value) => {
      Goccia.gc();
      return value === 1;
    });
    const every = [1, 1].every((value) => {
      Goccia.gc();
      return value === 1;
    });
    const found = [0, 2].find((value) => {
      Goccia.gc();
      return value === 2;
    });
    const foundIndex = [0, 2].findIndex((value) => {
      Goccia.gc();
      return value === 2;
    });
    const foundLast = [2, 0, 2].findLast((value) => {
      Goccia.gc();
      return value === 2;
    });
    const foundLastIndex = [2, 0, 2].findLastIndex((value) => {
      Goccia.gc();
      return value === 2;
    });

    expect(sum).toBe(3);
    expect(mapped).toEqual([2, 4]);
    expect(filtered).toEqual([1, 2]);
    expect(flatMapped).toEqual([1, 2]);
    expect(reduced).toBe(3);
    expect(sorted).toEqual([1, 2]);
    expect(toSorted).toEqual([1, 2]);
    expect(some).toBe(true);
    expect(every).toBe(true);
    expect(found).toBe(2);
    expect(foundIndex).toBe(1);
    expect(foundLast).toBe(2);
    expect(foundLastIndex).toBe(2);
  });

  test("Array.from and Array.fromAsync mapping callbacks survive explicit GC", () => {
    const fromResult = Array.from([1, 2], (value) => {
      Goccia.gc();
      return value * 2;
    });

    expect(fromResult).toEqual([2, 4]);
    return Array.fromAsync([1, 2], (value) => {
      Goccia.gc();
      return value * 3;
    }).then((fromAsyncResult) => {
      expect(fromAsyncResult).toEqual([3, 6]);
    });
  });

  test("TypedArray callbacks survive explicit GC", () => {
    let sum = 0;
    new Uint8Array([1, 2]).forEach((value) => {
      Goccia.gc();
      sum += value;
    });

    const mapped = new Uint8Array([1, 2]).map((value) => {
      Goccia.gc();
      return value * 2;
    });
    const filtered = new Uint8Array([1, 2]).filter((value) => {
      Goccia.gc();
      return value > 0;
    });
    const reduced = new Uint8Array([1, 2]).reduce((accumulator, value) => {
      Goccia.gc();
      return accumulator + value;
    }, 0);
    const sorted = new Uint8Array([2, 1]).sort((left, right) => {
      Goccia.gc();
      return left - right;
    });
    const toSorted = new Uint8Array([2, 1]).toSorted((left, right) => {
      Goccia.gc();
      return left - right;
    });
    const some = new Uint8Array([0, 1]).some((value) => {
      Goccia.gc();
      return value === 1;
    });
    const every = new Uint8Array([1, 1]).every((value) => {
      Goccia.gc();
      return value === 1;
    });
    const found = new Uint8Array([0, 2]).find((value) => {
      Goccia.gc();
      return value === 2;
    });
    const foundIndex = new Uint8Array([0, 2]).findIndex((value) => {
      Goccia.gc();
      return value === 2;
    });
    const fromResult = Uint8Array.from([1, 2], (value) => {
      Goccia.gc();
      return value * 2;
    });

    expect(sum).toBe(3);
    expect(Array.from(mapped)).toEqual([2, 4]);
    expect(Array.from(filtered)).toEqual([1, 2]);
    expect(reduced).toBe(3);
    expect(Array.from(sorted)).toEqual([1, 2]);
    expect(Array.from(toSorted)).toEqual([1, 2]);
    expect(some).toBe(true);
    expect(every).toBe(true);
    expect(found).toBe(2);
    expect(foundIndex).toBe(1);
    expect(Array.from(fromResult)).toEqual([2, 4]);
  });

  test("Iterator helper callbacks survive explicit GC", () => {
    let sum = 0;
    [1, 2].values().forEach((value) => {
      Goccia.gc();
      sum += value;
    });
    const reduced = [1, 2].values().reduce((accumulator, value) => {
      Goccia.gc();
      return accumulator + value;
    }, 0);
    const some = [0, 1].values().some((value) => {
      Goccia.gc();
      return value === 1;
    });
    const every = [1, 1].values().every((value) => {
      Goccia.gc();
      return value === 1;
    });
    const found = [0, 2].values().find((value) => {
      Goccia.gc();
      return value === 2;
    });

    expect(sum).toBe(3);
    expect(reduced).toBe(3);
    expect(some).toBe(true);
    expect(every).toBe(true);
    expect(found).toBe(2);
  });

  test("collection callbacks survive explicit GC", () => {
    let mapSum = 0;
    new Map([["a", 1], ["b", 2]]).forEach((value) => {
      Goccia.gc();
      mapSum += value;
    });

    let setSum = 0;
    new Set([1, 2]).forEach((value) => {
      Goccia.gc();
      setSum += value;
    });

    const objectGroups = Object.groupBy([1, 2], () => {
      Goccia.gc();
      return "items";
    });
    const mapGroups = Map.groupBy([1, 2], () => {
      Goccia.gc();
      return "items";
    });

    const upsertMap = new Map();
    const key = {};
    const inserted = upsertMap.getOrInsertComputed(key, () => {
      Goccia.gc();
      return 3;
    });

    let headersJoined = "";
    new Headers({ A: "1" }).forEach((value, name) => {
      Goccia.gc();
      headersJoined += name + value;
    });

    let paramsJoined = "";
    new URLSearchParams("a=1").forEach((value, name) => {
      Goccia.gc();
      paramsJoined += name + value;
    });

    expect(mapSum).toBe(3);
    expect(setSum).toBe(3);
    expect(objectGroups.items).toEqual([1, 2]);
    expect(mapGroups.get("items")).toEqual([1, 2]);
    expect(inserted).toBe(3);
    expect(upsertMap.get(key)).toBe(3);
    expect(headersJoined).toBe("a1");
    expect(paramsJoined).toBe("a1");
  });
});
