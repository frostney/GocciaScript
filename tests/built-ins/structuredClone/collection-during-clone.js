/*---
description: structuredClone keeps every value it has yet to read or has already mapped alive when guest code collects during the clone
features: [structuredClone, Map]
---*/

// Getters run during the walk, and a getter is a GC safe point. These cases
// each leave one value reachable from nothing but the clone algorithm itself.
describe.runIf(typeof Goccia !== "undefined")("structuredClone across a collection", () => {
  // Issue #1575: the memory map is keyed by source objects. A getter's result
  // has no other owner, so once it was freed a later getter's object could be
  // allocated at the same address and be handed the earlier clone.
  test("gives each getter result its own clone when a getter collects", () => {
    const sources = Array.from({ length: 60 }, (_, i) => {
      const source = {};
      Object.defineProperty(source, "g", {
        enumerable: true,
        get() {
          if (i % 50 === 0) Goccia.gc();
          return { junk: new Array(3).fill(i) };
        },
      });
      return source;
    });

    const clone = structuredClone(sources);

    const wrong = clone
      .map((entry, index) => (entry.g.junk[0] === index ? "" : index + ":" + entry.g.junk[0]))
      .filter(Boolean);
    expect(wrong).toEqual([]);
  });

  // Each source is built inside a helper so that, once the helper returns, the
  // container is the only owner of the value under test: a register of the
  // test's own frame would otherwise keep it alive.
  test("keeps a Map entry's value alive when cloning its key deletes the entry", () => {
    const makeSource = () => {
      const map = new Map();
      map.set({
        get probe() {
          map.clear();
          Goccia.gc();
          return "key";
        },
      }, { payload: [1, 2, 3] });
      return map;
    };

    const clone = structuredClone(makeSource());

    expect(clone.size).toBe(1);
    const [[clonedKey, clonedValue]] = [...clone];
    expect(clonedKey.probe).toBe("key");
    expect(clonedValue).toEqual({ payload: [1, 2, 3] });
  });

  test("keeps a Map entry's primitive key alive when cloning its value deletes the entry", () => {
    const makeSource = () => {
      const map = new Map();
      map.set(["entry", "key"].join("-"), {
        get probe() {
          map.clear();
          Goccia.gc();
          return "value";
        },
      });
      return map;
    };

    const clone = structuredClone(makeSource());

    expect([...clone.keys()]).toEqual(["entry-key"]);
    expect(clone.get("entry-key").probe).toBe("value");
  });

  test("keeps an error alive while its name getter empties the array holding it", () => {
    const makeSource = () => {
      const array = [new RangeError("kept")];
      Object.defineProperty(array[0], "name", {
        get() {
          array.length = 0;
          Goccia.gc();
          return "RangeError";
        },
      });
      return array;
    };

    const clone = structuredClone(makeSource());

    expect(clone[0]).toBeInstanceOf(RangeError);
    expect(clone[0].message).toBe("kept");
  });
});
