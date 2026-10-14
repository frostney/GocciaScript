/*---
description: structuredClone keeps the values it still needs alive when guest code collects during the clone, and clones a Map entry's key before its value
features: [structuredClone, Map]
---*/

// Getters run during the walk, and a getter is a GC safe point. These cases
// each leave one value reachable from nothing but the clone algorithm itself.
describe.runIf(typeof Goccia !== "undefined")("structuredClone across a collection", () => {
  // A bare gc() usually leaves a freed object readable; allocating afterwards
  // is what reuses its memory and makes the stale reference observable.
  const gcChurn = () => {
    Goccia.gc();
    return Array.from({ length: 64 }, (_, i) => ({ payload: [i, i + 1], text: "x" + i }));
  };

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
          gcChurn();
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
          gcChurn();
          return "value";
        },
      });
      return map;
    };

    const clone = structuredClone(makeSource());

    expect([...clone.keys()]).toEqual(["entry-key"]);
    expect(clone.get("entry-key").probe).toBe("value");
  });

  test("keeps a Map entry's object key alive when cloning its value deletes the entry", () => {
    const makeSource = () => {
      const map = new Map();
      map.set({ tag: "key" }, {
        get probe() {
          map.clear();
          gcChurn();
          return "value";
        },
      });
      return map;
    };

    const clone = structuredClone(makeSource());

    const [[clonedKey, clonedValue]] = [...clone];
    expect(clonedKey).toEqual({ tag: "key" });
    expect(clonedValue.probe).toBe("value");
  });
});

describe("structuredClone of a Map entry", () => {
  // StructuredSerializeInternal serializes each entry's key, then its value.
  test("clones the key before the value", () => {
    const log = [];
    const source = new Map([[
      { get key() { log.push("key"); return 1; } },
      { get value() { log.push("value"); return 2; } },
    ]]);

    structuredClone(source);

    expect(log).toEqual(["key", "value"]);
  });
});
