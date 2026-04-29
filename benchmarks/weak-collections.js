/*---
description: WeakMap and WeakSet weak collection benchmarks
---*/

const makeObjectKeys = () => Array.from({ length: 50 }, (_, i) => ({ index: i }));
const makeEntries = () => makeObjectKeys().map((key, i) => [key, { value: i }]);

suite("WeakMap operations", () => {
  bench("constructor from 50 entries", {
    setup: makeEntries,
    run: (entries) => {
      const map = new WeakMap(entries);
    },
  });

  bench("set 50 object keys", {
    setup: makeObjectKeys,
    run: (keys) => {
      const map = new WeakMap();
      keys.forEach((key, i) => { map.set(key, i); });
    },
  });

  bench("get lookups (50 entries)", {
    setup: () => {
      const entries = makeEntries();
      return { map: new WeakMap(entries), keys: entries.map((entry) => entry[0]) };
    },
    run: (state) => {
      const a = state.map.get(state.keys[0]);
      const b = state.map.get(state.keys[25]);
      const c = state.map.get(state.keys[49]);
      const d = state.map.get({});
    },
  });

  bench("has checks (50 entries)", {
    setup: () => {
      const entries = makeEntries();
      return { map: new WeakMap(entries), keys: entries.map((entry) => entry[0]) };
    },
    run: (state) => {
      const a = state.map.has(state.keys[0]);
      const b = state.map.has(state.keys[25]);
      const c = state.map.has({});
    },
  });

  bench("delete entries", {
    setup: makeObjectKeys,
    run: (keys) => {
      const map = new WeakMap();
      keys.forEach((key, i) => { map.set(key, i); });
      map.delete(keys[0]);
      map.delete(keys[10]);
      map.delete(keys[20]);
      map.delete(keys[30]);
      map.delete(keys[40]);
    },
  });

  bench("non-registered symbol keys", {
    setup: () => Array.from({ length: 20 }, (_, i) => Symbol("weak-map-" + i)),
    run: (keys) => {
      const map = new WeakMap();
      keys.forEach((key, i) => { map.set(key, i); });
      const value = map.get(keys[10]);
    },
  });

  bench("getOrInsert", {
    setup: makeObjectKeys,
    run: (keys) => {
      const map = new WeakMap();
      keys.forEach((key, i) => {
        const value = map.getOrInsert(key, i);
      });
      const existing = map.getOrInsert(keys[25], "existing");
    },
  });

  bench("getOrInsertComputed", {
    setup: makeObjectKeys,
    run: (keys) => {
      const map = new WeakMap();
      keys.forEach((key, i) => {
        const value = map.getOrInsertComputed(key, () => i);
      });
      const existing = map.getOrInsertComputed(keys[25], () => "existing");
    },
  });

  bench("forced gc live-key retention", {
    setup: () => {
      const key = {};
      return { key, map: new WeakMap([[key, { value: 1 }]]) };
    },
    run: (state) => {
      Array.from({ length: 20 }, (_, i) => i).forEach((i) => {
        state.map.set({ key: i }, { value: i });
      });
      Goccia.gc();
      const retained = state.map.get(state.key).value;
    },
  });
});

suite("WeakSet operations", () => {
  bench("constructor from 50 values", {
    setup: makeObjectKeys,
    run: (values) => {
      const set = new WeakSet(values);
    },
  });

  bench("add 50 object values", {
    setup: makeObjectKeys,
    run: (values) => {
      const set = new WeakSet();
      values.forEach((value) => { set.add(value); });
    },
  });

  bench("has checks (50 values)", {
    setup: () => {
      const values = makeObjectKeys();
      return { set: new WeakSet(values), values };
    },
    run: (state) => {
      const a = state.set.has(state.values[0]);
      const b = state.set.has(state.values[25]);
      const c = state.set.has({});
    },
  });

  bench("delete values", {
    setup: makeObjectKeys,
    run: (values) => {
      const set = new WeakSet(values);
      set.delete(values[0]);
      set.delete(values[10]);
      set.delete(values[20]);
      set.delete(values[30]);
      set.delete(values[40]);
    },
  });

  bench("non-registered symbol values", {
    setup: () => Array.from({ length: 20 }, (_, i) => Symbol("weak-set-" + i)),
    run: (values) => {
      const set = new WeakSet();
      values.forEach((value) => { set.add(value); });
      const present = set.has(values[10]);
    },
  });

  bench("forced gc pruning smoke", {
    setup: () => {
      const live = {};
      return { live, set: new WeakSet([live]) };
    },
    run: (state) => {
      Array.from({ length: 20 }, (_, i) => i).forEach((i) => {
        state.set.add({ value: i });
      });
      Goccia.gc();
      const retained = state.set.has(state.live);
    },
  });
});
