/*---
description: WeakMap and WeakSet weak collection benchmarks
---*/

import { bench, group } from "goccia:microbench";

const makeObjectKeys = () => Array.from({ length: 50 }, (_, i) => ({ index: i }));
const makeEntries = () => makeObjectKeys().map((key, i) => [key, { value: i }]);

group("WeakMap operations", () => {
  bench("constructor from 50 entries", ({ *setup() {
    const entries = makeEntries();
    yield () => {
      const map = new WeakMap(entries);
    };
  } }).setup);

  bench("set 50 object keys", ({ *setup() {
    const keys = makeObjectKeys();
    yield () => {
      const map = new WeakMap();
      keys.forEach((key, i) => { map.set(key, i); });
    };
  } }).setup);

  bench("get lookups (50 entries)", ({ *setup() {
    const state = (() => {
      const entries = makeEntries();
      return { map: new WeakMap(entries), keys: entries.map((entry) => entry[0]) };
    })();
    yield () => {
      const a = state.map.get(state.keys[0]);
      const b = state.map.get(state.keys[25]);
      const c = state.map.get(state.keys[49]);
      const d = state.map.get({});
    };
  } }).setup);

  bench("has checks (50 entries)", ({ *setup() {
    const state = (() => {
      const entries = makeEntries();
      return { map: new WeakMap(entries), keys: entries.map((entry) => entry[0]) };
    })();
    yield () => {
      const a = state.map.has(state.keys[0]);
      const b = state.map.has(state.keys[25]);
      const c = state.map.has({});
    };
  } }).setup);

  bench("delete entries", ({ *setup() {
    const keys = makeObjectKeys();
    yield () => {
      const map = new WeakMap();
      keys.forEach((key, i) => { map.set(key, i); });
      map.delete(keys[0]);
      map.delete(keys[10]);
      map.delete(keys[20]);
      map.delete(keys[30]);
      map.delete(keys[40]);
    };
  } }).setup);

  bench("non-registered symbol keys", ({ *setup() {
    const keys = (() => Array.from({ length: 20 }, (_, i) => Symbol("weak-map-" + i)))();
    yield () => {
      const map = new WeakMap();
      keys.forEach((key, i) => { map.set(key, i); });
      const value = map.get(keys[10]);
    };
  } }).setup);

  bench("getOrInsert", ({ *setup() {
    const keys = makeObjectKeys();
    yield () => {
      const map = new WeakMap();
      keys.forEach((key, i) => {
        const value = map.getOrInsert(key, i);
      });
      const existing = map.getOrInsert(keys[25], "existing");
    };
  } }).setup);

  bench("getOrInsertComputed", ({ *setup() {
    const keys = makeObjectKeys();
    yield () => {
      const map = new WeakMap();
      keys.forEach((key, i) => {
        const value = map.getOrInsertComputed(key, () => i);
      });
      const existing = map.getOrInsertComputed(keys[25], () => "existing");
    };
  } }).setup);

  bench("forced gc live-key retention", ({ *setup() {
    const state = (() => {
      const key = {};
      return { key, map: new WeakMap([[key, { value: 1 }]]) };
    })();
    yield () => {
      Array.from({ length: 20 }, (_, i) => i).forEach((i) => {
        state.map.set({ key: i }, { value: i });
      });
      Goccia.gc();
      const retained = state.map.get(state.key).value;
    };
  } }).setup);
});

group("WeakSet operations", () => {
  bench("constructor from 50 values", ({ *setup() {
    const values = makeObjectKeys();
    yield () => {
      const set = new WeakSet(values);
    };
  } }).setup);

  bench("add 50 object values", ({ *setup() {
    const values = makeObjectKeys();
    yield () => {
      const set = new WeakSet();
      values.forEach((value) => { set.add(value); });
    };
  } }).setup);

  bench("has checks (50 values)", ({ *setup() {
    const state = (() => {
      const values = makeObjectKeys();
      return { set: new WeakSet(values), values };
    })();
    yield () => {
      const a = state.set.has(state.values[0]);
      const b = state.set.has(state.values[25]);
      const c = state.set.has({});
    };
  } }).setup);

  bench("delete values", ({ *setup() {
    const values = makeObjectKeys();
    yield () => {
      const set = new WeakSet(values);
      set.delete(values[0]);
      set.delete(values[10]);
      set.delete(values[20]);
      set.delete(values[30]);
      set.delete(values[40]);
    };
  } }).setup);

  bench("non-registered symbol values", ({ *setup() {
    const values = (() => Array.from({ length: 20 }, (_, i) => Symbol("weak-set-" + i)))();
    yield () => {
      const set = new WeakSet();
      values.forEach((value) => { set.add(value); });
      const present = set.has(values[10]);
    };
  } }).setup);

  bench("forced gc pruning smoke", ({ *setup() {
    const state = (() => {
      const live = {};
      return { live, set: new WeakSet([live]) };
    })();
    yield () => {
      Array.from({ length: 20 }, (_, i) => i).forEach((i) => {
        state.set.add({ value: i });
      });
      Goccia.gc();
      const retained = state.set.has(state.live);
    };
  } }).setup);
});
