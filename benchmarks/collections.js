/*---
description: Set and Map collection benchmarks
---*/

import { bench, group } from "goccia:microbench";

group("Set operations", () => {
  bench("add 50 elements", ({ *setup() {
    const items = (() => Array.from({ length: 50 }, (_, i) => i))();
    yield () => {
      const s = new Set();
      items.forEach((i) => { s.add(i); });
    };
  } }).setup);

  bench("has lookup (50 elements)", ({ *setup() {
    const s = (() => new Set(Array.from({ length: 50 }, (_, i) => i)))();
    yield () => {
      const a = s.has(0);
      const b = s.has(25);
      const c = s.has(49);
      const d = s.has(999);
    };
  } }).setup);

  bench("delete elements", ({ *setup() {
    const items = (() => Array.from({ length: 20 }, (_, i) => i))();
    yield () => {
      const s = new Set(items);
      s.delete(0);
      s.delete(5);
      s.delete(10);
      s.delete(15);
      s.delete(19);
    };
  } }).setup);

  bench("forEach iteration", ({ *setup() {
    const s = (() => new Set(Array.from({ length: 50 }, (_, i) => i)))();
    yield () => {
      let sum = 0;
      s.forEach((v) => { sum = sum + v; });
    };
  } }).setup);

  bench("spread to array", ({ *setup() {
    const s = (() => new Set(Array.from({ length: 30 }, (_, i) => i)))();
    yield () => {
      const arr = [...s];
    };
  } }).setup);

  bench("deduplicate array", () => {
    const arr = [1, 2, 3, 1, 2, 3, 4, 5, 4, 5, 6, 7, 6, 7, 8, 9, 8, 9, 10, 10];
    const unique = [...new Set(arr)];
  });
});

group("Map operations", () => {
  bench("set 50 entries", ({ *setup() {
    const items = (() => Array.from({ length: 50 }, (_, i) => i))();
    yield () => {
      const m = new Map();
      items.forEach((i) => { m.set("key" + i, i); });
    };
  } }).setup);

  bench("get lookup (50 entries)", ({ *setup() {
    const m = (() => new Map(Array.from({ length: 50 }, (_, i) => ["key" + i, i])))();
    yield () => {
      const a = m.get("key0");
      const b = m.get("key25");
      const c = m.get("key49");
      const d = m.get("missing");
    };
  } }).setup);

  bench("has check", ({ *setup() {
    const m = (() => new Map(Array.from({ length: 50 }, (_, i) => ["key" + i, i])))();
    yield () => {
      const a = m.has("key0");
      const b = m.has("key25");
      const c = m.has("missing");
    };
  } }).setup);

  bench("delete entries", ({ *setup() {
    const entries = (() => Array.from({ length: 20 }, (_, i) => ["key" + i, i]))();
    yield () => {
      const m = new Map(entries);
      m.delete("key0");
      m.delete("key5");
      m.delete("key10");
      m.delete("key15");
      m.delete("key19");
    };
  } }).setup);

  bench("forEach iteration", ({ *setup() {
    const m = (() => new Map(Array.from({ length: 50 }, (_, i) => ["key" + i, i])))();
    yield () => {
      let sum = 0;
      m.forEach((v) => { sum = sum + v; });
    };
  } }).setup);

  bench("keys/values/entries", ({ *setup() {
    const m = (() => new Map(Array.from({ length: 30 }, (_, i) => ["key" + i, i])))();
    yield () => {
      const keys = [...m.keys()];
      const vals = [...m.values()];
      const ents = [...m.entries()];
    };
  } }).setup);
});
