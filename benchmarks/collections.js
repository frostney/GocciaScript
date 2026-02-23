/*---
description: Set and Map collection benchmarks
---*/

suite("Set operations", () => {
  bench("add 50 elements", {
    setup: () => Array.from({ length: 50 }, (_, i) => i),
    run: (items) => {
      const s = new Set();
      items.forEach((i) => { s.add(i); });
    },
  });

  bench("has lookup (50 elements)", {
    setup: () => new Set(Array.from({ length: 50 }, (_, i) => i)),
    run: (s) => {
      const a = s.has(0);
      const b = s.has(25);
      const c = s.has(49);
      const d = s.has(999);
    },
  });

  bench("delete elements", {
    setup: () => Array.from({ length: 20 }, (_, i) => i),
    run: (items) => {
      const s = new Set(items);
      s.delete(0);
      s.delete(5);
      s.delete(10);
      s.delete(15);
      s.delete(19);
    },
  });

  bench("forEach iteration", {
    setup: () => new Set(Array.from({ length: 50 }, (_, i) => i)),
    run: (s) => {
      let sum = 0;
      s.forEach((v) => { sum = sum + v; });
    },
  });

  bench("spread to array", {
    setup: () => new Set(Array.from({ length: 30 }, (_, i) => i)),
    run: (s) => {
      const arr = [...s];
    },
  });

  bench("deduplicate array", {
    run: () => {
      const arr = [1, 2, 3, 1, 2, 3, 4, 5, 4, 5, 6, 7, 6, 7, 8, 9, 8, 9, 10, 10];
      const unique = [...new Set(arr)];
    },
  });
});

suite("Map operations", () => {
  bench("set 50 entries", {
    setup: () => Array.from({ length: 50 }, (_, i) => i),
    run: (items) => {
      const m = new Map();
      items.forEach((i) => { m.set("key" + i, i); });
    },
  });

  bench("get lookup (50 entries)", {
    setup: () => new Map(Array.from({ length: 50 }, (_, i) => ["key" + i, i])),
    run: (m) => {
      const a = m.get("key0");
      const b = m.get("key25");
      const c = m.get("key49");
      const d = m.get("missing");
    },
  });

  bench("has check", {
    setup: () => new Map(Array.from({ length: 50 }, (_, i) => ["key" + i, i])),
    run: (m) => {
      const a = m.has("key0");
      const b = m.has("key25");
      const c = m.has("missing");
    },
  });

  bench("delete entries", {
    setup: () => Array.from({ length: 20 }, (_, i) => ["key" + i, i]),
    run: (entries) => {
      const m = new Map(entries);
      m.delete("key0");
      m.delete("key5");
      m.delete("key10");
      m.delete("key15");
      m.delete("key19");
    },
  });

  bench("forEach iteration", {
    setup: () => new Map(Array.from({ length: 50 }, (_, i) => ["key" + i, i])),
    run: (m) => {
      let sum = 0;
      m.forEach((v) => { sum = sum + v; });
    },
  });

  bench("keys/values/entries", {
    setup: () => new Map(Array.from({ length: 30 }, (_, i) => ["key" + i, i])),
    run: (m) => {
      const keys = [...m.keys()];
      const vals = [...m.values()];
      const ents = [...m.entries()];
    },
  });
});
