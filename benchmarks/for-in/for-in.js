/*---
description: for...in enumeration and prototype-chain key dedup benchmarks
---*/

import { bench, group } from "goccia:microbench";

group("for...in", () => {
  // Build a `levels`-deep prototype chain where every level redeclares the
  // same `width` keys (plus one unique key per level). for...in must dedup
  // the shared keys down the chain, which stresses the key-dedup set.
  const buildChain = (levels, width) => {
    const sharedKeys = Array.from({ length: width }, (_, i) => "k" + i);
    return Array.from({ length: levels }).reduce((proto, _unused, level) => {
      const obj = Object.create(proto);
      for (const key of sharedKeys) obj[key] = level;
      obj["unique" + level] = level;
      return obj;
    }, null);
  };

  const flat50 = Array.from({ length: 50 }).reduce((obj, _unused, i) => {
    obj["k" + i] = i;
    return obj;
  }, {});

  bench("for...in over 50 own keys", () => {
    let count = 0;
    for (const key in flat50) count = count + 1;
    return count;
  });

  bench("for...in over an 8-level chain of 50 shared keys", ({ *setup() {
    const obj = (() => buildChain(8, 50))();
    yield () => {
      let count = 0;
      for (const key in obj) count = count + 1;
      return count;
    };
  } }).setup);

  bench("for...in over a 16-level chain of 100 shared keys", ({ *setup() {
    const obj = (() => buildChain(16, 100))();
    yield () => {
      let count = 0;
      for (const key in obj) count = count + 1;
      return count;
    };
  } }).setup);
});
