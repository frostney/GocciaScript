/*---
description: Object operation benchmarks
---*/

import { bench, group } from "goccia:microbench";

group("object creation", () => {
  bench("create simple object", () => {
    const obj = { x: 1, y: 2, z: 3 };
  });

  bench("create nested object", () => {
    const obj = {
      name: "test",
      data: { x: 1, y: 2 },
      meta: { created: true, version: 1 }
    };
  });

  bench("create 50 objects via Array.from", () => {
    const arr = Array.from({ length: 50 }, (_, i) => ({ id: i, name: "item" }));
  });
});

group("object access", () => {
  bench("property read", ({ *setup() {
    const obj = (() => ({ a: 1, b: 2, c: 3, d: 4, e: 5 }))();
    yield () => {
      const sum = obj.a + obj.b + obj.c + obj.d + obj.e;
    };
  } }).setup);

  bench("Object.keys", ({ *setup() {
    const obj = (() => ({ a: 1, b: 2, c: 3, d: 4, e: 5, f: 6, g: 7, h: 8 }))();
    yield () => {
      const keys = Object.keys(obj);
    };
  } }).setup);

  bench("Object.entries", ({ *setup() {
    const obj = (() => ({ a: 1, b: 2, c: 3, d: 4, e: 5, f: 6, g: 7, h: 8 }))();
    yield () => {
      const entries = Object.entries(obj);
    };
  } }).setup);

  bench("spread operator", () => {
    const a = { x: 1, y: 2 };
    const b = { z: 3, w: 4 };
    const merged = { ...a, ...b };
  });
});
