/*---
description: Object operation benchmarks
---*/

suite("object creation", () => {
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

suite("object access", () => {
  bench("property read", () => {
    const obj = { a: 1, b: 2, c: 3, d: 4, e: 5 };
    const sum = obj.a + obj.b + obj.c + obj.d + obj.e;
  });

  bench("Object.keys", () => {
    const obj = { a: 1, b: 2, c: 3, d: 4, e: 5, f: 6, g: 7, h: 8 };
    const keys = Object.keys(obj);
  });

  bench("Object.entries", () => {
    const obj = { a: 1, b: 2, c: 3, d: 4, e: 5, f: 6, g: 7, h: 8 };
    const entries = Object.entries(obj);
  });

  bench("spread operator", () => {
    const a = { x: 1, y: 2 };
    const b = { z: 3, w: 4 };
    const merged = { ...a, ...b };
  });
});
