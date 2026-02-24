/*---
description: Object operation benchmarks
---*/

suite("object creation", () => {
  bench("create simple object", {
    run: () => {
      const obj = { x: 1, y: 2, z: 3 };
    },
  });

  bench("create nested object", {
    run: () => {
      const obj = {
        name: "test",
        data: { x: 1, y: 2 },
        meta: { created: true, version: 1 }
      };
    },
  });

  bench("create 50 objects via Array.from", {
    run: () => {
      const arr = Array.from({ length: 50 }, (_, i) => ({ id: i, name: "item" }));
    },
  });
});

suite("object access", () => {
  bench("property read", {
    setup: () => ({ a: 1, b: 2, c: 3, d: 4, e: 5 }),
    run: (obj) => {
      const sum = obj.a + obj.b + obj.c + obj.d + obj.e;
    },
  });

  bench("Object.keys", {
    setup: () => ({ a: 1, b: 2, c: 3, d: 4, e: 5, f: 6, g: 7, h: 8 }),
    run: (obj) => {
      const keys = Object.keys(obj);
    },
  });

  bench("Object.entries", {
    setup: () => ({ a: 1, b: 2, c: 3, d: 4, e: 5, f: 6, g: 7, h: 8 }),
    run: (obj) => {
      const entries = Object.entries(obj);
    },
  });

  bench("spread operator", {
    run: () => {
      const a = { x: 1, y: 2 };
      const b = { z: 3, w: 4 };
      const merged = { ...a, ...b };
    },
  });
});
