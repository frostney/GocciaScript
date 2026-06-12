/*---
description: Cross-instance property access benchmarks (shape-sensitive patterns)
---*/

const INSTANCE_COUNT = 1000;

suite("cross-instance field reads", () => {
  bench("class instance fields across 1000 instances", {
    setup: () => {
      class Point {
        x;
        y;
        constructor(x, y) {
          this.x = x;
          this.y = y;
        }
      }
      return Array.from({ length: INSTANCE_COUNT }, (_, i) => new Point(i, i * 2));
    },
    run: (points) => {
      let sum = 0;
      for (const p of points) {
        sum += p.x + p.y;
      }
      return sum;
    },
  });

  bench("object literal fields across 1000 literals", {
    setup: () => Array.from({ length: INSTANCE_COUNT }, (_, i) => ({ x: i, y: i * 2 })),
    run: (objects) => {
      let sum = 0;
      for (const o of objects) {
        sum += o.x + o.y;
      }
      return sum;
    },
  });

  bench("mixed-shape literals across 1000 literals", {
    setup: () =>
      Array.from({ length: INSTANCE_COUNT }, (_, i) => {
        switch (i % 4) {
          case 0:
            return { x: i, y: i * 2 };
          case 1:
            return { y: i * 2, x: i };
          case 2:
            return { x: i, pad: true, y: i * 2 };
          default:
            return { pad: true, x: i, y: i * 2 };
        }
      }),
    run: (objects) => {
      let sum = 0;
      for (const o of objects) {
        sum += o.x + o.y;
      }
      return sum;
    },
  });
});

suite("cross-instance method dispatch", () => {
  bench("own-class method across 1000 instances", {
    setup: () => {
      class Point {
        x;
        y;
        constructor(x, y) {
          this.x = x;
          this.y = y;
        }
        norm() {
          return this.x + this.y;
        }
      }
      return Array.from({ length: INSTANCE_COUNT }, (_, i) => new Point(i, i * 2));
    },
    run: (points) => {
      let sum = 0;
      for (const p of points) {
        sum += p.norm();
      }
      return sum;
    },
  });

  bench("inherited method across 1000 instances", {
    setup: () => {
      class Base {
        x;
        constructor(x) {
          this.x = x;
        }
        value() {
          return this.x;
        }
      }
      class Derived extends Base {
        y;
        constructor(x, y) {
          super(x);
          this.y = y;
        }
      }
      return Array.from({ length: INSTANCE_COUNT }, (_, i) => new Derived(i, i * 2));
    },
    run: (points) => {
      let sum = 0;
      for (const p of points) {
        sum += p.value();
      }
      return sum;
    },
  });
});
