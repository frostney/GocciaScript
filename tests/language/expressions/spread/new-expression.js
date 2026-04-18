/*---
description: Spread syntax in new expressions
features: [spread, class]
---*/

describe("spread in new expressions", () => {
  test("spread array into constructor", () => {
    class Collector {
      constructor(...args) {
        this.args = args;
      }
    }

    const items = [1, 2, 3];
    const c = new Collector(...items);
    expect(c.args).toEqual([1, 2, 3]);
  });

  test("spread with leading arguments", () => {
    class Collector {
      constructor(...args) {
        this.args = args;
      }
    }

    const rest = [2, 3];
    const c = new Collector(1, ...rest);
    expect(c.args).toEqual([1, 2, 3]);
  });

  test("spread with trailing arguments", () => {
    class Collector {
      constructor(...args) {
        this.args = args;
      }
    }

    const start = [1, 2];
    const c = new Collector(...start, 3);
    expect(c.args).toEqual([1, 2, 3]);
  });

  test("spread with arguments on both sides", () => {
    class Collector {
      constructor(...args) {
        this.args = args;
      }
    }

    const mid = [2, 3];
    const c = new Collector(1, ...mid, 4);
    expect(c.args).toEqual([1, 2, 3, 4]);
  });

  test("multiple spreads in one new expression", () => {
    class Collector {
      constructor(...args) {
        this.args = args;
      }
    }

    const a = [1, 2];
    const b = [3, 4];
    const c = new Collector(...a, ...b);
    expect(c.args).toEqual([1, 2, 3, 4]);
  });

  test("spread empty array", () => {
    class Collector {
      constructor(...args) {
        this.args = args;
      }
    }

    const empty = [];
    const c = new Collector(...empty);
    expect(c.args).toEqual([]);
  });

  test("spread with built-in Date constructor", () => {
    const parts = [2025, 0, 15];
    const d = new Date(...parts);
    expect(d.getFullYear()).toBe(2025);
    expect(d.getMonth()).toBe(0);
    expect(d.getDate()).toBe(15);
  });

  test("spread with built-in Array constructor", () => {
    const items = [10, 20, 30];
    const arr = new Array(...items);
    expect(arr).toEqual([10, 20, 30]);
  });

  test("spread with Map constructor", () => {
    const entries = [["a", 1], ["b", 2]];
    const m = new Map(entries);
    expect(m.get("a")).toBe(1);
    expect(m.get("b")).toBe(2);
  });

  test("spread with Set constructor", () => {
    const vals = [1, 2, 3, 2, 1];
    const s = new Set(vals);
    expect(s.size).toBe(3);
  });

  test("spread iterable into constructor", () => {
    class Collector {
      constructor(...args) {
        this.args = args;
      }
    }

    const iterable = {
      [Symbol.iterator]() {
        let i = 0;
        return {
          next() {
            return i < 3 ? { value: i++, done: false } : { done: true };
          },
        };
      },
    };
    const c = new Collector(...iterable);
    expect(c.args).toEqual([0, 1, 2]);
  });

  test("spread with TypedArray constructor", () => {
    const data = [1, 2, 3, 4];
    const ta = new Uint8Array(data);
    expect(ta.length).toBe(4);
    expect(ta[0]).toBe(1);
    expect(ta[3]).toBe(4);
  });
});
