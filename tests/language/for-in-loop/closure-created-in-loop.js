/*---
description: A closure created inside a for...in loop writes a let binding that the next pass of the loop reads
features: [compat-for-in-loop, let, closures, destructuring]
---*/

const pass = (value) => value;

describe("a closure created in a for...in loop", () => {
  const sink = (value) => value;

  test("in the body", () => {
    const run = (source) => {
      let value = 1;
      let write = null;
      const seen = [];
      for (const key in source) {
        seen.push(value + 1, value + key);
        if (write) write(source[key] * 10);
        write = (next) => (value = next);
      }
      return seen;
    };

    expect(run(pass({ a: 1, b: 2, c: 3 }))).toEqual([2, "1a", 2, "1b", 21, "20c"]);
  });

  test("in the object expression of a nested loop", () => {
    const run = (steps) => {
      let value = 1;
      let write = null;
      const seen = [];
      for (const step of steps) {
        seen.push(value + 1);
        if (write) write(step * 10);
        for (const key in ((write = (next) => (value = next)), { a: 1 })) {
          sink(key);
        }
      }
      return seen;
    };

    expect(run(pass([1, 2, 3]))).toEqual([2, 2, 21]);
  });

  test("in the body of a nested loop", () => {
    const run = (steps) => {
      let value = 1;
      let write = null;
      const seen = [];
      for (const step of steps) {
        seen.push(value + 1);
        if (write) write(step * 10);
        for (const key in { a: 1 }) {
          write = (next) => (value = next + key.length - 1);
        }
      }
      return seen;
    };

    expect(run(pass([1, 2, 3]))).toEqual([2, 2, 21]);
  });

  test("in a default of the binding pattern", () => {
    const run = (steps) => {
      let value = 1;
      let write = null;
      const seen = [];
      for (const step of steps) {
        seen.push(value + 1);
        if (write) write(step * 10);
        for (const { missing = (write = (next) => (value = next)) } in { a: 1 }) {
          sink(missing);
        }
      }
      return seen;
    };

    expect(run(pass([1, 2, 3]))).toEqual([2, 2, 21]);
  });

  test("in a default of the assignment target", () => {
    const run = (steps) => {
      let value = 1;
      let write = null;
      const seen = [];
      const holder = {};
      for (const step of steps) {
        seen.push(value + 1);
        if (write) write(step * 10);
        for ({ missing: holder.made = (write = (next) => (value = next)) } in { a: 1 }) {
          sink(holder.made);
        }
      }
      return seen;
    };

    expect(run(pass([1, 2, 3]))).toEqual([2, 2, 21]);
  });

  test("the loop variable is an ordinary operand", () => {
    const run = (source) => {
      let joined = "";
      let count = 0;
      for (let key in source) {
        key = key + key;
        joined = joined + key + source[key[0]];
        count += 1;
      }
      return [joined, count];
    };

    expect(run(pass({ a: 1, b: 2 }))).toEqual(["aa1bb2", 2]);
  });
});
