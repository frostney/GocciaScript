/*---
description: Loop variables and other let bindings used as operands inside a traditional for loop
features: [let, for-loop, closures, arrow-functions]
---*/

const pass = (value) => value;

describe("the loop variable as an operand", () => {
  test("a counted loop reads the current count", () => {
    const run = (limit) => {
      const seen = [];
      let total = 0;
      for (let i = 0; i < limit; i++) {
        total = total + i * 2;
        seen.push(i + 1, i < 2, total);
      }
      return seen;
    };

    expect(run(pass(3))).toEqual([1, true, 0, 2, true, 2, 3, false, 6]);
  });

  test("a counted loop indexes with the count", () => {
    const run = (items) => {
      let sum = 0;
      for (let i = 0; i < items.length; i++) {
        items[i] = items[i] * 2 + i;
        sum += items[i];
      }
      return [items, sum];
    };

    expect(run(pass([1, 2, 3]))).toEqual([[2, 5, 8], 15]);
  });

  test("the body may assign the loop variable", () => {
    const run = (limit) => {
      const seen = [];
      for (let i = 0; i < limit; i++) {
        seen.push(i * 10);
        if (i === 1) {
          i = i + 2;
        }
        seen.push(i + 1);
      }
      return seen;
    };

    expect(run(pass(6))).toEqual([0, 1, 10, 4, 40, 5, 50, 6]);
  });

  test("the update may be any expression of the loop variable", () => {
    const run = (limit) => {
      const seen = [];
      for (let i = 1; i < limit; i = i * 2 + (i = i + 1)) {
        seen.push(i + i);
      }
      return seen;
    };

    expect(run(pass(40))).toEqual([2, 8, 26]);
  });

  test("a counting-down loop and a loop with two variables", () => {
    const run = (limit) => {
      const seen = [];
      for (let i = limit; i > 0; i--) {
        seen.push(i * i);
      }
      for (let low = 0, high = limit; low < high; low++, high--) {
        seen.push(high - low);
      }
      return seen;
    };

    expect(run(pass(3))).toEqual([9, 4, 1, 3, 1]);
  });

  test("the limit is read again on every iteration", () => {
    const run = (limit) => {
      let count = 0;
      for (let i = 0; i < limit; i++) {
        count = count + 1;
        if (i === 0) {
          limit = limit + 2;
        }
      }
      return [count, limit];
    };

    expect(run(pass(2))).toEqual([4, 4]);
  });

  test("break and continue leave the bindings as they are", () => {
    const run = (limit) => {
      let total = 0;
      let last = -1;
      for (let i = 0; i < limit; i++) {
        last = i;
        if (i % 2 === 1) {
          continue;
        }
        if (i > 5) {
          break;
        }
        total = total + i;
      }
      return [total, last, total + last];
    };

    expect(run(pass(10))).toEqual([6, 6, 12]);
  });

  test("nested loops keep their own variables", () => {
    const run = (rows, columns) => {
      let cells = 0;
      const seen = [];
      for (let row = 0; row < rows; row++) {
        for (let column = 0; column < columns; column++) {
          cells = cells + row * columns + column;
          seen.push(row * 10 + column);
        }
      }
      return [cells, seen];
    };

    expect(run(pass(2), pass(3))).toEqual([15, [0, 1, 2, 10, 11, 12]]);
  });
});

describe("closures created in the loop", () => {
  test("a closure created later in the body writes a binding read earlier", () => {
    const run = (limit) => {
      let value = 1;
      let write = null;
      const seen = [];
      for (let i = 0; i < limit; i++) {
        seen.push(value + i);
        if (write) {
          write(i * 10);
        }
        write = (next) => {
          value = next;
        };
      }
      seen.push(value + 1);
      return seen;
    };

    expect(run(pass(3))).toEqual([1, 2, 12, 21]);
  });

  test("each iteration's closure sees its own copy of the loop variable", () => {
    const run = (limit) => {
      const readers = [];
      const seen = [];
      for (let i = 0; i < limit; i++) {
        readers.push(() => i * 10);
        seen.push(i + 100);
      }
      return [seen, readers.map((read) => read())];
    };

    expect(run(pass(3))).toEqual([[100, 101, 102], [0, 10, 20]]);
  });

  test("a closure created in the update expression", () => {
    const run = (limit) => {
      let value = 0;
      const writers = [];
      const seen = [];
      for (let i = 0; i < limit; writers.push(() => (value = value + 10)), i++) {
        seen.push(value + i);
        for (const writer of writers) {
          writer();
        }
      }
      return seen;
    };

    expect(run(pass(3))).toEqual([0, 1, 12]);
  });

  test("a closure created in the condition", () => {
    const run = (limit) => {
      let value = 0;
      let write = null;
      const seen = [];
      for (let i = 0; ((write = () => (value = value + 5)), i < limit); i++) {
        seen.push(value + i);
        write();
      }
      return seen;
    };

    expect(run(pass(3))).toEqual([0, 6, 12]);
  });

  test("a closure created in the head of a nested loop", () => {
    const inInit = (steps) => {
      let value = 1;
      let write = null;
      const seen = [];
      for (const step of steps) {
        seen.push(value + 1);
        if (write) write(step * 10);
        for (let i = ((write = (next) => (value = next)), 0); i < 1; i++) {
          seen.push(i);
        }
      }
      return seen;
    };
    const inCondition = (steps) => {
      let value = 1;
      let write = null;
      const seen = [];
      for (const step of steps) {
        seen.push(value + 1);
        if (write) write(step * 10);
        for (let i = 0; ((write = (next) => (value = next)), i < 1); i++) {
          seen.push(i);
        }
      }
      return seen;
    };
    const inUpdate = (steps) => {
      let value = 1;
      let write = null;
      const seen = [];
      for (const step of steps) {
        seen.push(value + 1);
        if (write) write(step * 10);
        for (let i = 0; i < 1; write = (next) => (value = next), i++) {
          seen.push(i);
        }
      }
      return seen;
    };
    const inBody = (steps) => {
      let value = 1;
      let write = null;
      const seen = [];
      for (const step of steps) {
        seen.push(value + 1);
        if (write) write(step * 10);
        for (let i = 0; i < 1; i++) {
          write = (next) => (value = next);
          seen.push(i);
        }
      }
      return seen;
    };

    expect(inInit(pass([1, 2, 3]))).toEqual([2, 0, 2, 0, 21, 0]);
    expect(inCondition(pass([1, 2, 3]))).toEqual([2, 0, 2, 0, 21, 0]);
    expect(inUpdate(pass([1, 2, 3]))).toEqual([2, 0, 2, 0, 21, 0]);
    expect(inBody(pass([1, 2, 3]))).toEqual([2, 0, 2, 0, 21, 0]);
  });

  test("a closure created in an inner loop writes a binding the outer loop reads", () => {
    const run = (rows, columns) => {
      let cell = 0;
      const writers = [];
      const seen = [];
      for (let row = 0; row < rows; row++) {
        seen.push(cell + row);
        for (let column = 0; column < columns; column++) {
          writers.push(() => {
            cell = cell + column + 1;
          });
        }
        for (let n = 0; n < writers.length; n++) {
          writers[n]();
        }
      }
      return seen;
    };

    expect(run(pass(3), pass(2))).toEqual([0, 4, 11]);
  });
});
