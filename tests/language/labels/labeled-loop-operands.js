/*---
description: let bindings used as operands stay correct when control leaves a loop body through a labeled break or continue
features: [compat-label, let, closures]
---*/

const pass = (value) => value;

describe("labeled jumps and operands", () => {
  test("continue to an outer loop keeps the accumulators", () => {
    const run = (rows, columns) => {
      let total = 0;
      let skipped = 0;
      outer: for (let row = 0; row < rows; row++) {
        for (let column = 0; column < columns; column++) {
          if (column > row) {
            skipped = skipped + 1;
            continue outer;
          }
          total = total + row * 10 + column;
        }
      }
      return [total, skipped, total + skipped];
    };

    expect(run(pass(3), pass(3))).toEqual([84, 2, 86]);
  });

  test("break from an inner loop leaves the bindings as written", () => {
    const run = (rows, columns) => {
      let last = -1;
      let count = 0;
      outer: for (let row = 0; row < rows; row++) {
        for (let column = 0; column < columns; column++) {
          last = row * columns + column;
          if (last === 4) {
            break outer;
          }
          count += 1;
        }
      }
      return [last, count, last + count];
    };

    expect(run(pass(3), pass(3))).toEqual([4, 4, 8]);
  });

  test("a binding declared after a labeled continue starts each pass uninitialized", () => {
    const run = (rows) => {
      const seen = [];
      outer: for (let row = 0; row < rows; row++) {
        try {
          seen.push(late + 1);
        } catch (error) {
          seen.push(error instanceof ReferenceError);
        }
        for (let column = 0; column < 2; column++) {
          if (row === 1) {
            continue outer;
          }
        }
        let late = row * 10;
        late = late + 1;
        seen.push(late + 1);
      }
      return seen;
    };

    expect(run(pass(3))).toEqual([true, 2, true, true, 22]);
  });

  test("a closure created after a labeled continue writes a binding read at the top", () => {
    const run = (rows) => {
      let value = 1;
      let write = null;
      const seen = [];
      outer: for (let row = 0; row < rows; row++) {
        seen.push(value + row);
        for (let column = 0; column < 2; column++) {
          if (write && column === 1) {
            write(row * 100);
            continue outer;
          }
        }
        write = (next) => {
          value = next;
        };
      }
      return seen;
    };

    expect(run(pass(3))).toEqual([1, 2, 102]);
  });

  test("break out of a labeled block skips the rest of the block", () => {
    const run = (stop) => {
      let value = 1;
      block: {
        value = value + 1;
        if (stop) {
          break block;
        }
        let inner = value * 10;
        inner = inner + 1;
        value = value + inner;
      }
      return value + 1;
    };

    expect(run(pass(true))).toBe(3);
    expect(run(pass(false))).toBe(24);
  });

  test("a finally loop that runs for a labeled break out of another loop rereads a binding its closure writes", () => {
    // The finally block is compiled again at the break, inside the loop the
    // break leaves, although it is not part of that loop.
    const run = (start) => {
      let value = start;
      const seen = [];
      exit: try {
        for (let index = 0; index < 3; index++) {
          break exit;
        }
      } finally {
        for (let step = 0; step < 3; step++) {
          seen.push(value + 1);
          const scale = () => {
            value = value * 10;
          };
          scale();
        }
      }
      return seen;
    };

    expect(run(pass(1))).toEqual([2, 11, 101]);
  });
});
