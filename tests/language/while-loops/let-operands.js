/*---
description: let bindings and parameters used as operands inside while and do...while loops
features: [let, while-loops, closures, arrow-functions]
---*/

const pass = (value) => value;

describe("operands in a while loop", () => {
  test("the condition and the body read the current value", () => {
    const run = (limit) => {
      let count = 0;
      let total = 0;
      while (count < limit) {
        total = total + count * 2;
        count = count + 1;
      }
      return [count, total, count + total];
    };

    expect(run(pass(4))).toEqual([4, 12, 16]);
  });

  test("a parameter counted down to zero", () => {
    const run = (n) => {
      let product = 1;
      while (n > 1) {
        product = product * n;
        n = n - 1;
      }
      return [product, n + 1];
    };

    expect(run(pass(5))).toEqual([120, 2]);
  });

  test("a condition that rebinds its own operand", () => {
    const run = (limit) => {
      let count = 0;
      const seen = [];
      while (count < ((count = count + 1), limit)) {
        seen.push(count * 10);
      }
      return [seen, count];
    };

    expect(run(pass(3))).toEqual([[10, 20, 30], 4]);
  });

  test("a closure created later in the body writes a binding read earlier", () => {
    const run = (limit) => {
      let value = 1;
      let count = 0;
      let write = null;
      const seen = [];
      while (count < limit) {
        seen.push(value + count, value * 2);
        if (write) {
          write(count * 10);
        }
        write = (next) => {
          value = next;
        };
        count = count + 1;
      }
      seen.push(value + 1);
      return seen;
    };

    expect(run(pass(3))).toEqual([1, 2, 2, 2, 12, 20, 21]);
  });

  test("a closure created in the condition writes a binding the body reads", () => {
    const run = (limit) => {
      let value = 0;
      let count = 0;
      let write = null;
      const seen = [];
      while (((write = () => (value = value + 5)), count < limit)) {
        seen.push(value + count);
        write();
        count = count + 1;
      }
      return seen;
    };

    expect(run(pass(3))).toEqual([0, 6, 12]);
  });

  test("a nested loop reads a binding that the outer loop's closure writes", () => {
    const run = (rows, columns) => {
      let cell = 1;
      let row = 0;
      let write = null;
      const seen = [];
      while (row < rows) {
        let column = 0;
        while (column < columns) {
          seen.push(cell + column);
          column = column + 1;
        }
        if (write) {
          write(row * 10);
        }
        write = (next) => {
          cell = next;
        };
        row = row + 1;
      }
      return seen;
    };

    expect(run(pass(3), pass(2))).toEqual([1, 2, 1, 2, 10, 11]);
  });
});

describe("operands in a do...while loop", () => {
  test("the body runs before the condition reads the binding", () => {
    const run = (limit) => {
      let count = 0;
      let total = 0;
      do {
        total = total + count;
        count += 1;
      } while (count < limit);
      return [count, total];
    };

    expect(run(pass(4))).toEqual([4, 6]);
    expect(run(pass(0))).toEqual([1, 0]);
  });

  test("a closure created in the condition writes a binding the next pass reads", () => {
    const run = (limit) => {
      let value = 1;
      let count = 0;
      let write = null;
      const seen = [];
      do {
        seen.push(value + count);
        if (write) {
          write(count + 100);
        }
        count = count + 1;
      } while (((write = (next) => (value = next)), count < limit));
      return seen;
    };

    expect(run(pass(3))).toEqual([1, 2, 103]);
  });

  test("a closure created in a nested loop writes a binding the outer loop reads", () => {
    const inCondition = (limit) => {
      let value = 1;
      let count = 0;
      let write = null;
      const seen = [];
      while (count < limit) {
        seen.push(value + count);
        if (write) {
          write(count + 100);
        }
        let inner = 0;
        while (((write = (next) => (value = next)), inner < 1)) {
          inner = inner + 1;
        }
        do {
          inner = inner + 1;
        } while (inner < 3);
        count = count + 1;
      }
      return seen;
    };
    const inBody = (limit) => {
      let value = 1;
      let count = 0;
      let write = null;
      const seen = [];
      while (count < limit) {
        seen.push(value + count);
        if (write) {
          write(count + 100);
        }
        let inner = 0;
        do {
          write = (next) => (value = next);
          inner = inner + 1;
        } while (inner < 2);
        count = count + 1;
      }
      return seen;
    };
    const inNestedCondition = (limit) => {
      let value = 1;
      let count = 0;
      let write = null;
      const seen = [];
      while (count < limit) {
        seen.push(value + count);
        if (write) {
          write(count + 100);
        }
        let inner = 0;
        do {
          inner = inner + 1;
        } while (((write = (next) => (value = next)), inner < 2));
        count = count + 1;
      }
      return seen;
    };

    expect(inCondition(pass(3))).toEqual([1, 2, 103]);
    expect(inBody(pass(3))).toEqual([1, 2, 103]);
    expect(inNestedCondition(pass(3))).toEqual([1, 2, 103]);
  });

  test("a closure created in the body writes a binding the next pass reads", () => {
    const run = (limit) => {
      let value = 1;
      let count = 0;
      let write = null;
      const seen = [];
      do {
        seen.push(value + count);
        if (write) {
          write(count + 100);
        }
        write = (next) => {
          value = next;
        };
        count = count + 1;
      } while (count < limit);
      return seen;
    };

    expect(run(pass(3))).toEqual([1, 2, 103]);
  });
});
