/*---
description: A function that reads its own let binding or parameter sees every write made through a closure, wherever the closure is created
features: [let, arrow-functions, closures, for-of, classes, getters-setters]
---*/

// A closure and the function that created it share one binding. The cases
// below create the closure after the code that reads the binding, inside a
// loop, so the read runs again once the closure exists.

const pass = (value) => value;

describe("a closure created later in the same loop", () => {
  test("writes a let binding that an earlier operand reads", () => {
    const run = (steps) => {
      let value = 1;
      let write = null;
      const seen = [];
      for (const step of steps) {
        seen.push(value + 1);
        if (write) {
          write(step * 10);
        }
        write = (next) => {
          value = next;
        };
      }
      seen.push(value + 1);
      return seen;
    };

    expect(run(pass([1, 2, 3]))).toEqual([2, 2, 21, 31]);
  });

  test("writes a parameter that an earlier operand reads", () => {
    const run = (value, steps) => {
      const writers = [];
      const seen = [];
      for (const step of steps) {
        seen.push(value * 2, value - 1, value < 10);
        writers.push(() => {
          value = value + 5;
        });
        writers[writers.length - 1]();
      }
      return seen;
    };

    expect(run(pass(1), pass([0, 1, 2]))).toEqual([2, 0, true, 12, 5, true, 22, 10, false]);
  });

  test("is seen by an assignment and a compound assignment", () => {
    const run = (steps) => {
      let total = 0;
      let reset = null;
      const seen = [];
      for (const step of steps) {
        total = total + step;
        total += 1;
        seen.push(total);
        if (reset) {
          reset();
        }
        reset = () => {
          total = 100;
        };
      }
      return seen;
    };

    expect(run(pass([1, 2, 3]))).toEqual([2, 5, 104]);
  });

  test("is seen by an element read and an element store", () => {
    const run = (steps) => {
      let target = [0, 0, 0];
      let index = 0;
      let advance = null;
      const first = target;
      const seen = [];
      for (const step of steps) {
        target[index] = step;
        seen.push(target[index]);
        if (advance) {
          advance();
        }
        advance = () => {
          index = index + 1;
          target = [9, 9, 9];
        };
      }
      return [first, target, index, seen];
    };

    expect(run(pass([1, 2, 3]))).toEqual([[2, 0, 0], [9, 9, 9], 2, [1, 2, 3]]);
  });

  test("is created in an inner loop and read in the outer one", () => {
    const run = (rows, columns) => {
      let cell = 0;
      const writers = [];
      const seen = [];
      for (const row of rows) {
        seen.push(cell + row);
        for (const column of columns) {
          seen.push(cell * 2);
          writers.push(() => {
            cell = cell + column;
          });
        }
        for (const writer of writers) {
          writer();
        }
      }
      return seen;
    };

    expect(run(pass([10, 20]), pass([1, 2]))).toEqual([10, 0, 0, 23, 6, 6]);
  });

  test("is created in the outer loop and read in an inner one", () => {
    const run = (rows, columns) => {
      let cell = 1;
      let write = null;
      const seen = [];
      for (const row of rows) {
        for (const column of columns) {
          seen.push(cell + column);
        }
        if (write) {
          write(row);
        }
        write = (next) => {
          cell = next;
        };
      }
      for (const column of columns) {
        seen.push(cell + column);
      }
      return seen;
    };

    expect(run(pass([5, 6, 7]), pass([1, 2]))).toEqual([2, 3, 2, 3, 7, 8, 8, 9]);
  });

  test("is created after an inner loop that the read follows", () => {
    const run = (rows, columns) => {
      let cell = 1;
      let write = null;
      const seen = [];
      for (const row of rows) {
        for (const column of columns) {
          seen.push(column);
        }
        seen.push(cell + row);
        if (write) {
          write(row * 10);
        }
        write = (next) => {
          cell = next;
        };
      }
      return seen;
    };

    expect(run(pass([1, 2, 3]), pass([0]))).toEqual([0, 2, 0, 3, 0, 23]);
  });

  test("is created only on some iterations", () => {
    const run = (steps) => {
      let value = 0;
      let write = null;
      const seen = [];
      for (const step of steps) {
        seen.push(value + step);
        if (write) {
          write();
        }
        if (step === 2) {
          write = () => {
            value = value + 100;
          };
        }
      }
      return seen;
    };

    expect(run(pass([1, 2, 3, 4]))).toEqual([1, 2, 3, 104]);
  });

  test("is an object method, an accessor or a class method", () => {
    const run = (steps) => {
      let value = 0;
      let holder = null;
      const seen = [];
      for (const step of steps) {
        seen.push(value + 1);
        if (holder) {
          holder.bump();
          holder.next = step;
          seen.push(holder.current + 1);
        }
        if (step === 1) {
          holder = {
            bump() {
              value = value + 10;
            },
            set next(amount) {
              value = value + amount;
            },
            get current() {
              value = value + 1000;
              return value;
            },
          };
        } else {
          class Holder {
            bump() {
              value = value * 2;
            }
            set next(amount) {
              value = value - amount;
            }
            get current() {
              return value;
            }
          }
          holder = new Holder();
        }
      }
      return seen;
    };

    expect(run(pass([1, 2, 3]))).toEqual([1, 1, 1013, 1013, 2022]);
  });

  test("is a setter or a getter and nothing else", () => {
    const viaSetter = (steps) => {
      let value = 1;
      let box = null;
      const seen = [];
      for (const step of steps) {
        seen.push(value + 1);
        if (box) {
          box.next = step * 10;
        }
        box = {
          set next(amount) {
            value = amount;
          },
        };
      }
      return seen;
    };
    const viaGetter = (steps) => {
      let value = 1;
      let box = null;
      const seen = [];
      for (const step of steps) {
        seen.push(value + 1);
        if (box) {
          seen.push(box.bump);
        }
        box = {
          get bump() {
            value = value + step * 10;
            return 0;
          },
        };
      }
      return seen;
    };
    const viaComputedAccessors = (steps, name) => {
      let value = 1;
      let box = null;
      const seen = [];
      for (const step of steps) {
        seen.push(value + 1);
        if (box) {
          box[name] = step * 10;
          seen.push(box[name]);
        }
        box = {
          set [name](amount) {
            value = amount;
          },
          get [name]() {
            value = value + 1;
            return 0;
          },
        };
      }
      return seen;
    };

    expect(viaSetter(pass([1, 2, 3]))).toEqual([2, 2, 21]);
    expect(viaGetter(pass([1, 2, 3]))).toEqual([2, 2, 0, 12, 0]);
    expect(viaComputedAccessors(pass([1, 2, 3]), pass("slot"))).toEqual([2, 2, 0, 22, 0]);
  });

  test("is created in a finally block inside the loop", () => {
    const run = (steps) => {
      let value = 1;
      let write = null;
      const seen = [];
      for (const step of steps) {
        try {
          seen.push(value + step);
          if (write) {
            write(step);
          }
        } finally {
          write = (next) => {
            value = next * 100;
          };
        }
      }
      seen.push(value + 1);
      return seen;
    };

    expect(run(pass([1, 2, 3]))).toEqual([2, 3, 203, 301]);
  });

  test("captures a binding declared inside the loop body", () => {
    const run = (steps) => {
      const seen = [];
      const writers = [];
      for (const step of steps) {
        let local = step;
        seen.push(local + 1);
        writers.push(() => {
          local = local + 100;
          return local;
        });
        local = local * 2;
        seen.push(local + 1, writers[writers.length - 1](), local + 1);
      }
      return [seen, writers.map((writer) => writer())];
    };

    expect(run(pass([1, 2]))).toEqual([[2, 3, 102, 103, 3, 5, 104, 105], [202, 204]]);
  });
});

describe("a closure created outside the loop", () => {
  test("written before the loop and called inside it", () => {
    const run = (steps) => {
      let value = 0;
      const bump = (amount) => {
        value = value + amount;
      };
      const seen = [];
      for (const step of steps) {
        seen.push(value + 1);
        bump(step);
        seen.push(value + 1);
      }
      return seen;
    };

    expect(run(pass([1, 2]))).toEqual([1, 2, 2, 4]);
  });

  test("created after the loop leaves the loop's reads alone", () => {
    const run = (steps) => {
      let value = 0;
      const seen = [];
      for (const step of steps) {
        value = value + step;
        seen.push(value * 2);
      }
      const bump = () => {
        value = value + 100;
      };
      bump();
      seen.push(value + 1);
      return seen;
    };

    expect(run(pass([1, 2]))).toEqual([2, 6, 104]);
  });

  test("called from a callback during the read", () => {
    const run = (items) => {
      let total = 0;
      items.forEach((item) => {
        total = total + item;
      });
      const doubled = items.map((item) => item * 2 + total);
      return [total, doubled, total + 1];
    };

    expect(run(pass([1, 2, 3]))).toEqual([6, [8, 10, 12], 7]);
  });
});
