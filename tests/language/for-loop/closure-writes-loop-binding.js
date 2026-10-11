/*---
description: >
  A closure's write to a traditional for loop's let binding carries into the
  next iteration (ES2026 §14.7.4.4 CreatePerIterationEnvironment copies each
  binding's last value, wherever it was written from)
features: [compat-traditional-for-loop, generators, async-functions]
---*/

describe("a closure's write to the loop binding carries into the next iteration", () => {
  test("closure in the body, limit read from a parameter", () => {
    const run = (limit) => {
      const seen = [];
      for (let i = 0; i < limit; i++) {
        const skip = () => {
          i = i + 1;
        };
        seen.push(i * 2);
        if (i === 1) {
          skip();
        }
        seen.push(i + 1);
      }
      return seen;
    };
    expect(run(5)).toEqual([0, 1, 2, 3, 6, 4, 8, 5]);
  });

  test("closure in the body, literal limit", () => {
    const seen = [];
    for (let i = 0; i < 5; i++) {
      const skip = () => {
        i = i + 1;
      };
      seen.push(i);
      if (i === 1) skip();
    }
    expect(seen).toEqual([0, 1, 3, 4]);
  });

  test("closure in the body, descending loop", () => {
    const seen = [];
    for (let i = 6; i > 0; i--) {
      const skip = () => {
        i -= 2;
      };
      seen.push(i);
      if (i === 5) skip();
    }
    expect(seen).toEqual([6, 5, 2, 1]);
  });

  test("closure write followed by continue", () => {
    const seen = [];
    for (let i = 0; i < 6; i++) {
      const skip = () => {
        i += 1;
      };
      if (i === 1) {
        skip();
        continue;
      }
      seen.push(i);
    }
    expect(seen).toEqual([0, 3, 4, 5]);
  });

  test("closure write that ends the loop", () => {
    const seen = [];
    for (let i = 0; i < 5; i++) {
      const finish = () => {
        i = 100;
      };
      seen.push(i);
      if (i === 2) finish();
    }
    expect(seen).toEqual([0, 1, 2]);
  });

  test("nested closure in the body", () => {
    const seen = [];
    for (let i = 0; i < 6; i++) {
      const outer = () => () => {
        i += 2;
      };
      seen.push(i);
      if (i === 1) outer()();
    }
    expect(seen).toEqual([0, 1, 4, 5]);
  });

  test("closure called from a callback inside the body", () => {
    const seen = [];
    for (let i = 0; i < 6; i++) {
      seen.push(i);
      [1].forEach(() => {
        if (i === 2) i += 2;
      });
    }
    expect(seen).toEqual([0, 1, 2, 5]);
  });

  test("closure created and called in the test", () => {
    const seen = [];
    for (let i = 0; (() => {
      i += 1;
      return i;
    })() < 8; i++) {
      seen.push(i);
    }
    expect(seen).toEqual([1, 3, 5, 7]);
  });

  test("closure created and called in the update", () => {
    const seen = [];
    for (let i = 0; i < 10; i++, (() => {
      i += 1;
    })()) {
      seen.push(i);
    }
    expect(seen).toEqual([0, 2, 4, 6, 8]);
  });

  test("closure created in the update and called in the next body", () => {
    const seen = [];
    let bump = null;
    for (let i = 0; i < 10; i++, bump = () => {
      i += 3;
    }) {
      seen.push(i);
      if (bump && i === 1) bump();
    }
    expect(seen).toEqual([0, 1, 5, 6, 7, 8, 9]);
  });

  test("closure created in the initializer writes the binding before the first iteration", () => {
    const seen = [];
    for (let i = 0, k = (() => {
      i = 3;
      return 10;
    })(); i < 6; i++) {
      const read = () => i;
      seen.push(read() + k);
    }
    expect(seen).toEqual([13, 14, 15]);
  });

  test("closure from an earlier iteration does not change the current one", () => {
    const writers = [];
    const seen = [];
    for (let i = 0; i < 4; i++) {
      writers.push(() => {
        i += 100;
      });
      if (i > 0) writers[i - 1]();
      seen.push(i);
    }
    expect(seen).toEqual([0, 1, 2, 3]);
  });

  test("closures keep the value their own iteration ended with", () => {
    const readers = [];
    for (let i = 0; i < 4; i++) {
      readers.push(() => i);
      if (i === 1) {
        const write = () => {
          i = 2;
        };
        write();
      }
    }
    expect(readers.map((read) => read())).toEqual([0, 2, 3]);
  });

  test("multiple let bindings", () => {
    const seen = [];
    for (let i = 0, j = 10; i < 5; i++, j--) {
      const write = () => {
        i += 1;
        j -= 5;
      };
      seen.push([i, j]);
      if (i === 1) write();
    }
    expect(seen).toEqual([[0, 10], [1, 9], [3, 3], [4, 2]]);
  });

  test("destructuring let head", () => {
    const seen = [];
    for (let [i, j] = [0, 0]; i < 5; i++) {
      const write = () => {
        i += 1;
        j += 1;
      };
      seen.push([i, j]);
      if (i === 1) write();
    }
    expect(seen).toEqual([[0, 0], [1, 0], [3, 1], [4, 1]]);
  });

  test("const head read by a closure", () => {
    const seen = [];
    let n = 0;
    for (const c = 7; n < 3; n++) {
      const read = () => c;
      seen.push(read());
    }
    expect(seen).toEqual([7, 7, 7]);
  });

  test("generator that yields inside the loop", () => {
    const gen = ({
      *values() {
        for (let i = 0; i < 6; i++) {
          const skip = () => {
            i += 1;
          };
          yield i;
          if (i === 1) skip();
        }
      },
    }).values();
    expect([...gen]).toEqual([0, 1, 3, 4, 5]);
  });

  test("generator that yields inside the writing closure's iteration", () => {
    const gen = ({
      *values(limit) {
        for (let i = 0; i < limit; i++) {
          const skip = () => {
            i += 1;
          };
          if (i === 1) skip();
          yield i;
        }
      },
    }).values(6);
    expect([...gen]).toEqual([0, 2, 3, 4, 5]);
  });

  test("async function that awaits inside the loop", async () => {
    const run = async () => {
      const seen = [];
      for (let i = 0; i < 6; i++) {
        const skip = () => {
          i += 1;
        };
        await null;
        seen.push(i);
        if (i === 1) skip();
        await null;
      }
      return seen;
    };
    expect(await run()).toEqual([0, 1, 3, 4, 5]);
  });

  test("async function whose closure writes after an await", async () => {
    const run = async (limit) => {
      const seen = [];
      for (let i = 0; i < limit; i++) {
        const skip = async () => {
          await null;
          i += 1;
        };
        seen.push(i);
        if (i === 1) await skip();
      }
      return seen;
    };
    expect(await run(6)).toEqual([0, 1, 3, 4, 5]);
  });
});
