/*---
description: A finally block run by return, break or continue resolves names in the scope enclosing the try statement
features: [try-finally, block-scoping]
---*/

describe("finally block reached by return", () => {
  test("reads the outer const when the try block shadows it after the return", () => {
    const run = () => {
      const label = "outer";
      const seen = [];
      try {
        if (seen.length === 0) return seen;
        const label = "inner";
        seen.push(label);
      } finally {
        seen.push(label);
      }
      return seen;
    };

    expect(run()).toEqual(["outer"]);
  });

  test("reads the outer const when the try block shadows it before the return", () => {
    const run = () => {
      const label = "outer";
      const seen = [];
      try {
        const label = "inner";
        seen.push(label);
        return seen;
      } finally {
        seen.push(label);
      }
    };

    expect(run()).toEqual(["inner", "outer"]);
  });

  test("reads the outer let, whichever kind of binding shadows it", () => {
    const run = (early) => {
      let label = "outer";
      const seen = [];
      try {
        if (early) return seen;
        let label = "inner";
        seen.push(label);
        return seen;
      } finally {
        seen.push(label);
      }
    };

    expect(run(true)).toEqual(["outer"]);
    expect(run(false)).toEqual(["inner", "outer"]);
  });

  test("reads the parameter, whichever kind of binding shadows it", () => {
    const run = (label, early) => {
      const seen = [];
      try {
        if (early) return seen;
        const label = "inner";
        seen.push(label);
        return seen;
      } finally {
        seen.push(label);
      }
    };

    expect(run("parameter", true)).toEqual(["parameter"]);
    expect(run("parameter", false)).toEqual(["inner", "parameter"]);
  });

  test("writes the outer let, not the binding that shadows it", () => {
    let count = 0;
    const run = () => {
      try {
        let count = 100;
        count += 1;
        return count;
      } finally {
        count += 1;
      }
    };

    expect(run()).toBe(101);
    expect(count).toBe(1);
  });

  test("a closure created in the finally block captures the outer binding", () => {
    const label = "outer";
    const readers = [];
    const run = () => {
      try {
        const label = "inner";
        return label;
      } finally {
        readers.push(() => label);
      }
    };

    expect(run()).toBe("inner");
    expect(readers[0]()).toBe("outer");
  });

  test("a name declared only in the try block is not visible", () => {
    const run = () => {
      const seen = [];
      try {
        const onlyInTryBlock = 1;
        seen.push(typeof onlyInTryBlock);
        return seen;
      } finally {
        seen.push(typeof onlyInTryBlock);
      }
    };

    expect(run()).toEqual(["number", "undefined"]);
  });

  test("ignores a binding in a block nested in the try block", () => {
    const run = () => {
      const label = "outer";
      const seen = [];
      try {
        {
          const label = "inner";
          seen.push(label);
          return seen;
        }
      } finally {
        seen.push(label);
      }
    };

    expect(run()).toEqual(["inner", "outer"]);
  });

  test("ignores the catch parameter when the catch block returns", () => {
    const run = () => {
      const label = "outer";
      const seen = [];
      try {
        throw "caught";
      } catch (label) {
        seen.push(label);
        return seen;
      } finally {
        seen.push(label);
      }
    };

    expect(run()).toEqual(["caught", "outer"]);
  });

  test("ignores a binding of the catch block when the catch block returns", () => {
    const run = () => {
      const label = "outer";
      const seen = [];
      try {
        throw "caught";
      } catch {
        const label = "inner";
        seen.push(label);
        return seen;
      } finally {
        seen.push(label);
      }
    };

    expect(run()).toEqual(["inner", "outer"]);
  });

  test("its own declarations still shadow the outer binding", () => {
    const run = () => {
      const label = "outer";
      const seen = [];
      try {
        const label = "inner";
        seen.push(label);
        return seen;
      } finally {
        seen.push(label);
        {
          const label = "finally";
          seen.push(label);
        }
        seen.push(label);
      }
    };

    expect(run()).toEqual(["inner", "outer", "finally", "outer"]);
  });

  test("finds the outer binding in a function with many locals", () => {
    const run = () => {
      const label = "outer";
      const [a0, a1, a2, a3, a4, a5, a6, a7, a8, a9] = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9];
      const [b0, b1, b2, b3, b4, b5, b6, b7, b8, b9] = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9];
      const [c0, c1, c2, c3, c4, c5, c6, c7, c8, c9] = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9];
      const [d0, d1, d2, d3, d4, d5, d6, d7, d8, d9] = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9];
      const [e0, e1, e2, e3, e4, e5, e6, e7, e8, e9] = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9];
      const [f0, f1, f2, f3, f4, f5, f6, f7, f8, f9] = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9];
      const [g0, g1, g2, g3, g4, g5, g6, g7, g8, g9] = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9];
      const seen = [];
      try {
        const label = "inner";
        const onlyInTryBlock = 1;
        seen.push(label);
        return seen;
      } finally {
        seen.push(label, typeof onlyInTryBlock);
        {
          const label = "finally";
          seen.push(label);
        }
        seen.push(label, a0 + g9);
      }
    };

    expect(run()).toEqual(["inner", "outer", "undefined", "finally", "outer", 9]);
  });

  test("each finally block of nested try statements sees its own enclosing scope", () => {
    const run = (returnFromFinally) => {
      const label = "outer";
      const seen = [];
      const inner = () => {
        try {
          const label = "middle";
          try {
            const label = "inner";
            seen.push(label);
            return 1;
          } finally {
            if (returnFromFinally) return 2;
            seen.push(label);
          }
        } finally {
          seen.push(label);
        }
      };
      seen.push(inner());
      return seen;
    };

    expect(run(false)).toEqual(["inner", "middle", "outer", 1]);
    expect(run(true)).toEqual(["inner", "outer", 2]);
  });
});

describe("finally block reached by break", () => {
  test("reads the outer const when the try block shadows it after the break", () => {
    const label = "outer";
    const seen = [];
    for (const item of [1, 2]) {
      try {
        if (item === 1) break;
        const label = "inner";
        seen.push(label);
      } finally {
        seen.push(label);
      }
    }

    expect(seen).toEqual(["outer"]);
  });

  test("reads the outer const when the try block shadows it before the break", () => {
    const label = "outer";
    const seen = [];
    for (const item of [1, 2]) {
      try {
        const label = "inner";
        seen.push(label + item);
        break;
      } finally {
        seen.push(label);
      }
    }

    expect(seen).toEqual(["inner1", "outer"]);
  });

  test("reads the outer let and the loop binding", () => {
    let label = "outer";
    const seen = [];
    for (const item of [1, 2]) {
      try {
        let label = "inner";
        const item = "shadow";
        seen.push(label + item);
        break;
      } finally {
        seen.push(label + item);
      }
    }

    expect(seen).toEqual(["innershadow", "outer1"]);
  });

  test("reads the parameter", () => {
    const run = (label) => {
      const seen = [];
      for (const item of [1, 2]) {
        try {
          const label = "inner";
          seen.push(label);
          break;
        } finally {
          seen.push(label);
        }
      }
      return seen;
    };

    expect(run("parameter")).toEqual(["inner", "parameter"]);
  });
});

describe("finally block reached by continue", () => {
  test("reads the outer const when the try block shadows it after the continue", () => {
    const label = "outer";
    const seen = [];
    for (const item of [1, 2]) {
      try {
        if (item === 1) continue;
        const label = "inner";
        seen.push(label);
      } finally {
        seen.push(label);
      }
    }

    expect(seen).toEqual(["outer", "inner", "outer"]);
  });

  test("reads the outer const when the try block shadows it before the continue", () => {
    const label = "outer";
    const seen = [];
    for (const item of [1, 2]) {
      try {
        const label = "inner";
        seen.push(label + item);
        continue;
      } finally {
        seen.push(label);
      }
    }

    expect(seen).toEqual(["inner1", "outer", "inner2", "outer"]);
  });

  test("the try block's bindings are back in scope after the continue", () => {
    const label = "outer";
    const seen = [];
    for (const item of [1, 2]) {
      try {
        const label = "inner";
        if (item === 1) continue;
        seen.push(label);
      } finally {
        seen.push(label);
      }
    }

    expect(seen).toEqual(["outer", "inner", "outer"]);
  });

  test("reads the outer let and the loop binding", () => {
    let label = "outer";
    const seen = [];
    for (const item of [1, 2]) {
      try {
        let label = "inner";
        const item = "shadow";
        seen.push(label + item);
        continue;
      } finally {
        seen.push(label + item);
      }
    }

    expect(seen).toEqual(["innershadow", "outer1", "innershadow", "outer2"]);
  });

  test("reads the parameter", () => {
    const run = (label) => {
      const seen = [];
      for (const item of [1, 2]) {
        try {
          const label = "inner";
          seen.push(label);
          continue;
        } finally {
          seen.push(label);
        }
      }
      return seen;
    };

    expect(run("parameter")).toEqual([
      "inner",
      "parameter",
      "inner",
      "parameter",
    ]);
  });
});
