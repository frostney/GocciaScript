/*---
description: Temporal Dead Zone enforcement for let declarations
features: [let, temporal-dead-zone]
---*/

describe("Temporal Dead Zone", () => {
  test("accessing let before declaration throws", () => {
    expect(() => {
      const val = x;
      let x = 10;
    }).toThrow(ReferenceError);
  });

  test("accessing const before declaration throws", () => {
    expect(() => {
      const val = y;
      const y = 20;
    }).toThrow(ReferenceError);
  });

  test("let self-reference in initializer throws", () => {
    expect(() => {
      let x = x;
    }).toThrow(ReferenceError);
  });

  test("const self-reference in initializer throws", () => {
    expect(() => {
      const x = x;
    }).toThrow(ReferenceError);
  });

  test("typeof on undeclared variable returns undefined", () => {
    expect(typeof nonExistentVar).toBe("undefined");
  });

  test("let in block scope does not leak", () => {
    let outer = "outer";
    {
      let inner = "inner";
      expect(inner).toBe("inner");
    }
    expect(() => {
      const val = inner;
    }).toThrow(ReferenceError);
  });

  test("let in nested blocks shadows outer", () => {
    let x = 1;
    {
      let x = 2;
      expect(x).toBe(2);
    }
    expect(x).toBe(1);
  });

  test("const reassignment throws TypeError", () => {
    const a = 42;
    expect(() => {
      a = 99;
    }).toThrow(TypeError);
  });

  test("const object properties can still be mutated", () => {
    const obj = { x: 1 };
    obj.x = 2;
    expect(obj.x).toBe(2);
  });

  test("const array elements can still be mutated", () => {
    const arr = [1, 2, 3];
    arr[0] = 99;
    expect(arr[0]).toBe(99);
  });

  test("multiple let declarations in same scope", () => {
    let a = 1;
    let b = 2;
    let c = 3;
    expect(a + b + c).toBe(6);
  });

  test("captured let read before declaration throws", () => {
    {
      const readBeforeDeclaration = () => capturedLet;
      expect(() => {
        readBeforeDeclaration();
      }).toThrow(ReferenceError);
      let capturedLet;
    }
  });

  test("captured let write before declaration throws", () => {
    {
      const writeBeforeDeclaration = () => {
        capturedLet = 1;
      };
      expect(() => {
        writeBeforeDeclaration();
      }).toThrow(ReferenceError);
      let capturedLet;
    }
  });

  test("direct assignment before a function-body declaration throws after its rhs", () => {
    let rhsEvaluated = false;

    expect(() => {
      assignedBeforeDeclaration = (rhsEvaluated = true, 1);
      let assignedBeforeDeclaration;
    }).toThrow(ReferenceError);
    expect(rhsEvaluated).toBe(true);
  });

  test("logical assignment reads an uninitialized binding before short-circuiting", () => {
    expect(() => {
      nullishBeforeDeclaration ??= 1;
      let nullishBeforeDeclaration;
    }).toThrow(ReferenceError);

    expect(() => {
      truthyBeforeDeclaration &&= 1;
      let truthyBeforeDeclaration;
    }).toThrow(ReferenceError);
  });

  test("destructuring assignment rejects an uninitialized binding", () => {
    expect(() => {
      [destructuredBeforeDeclaration] = [1];
      let destructuredBeforeDeclaration;
    }).toThrow(ReferenceError);
  });

  test("captured const read before declaration throws", () => {
    {
      const readBeforeDeclaration = () => capturedConst;
      expect(() => {
        readBeforeDeclaration();
      }).toThrow(ReferenceError);
      const capturedConst = 1;
    }
  });

  test("let can be reassigned", () => {
    let x = 1;
    x = 2;
    expect(x).toBe(2);
  });

  test("deeply nested scope shadowing", () => {
    let x = "a";
    {
      let x = "b";
      {
        let x = "c";
        expect(x).toBe("c");
      }
      expect(x).toBe("b");
    }
    expect(x).toBe("a");
  });
});

describe("let temporal dead zone for operands", () => {
  // The values go through a call so the compiler cannot fold the expressions.
  const pass = (value) => value;

  test("an arithmetic read before the declaration in the same block throws", () => {
    expect(() => {
      const early = value + 1;
      let value = pass(2);
      return early;
    }).toThrow(ReferenceError);
  });

  test("the initializer cannot use its own binding as an operand", () => {
    expect(() => {
      let value = value + pass(1);
      return value;
    }).toThrow(ReferenceError);
    expect(() => {
      let value = pass(1) * value;
      return value;
    }).toThrow(ReferenceError);
  });

  test("an inner declaration shadows the outer one from the start of its block", () => {
    expect(() => {
      let value = pass(1);
      {
        const early = value + 0;
        let value = pass(2);
        return early;
      }
    }).toThrow(ReferenceError);
  });

  test("every loop iteration starts with the binding uninitialized", () => {
    const run = (steps) => {
      const results = [];
      for (const step of steps) {
        try {
          results.push(total * 2);
        } catch (error) {
          results.push(error instanceof ReferenceError);
        }
        let total = step + 1;
        total = total + 1;
        results.push(total * 2);
      }
      return results;
    };
    expect(run(pass([0, 1]))).toEqual([true, 4, true, 6]);
  });

  test("a switch clause entered directly does not see an earlier clause's declaration", () => {
    const read = (which) => {
      switch (which) {
        case 0:
          let scale = which + 5;
          return scale * 2;
        case 1:
          return scale * 2;
      }
      return "none";
    };
    expect(read(pass(0))).toBe(10);
    expect(() => read(pass(1))).toThrow(ReferenceError);
  });

  test("a switch clause reached by falling through sees the declaration", () => {
    const read = (which) => {
      switch (which) {
        case 0:
          let scale = which + 5;
        case 1:
          return scale + 1;
      }
      return "none";
    };
    expect(read(pass(0))).toBe(6);
    expect(() => read(pass(1))).toThrow(ReferenceError);
  });

  test("a switch clause entered directly cannot assign an earlier clause's binding", () => {
    const events = [];
    const assign = (which) => {
      switch (which) {
        case 0:
          let slot = which;
          slot = slot + 1;
          return slot;
        case 1:
          slot = (events.push("rhs"), 5);
          return slot;
        case 2:
          slot += 1;
          return slot;
      }
      return "none";
    };
    expect(assign(pass(0))).toBe(1);
    expect(() => assign(pass(1))).toThrow(ReferenceError);
    expect(events).toEqual(["rhs"]);
    expect(() => assign(pass(2))).toThrow(ReferenceError);
  });

  test("a nested switch keeps the outer clause's declaration", () => {
    const read = (outer, inner) => {
      switch (outer) {
        case 0:
          let base = outer + 10;
          switch (inner) {
            case 0:
              let extra = base + 1;
              return extra + base;
            case 1:
              return base * 2;
          }
          return base;
        case 1:
          return base;
      }
      return "none";
    };
    expect(read(pass(0), pass(0))).toBe(21);
    expect(read(pass(0), pass(1))).toBe(20);
    expect(read(pass(0), pass(2))).toBe(10);
    expect(() => read(pass(1), pass(0))).toThrow(ReferenceError);
  });

  test("a closure created before the declaration throws until it runs", () => {
    const run = (seed) => {
      const read = () => value * 2;
      const before = [];
      try {
        read();
      } catch (error) {
        before.push(error instanceof ReferenceError);
      }
      let value = seed;
      value = value + 1;
      return [before, read(), value * 2];
    };
    expect(run(pass(20))).toEqual([[true], 42, 42]);
  });

  test("an assignment after the declaration never throws", () => {
    const run = (seed) => {
      let value = seed;
      value = value + 1;
      value += 1;
      let later;
      later = value * 2;
      return [value, later];
    };
    expect(run(pass(1))).toEqual([3, 6]);
  });

  test("a compound assignment before the declaration throws", () => {
    expect(() => {
      early += pass(1);
      let early = 0;
      return early;
    }).toThrow(ReferenceError);
  });

  test("a try block that throws before its declaration leaves nothing behind", () => {
    const run = (fail) => {
      const seen = [];
      let value = pass(1);
      try {
        if (fail) {
          throw new Error("early");
        }
        let value = pass(10);
        seen.push(value + 1);
      } catch (error) {
        seen.push(value + 2);
      } finally {
        seen.push(value + 3);
      }
      return seen;
    };
    expect(run(pass(true))).toEqual([3, 4]);
    expect(run(pass(false))).toEqual([11, 4]);
  });

  test("a class static block has its own dead zone", () => {
    const results = [];
    class Holder {
      static {
        try {
          results.push(counter + 1);
        } catch (error) {
          results.push(error instanceof ReferenceError);
        }
        let counter = pass(1);
        counter = counter + 1;
        results.push(counter * 2);
      }
    }
    expect(results).toEqual([true, 4]);
    expect(typeof Holder).toBe("function");
  });
});

describe("let bindings as operands", () => {
  const pass = (value) => value;

  test("arithmetic and comparison operands read the current value", () => {
    let a = pass(7);
    let b = pass(3);
    expect(a + b).toBe(10);
    expect(a - b).toBe(4);
    expect(a * b).toBe(21);
    expect(a % b).toBe(1);
    expect(a < b).toBe(false);
    expect(a >= b).toBe(true);
    a = b;
    b = b + 1;
    expect(a + b).toBe(7);
    expect(a - 1).toBe(2);
    expect(1 + b).toBe(5);
    expect(a < b).toBe(true);
    expect(a === b).toBe(false);
  });

  test("the same binding can be both operands", () => {
    let value = pass(6);
    expect(value + value).toBe(12);
    value = value * value;
    expect(value - value).toBe(0);
    expect(value === value).toBe(true);
  });

  test("a binding that changes its type keeps generic operator semantics", () => {
    const run = (seed) => {
      let value = seed;
      const results = [value + 1, value < 5];
      value = "ab";
      results.push(value + 1, value < "b");
      value = 10n;
      results.push(value * value);
      value = { valueOf: () => 4 };
      results.push(value + value, value - 1);
      value = undefined;
      results.push(Number.isNaN(value + 1));
      value = null;
      results.push(value + 1);
      return results;
    };
    expect(run(pass(1))).toEqual([2, true, "ab1", true, 100n, 8, 3, true, 1]);
  });

  test("computed reads and writes use the binding as object, key and value", () => {
    const run = (list, index) => {
      let target = list;
      let key = index;
      let stored = target[key];
      target[key] = stored + 1;
      key = key + 1;
      target[key] = stored;
      stored = target[key] + target[key - 1];
      target = { total: stored };
      target.total = target.total + key;
      return [list, target.total];
    };
    expect(run(pass([10, 20, 30]), pass(0))).toEqual([[11, 10, 30], 22]);
  });

  test("an accumulator updated in a loop", () => {
    const run = (items) => {
      let total = 0;
      let product = 1;
      let longest = "";
      for (const item of items) {
        total = total + item;
        product *= item;
        longest = longest + item;
      }
      return [total, product, longest];
    };
    expect(run(pass([2, 3, 4]))).toEqual([9, 24, "234"]);
  });

  test("a binding captured by a closure keeps its value in each iteration", () => {
    const readers = [];
    for (let step of [1, 2, 3]) {
      let scaled = step * 10;
      readers.push(() => scaled + step);
      scaled = scaled + step;
      step = step + 100;
      expect(scaled + step).toBe((step - 100) * 12 + 100);
    }
    expect(readers.map((read) => read())).toEqual([112, 124, 136]);
  });
});
