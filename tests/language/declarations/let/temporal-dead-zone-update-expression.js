/*---
description: An update expression reads its binding first, so it throws ReferenceError while the binding is in its temporal dead zone
features: [let, const, temporal-dead-zone, update-expressions]
---*/

const errorName = (run) => {
  try {
    run();
    return "none";
  } catch (error) {
    return error.constructor.name;
  }
};

// Everything up to the declarations below runs while they are uninitialized.
let topLevelLetUpdate = "none";
try {
  topLevelLet++;
} catch (error) {
  topLevelLetUpdate = error.constructor.name;
}
let topLevelConstUpdate = "none";
try {
  --topLevelConst;
} catch (error) {
  topLevelConstUpdate = error.constructor.name;
}
const topLevelLetUpdateFromClosure = errorName(() => {
  --topLevelLet;
});
const topLevelConstUpdateFromClosure = errorName(() => {
  topLevelConst++;
});

let topLevelLet = 5;
const topLevelConst = 5;

describe("update expressions in the temporal dead zone", () => {
  test("++ and -- on a let before its declaration throw", () => {
    expect(() => {
      count++;
      let count = 0;
    }).toThrow(ReferenceError);
    expect(() => {
      ++count;
      let count = 0;
    }).toThrow(ReferenceError);
    expect(() => {
      count--;
      let count = 0;
    }).toThrow(ReferenceError);
    expect(() => {
      --count;
      let count = 0;
    }).toThrow(ReferenceError);
  });

  test("an update whose result is used throws as well", () => {
    expect(() => {
      {
        const before = count++;
        let count = 0;
        return before;
      }
    }).toThrow(ReferenceError);
    expect(() => {
      {
        const after = ++count;
        let count = 0;
        return after;
      }
    }).toThrow(ReferenceError);
    expect(() => {
      {
        const before = count--;
        let count = 0;
        return before;
      }
    }).toThrow(ReferenceError);
    expect(() => {
      {
        const after = --count;
        let count = 0;
        return after;
      }
    }).toThrow(ReferenceError);
  });

  test("a const in its dead zone throws ReferenceError, not the assignment TypeError", () => {
    expect(() => {
      fixed++;
      const fixed = 0;
    }).toThrow(ReferenceError);
    expect(() => {
      ++fixed;
      const fixed = 0;
    }).toThrow(ReferenceError);
    expect(() => {
      fixed--;
      const fixed = 0;
    }).toThrow(ReferenceError);
    expect(() => {
      --fixed;
      const fixed = 0;
    }).toThrow(ReferenceError);
  });

  test("an initialized const still throws TypeError", () => {
    expect(() => {
      const fixed = 0;
      fixed++;
    }).toThrow(TypeError);
    expect(() => {
      const fixed = 0;
      --fixed;
    }).toThrow(TypeError);
  });

  test("compound assignment before the declaration throws", () => {
    expect(() => {
      total += 1;
      let total = 0;
    }).toThrow(ReferenceError);
    expect(() => {
      total -= 1;
      let total = 0;
    }).toThrow(ReferenceError);
    expect(() => {
      total = total + 1;
      let total = 0;
    }).toThrow(ReferenceError);
    expect(() => {
      fixed += 1;
      const fixed = 0;
    }).toThrow(ReferenceError);
  });

  test("a switch clause entered directly does not see an earlier clause's declaration", () => {
    const postfix = (which) => {
      switch (which) {
        case 0:
          let count = 10;
        case 1:
          count++;
          return count;
      }
      return "none";
    };
    expect(postfix(0)).toBe(11);
    expect(() => postfix(1)).toThrow(ReferenceError);

    const prefix = (which) => {
      switch (which) {
        case 0:
          let count = 10;
        case 1:
          return --count;
      }
      return "none";
    };
    expect(prefix(0)).toBe(9);
    expect(() => prefix(1)).toThrow(ReferenceError);

    const addOne = (which) => {
      switch (which) {
        case 0:
          let count = 10;
        case 1:
          count = count + 1;
          return count;
      }
      return "none";
    };
    expect(addOne(0)).toBe(11);
    expect(() => addOne(1)).toThrow(ReferenceError);

    const constant = (which) => {
      switch (which) {
        case 0:
          const fixed = 10;
        case 1:
          fixed++;
          return fixed;
      }
      return "none";
    };
    expect(() => constant(0)).toThrow(TypeError);
    expect(() => constant(1)).toThrow(ReferenceError);
  });

  test("every loop iteration starts with the binding uninitialized", () => {
    const outcomes = [];
    for (const step of [0, 1]) {
      outcomes.push(
        errorName(() => {
          captured--;
        }),
      );
      try {
        count++;
        outcomes.push("none");
      } catch (error) {
        outcomes.push(error.constructor.name);
      }
      let count = step;
      let captured = step;
      count++;
      outcomes.push(count);
    }
    expect(outcomes).toEqual([
      "ReferenceError",
      "ReferenceError",
      1,
      "ReferenceError",
      "ReferenceError",
      2,
    ]);
  });

  test("a closure called before the declaration throws until it runs", () => {
    const run = () => {
      const bump = () => count++;
      const bumpFixed = () => {
        fixed++;
      };
      const before = [errorName(bump), errorName(bumpFixed)];
      let count = 5;
      const fixed = 5;
      return [before, bump(), count, errorName(bumpFixed)];
    };
    expect(run()).toEqual([["ReferenceError", "ReferenceError"], 5, 6, "TypeError"]);
  });

  test("a captured binding updated by its own function before the declaration throws", () => {
    expect(() => {
      const read = () => count;
      count++;
      let count = 0;
      return read();
    }).toThrow(ReferenceError);
    expect(() => {
      const read = () => fixed;
      --fixed;
      const fixed = 0;
      return read();
    }).toThrow(ReferenceError);
  });

  test("a class binding and a later parameter are in the dead zone as well", () => {
    expect(() => {
      Shape++;
      class Shape {}
    }).toThrow(ReferenceError);
    const withDefault = (first = second++, second = 1) => [first, second];
    expect(() => withDefault()).toThrow(ReferenceError);
    expect(withDefault(0)).toEqual([0, 1]);
  });

  test("a top-level binding updated before its declaration throws", () => {
    expect(topLevelLetUpdate).toBe("ReferenceError");
    expect(topLevelConstUpdate).toBe("ReferenceError");
    expect(topLevelLetUpdateFromClosure).toBe("ReferenceError");
    expect(topLevelConstUpdateFromClosure).toBe("ReferenceError");
    expect(topLevelLet++).toBe(5);
    expect(topLevelLet).toBe(6);
    expect(topLevelConst).toBe(5);
  });

  test("a failed update leaves the declaration to initialize the binding", () => {
    let caught;
    {
      try {
        count++;
      } catch (error) {
        caught = error;
      }
      let count = 10;
      expect(count).toBe(10);
    }
    expect(caught instanceof ReferenceError).toBe(true);
  });
});

describe("update expressions on an initialized binding", () => {
  test("++ and -- produce the old or the new value", () => {
    let count = 1;
    expect(count++).toBe(1);
    expect(++count).toBe(3);
    expect(count--).toBe(3);
    expect(--count).toBe(1);
    count++;
    --count;
    expect(count).toBe(1);
  });

  test("a let declared without an initializer holds undefined, not the dead zone", () => {
    let postfix;
    let prefix;
    expect(postfix++).toBeNaN();
    expect(--prefix).toBeNaN();
    expect(postfix).toBeNaN();
    expect(prefix).toBeNaN();
  });

  test("an array hole reads as undefined", () => {
    const list = [, 1];
    expect(list[0]++).toBeNaN();
    expect(list[0]).toBeNaN();
    const [missing] = [,];
    let copy = missing;
    expect(--copy).toBeNaN();
  });

  test("a captured binding is updated through the closure and its own function", () => {
    let count = 0;
    const bump = () => ++count;
    expect(bump()).toBe(1);
    expect(count++).toBe(1);
    expect(bump()).toBe(3);
    expect(count).toBe(3);
  });

  test("a binding declared in a loop body is updated in each iteration", () => {
    const results = [];
    for (const step of [1, 2, 3]) {
      let count = step * 10;
      count++;
      results.push(count--, count);
    }
    expect(results).toEqual([11, 10, 21, 20, 31, 30]);
  });
});
