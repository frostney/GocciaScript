/*---
description: A compound assignment to a const applies its operator before it throws the assignment TypeError
features: [const, compound-assignment-operators, logical-assignment-operators, Symbol.toPrimitive]
---*/

// ES2026 §13.15.2: GetValue(lRef), the right-hand side and
// ApplyStringOrNumericBinaryOperator all run before PutValue rejects the
// immutable binding, so their side effects and their errors are observable.

const counting = (calls) => ({
  valueOf() {
    calls.push("valueOf");
    return 1;
  },
});

const errorName = (run) => {
  try {
    run();
    return "none";
  } catch (error) {
    return error.constructor.name;
  }
};

const allOperatorsOnLocal = (calls) => [
  () => {
    const c = counting(calls);
    c += 1;
  },
  () => {
    const c = counting(calls);
    c -= 1;
  },
  () => {
    const c = counting(calls);
    c *= 1;
  },
  () => {
    const c = counting(calls);
    c /= 1;
  },
  () => {
    const c = counting(calls);
    c %= 1;
  },
  () => {
    const c = counting(calls);
    c **= 1;
  },
  () => {
    const c = counting(calls);
    c <<= 1;
  },
  () => {
    const c = counting(calls);
    c >>= 1;
  },
  () => {
    const c = counting(calls);
    c >>>= 1;
  },
  () => {
    const c = counting(calls);
    c &= 1;
  },
  () => {
    const c = counting(calls);
    c |= 1;
  },
  () => {
    const c = counting(calls);
    c ^= 1;
  },
];

const topLevelCalls = [];
const topLevelTarget = counting(topLevelCalls);
const topLevelOutcomes = [];
try {
  topLevelTarget += 1;
} catch (error) {
  topLevelOutcomes.push(error.constructor.name);
}
try {
  topLevelTarget -= 1;
} catch (error) {
  topLevelOutcomes.push(error.constructor.name);
}
try {
  topLevelTarget *= 1;
} catch (error) {
  topLevelOutcomes.push(error.constructor.name);
}
try {
  topLevelTarget /= 1;
} catch (error) {
  topLevelOutcomes.push(error.constructor.name);
}
try {
  topLevelTarget %= 1;
} catch (error) {
  topLevelOutcomes.push(error.constructor.name);
}
try {
  topLevelTarget **= 1;
} catch (error) {
  topLevelOutcomes.push(error.constructor.name);
}
try {
  topLevelTarget <<= 1;
} catch (error) {
  topLevelOutcomes.push(error.constructor.name);
}
try {
  topLevelTarget >>= 1;
} catch (error) {
  topLevelOutcomes.push(error.constructor.name);
}
try {
  topLevelTarget >>>= 1;
} catch (error) {
  topLevelOutcomes.push(error.constructor.name);
}
try {
  topLevelTarget &= 1;
} catch (error) {
  topLevelOutcomes.push(error.constructor.name);
}
try {
  topLevelTarget |= 1;
} catch (error) {
  topLevelOutcomes.push(error.constructor.name);
}
try {
  topLevelTarget ^= 1;
} catch (error) {
  topLevelOutcomes.push(error.constructor.name);
}
const topLevelCallsAfterDirect = topLevelCalls.length;

describe("compound assignment to an initialized const", () => {
  test("every arithmetic and bitwise operator converts a local operand once, then throws TypeError", () => {
    const calls = [];
    const outcomes = allOperatorsOnLocal(calls).map(errorName);
    expect(outcomes).toEqual(new Array(12).fill("TypeError"));
    expect(calls.length).toBe(12);
  });

  test("the target is read, then the right-hand side, then the operator runs", () => {
    const log = [];
    expect(() => {
      const target = {
        valueOf() {
          log.push("target");
          return 1;
        },
      };
      target += {
        valueOf() {
          log.push("value");
          return 2;
        },
      };
    }).toThrow(TypeError);
    expect(log).toEqual(["target", "value"]);
  });

  test("the operator converts with its own hint", () => {
    const hints = [];
    const target = () => ({
      [Symbol.toPrimitive](hint) {
        hints.push(hint);
        return 1;
      },
    });
    expect(() => {
      const operand = target();
      operand += 1;
    }).toThrow(TypeError);
    expect(() => {
      const operand = target();
      operand -= 1;
    }).toThrow(TypeError);
    expect(() => {
      const operand = target();
      operand |= 1;
    }).toThrow(TypeError);
    expect(hints).toEqual(["default", "number", "number"]);
  });

  test("string concatenation converts the operand with toString", () => {
    const calls = [];
    expect(() => {
      const text = {
        valueOf: undefined,
        toString() {
          calls.push("toString");
          return "a";
        },
      };
      text += "b";
    }).toThrow(TypeError);
    expect(calls).toEqual(["toString"]);
  });

  test("an error thrown by the conversion wins over the assignment TypeError", () => {
    expect(() => {
      const operand = {
        valueOf() {
          throw new RangeError("conversion failed");
        },
      };
      operand -= 1;
    }).toThrow(RangeError);
  });

  test("an error thrown by the right-hand side wins over the assignment TypeError", () => {
    expect(() => {
      const operand = 1;
      operand *= (() => {
        throw new RangeError("right-hand side failed");
      })();
    }).toThrow(RangeError);
  });

  test("mixing BigInt and Number throws the operator's TypeError, not the assignment's", () => {
    let assignmentMessage = "none";
    try {
      const big = 1n;
      big = 2n;
    } catch (error) {
      assignmentMessage = error.message;
    }
    let mixMessage = "none";
    try {
      const big = 1n;
      big += 1;
    } catch (error) {
      expect(error instanceof TypeError).toBe(true);
      mixMessage = error.message;
    }
    expect(mixMessage).not.toBe("none");
    expect(mixMessage).not.toBe(assignmentMessage);
  });

  test("the failed assignment leaves the const and the enclosing result unchanged", () => {
    const fixed = 1;
    let result = "unassigned";
    try {
      result = fixed += 5;
    } catch (error) {
      expect(error instanceof TypeError).toBe(true);
    }
    expect(result).toBe("unassigned");
    expect(fixed).toBe(1);
  });

  test("a captured const converts once from the closure", () => {
    const calls = [];
    const target = counting(calls);
    const outcomes = [
      () => {
        target += 1;
      },
      () => {
        target **= 1;
      },
      () => {
        target >>>= 1;
      },
      () => {
        target ^= 1;
      },
    ].map(errorName);
    expect(outcomes).toEqual(["TypeError", "TypeError", "TypeError", "TypeError"]);
    expect(calls.length).toBe(4);
  });

  test("a top-level const converts once for every operator, directly and from a function", () => {
    expect(topLevelOutcomes).toEqual(new Array(12).fill("TypeError"));
    expect(topLevelCallsAfterDirect).toBe(12);
    expect(
      errorName(() => {
        topLevelTarget %= 1;
      }),
    ).toBe("TypeError");
    expect(topLevelCalls.length).toBe(13);
  });

  test("a class binding converts once inside its own class body", () => {
    const calls = [];
    class Shape {
      static valueOf() {
        calls.push("valueOf");
        return 1;
      }
      static fromField = errorName(() => {
        Shape += 1;
      });
      static {
        this.fromBlock = errorName(() => {
          Shape -= 1;
        });
      }
      static fromMethod() {
        try {
          Shape <<= 1;
          return "none";
        } catch (error) {
          return error.constructor.name;
        }
      }
    }
    expect([Shape.fromField, Shape.fromBlock, Shape.fromMethod()]).toEqual([
      "TypeError",
      "TypeError",
      "TypeError",
    ]);
    expect(calls.length).toBe(3);
  });
});

describe("logical assignment to an initialized const", () => {
  test("a short-circuited ||=, &&= or ??= does not assign and does not throw", () => {
    const log = [];
    const truthy = 1;
    const falsy = 0;
    const present = "value";
    truthy ||= (log.push("||="), 2);
    falsy &&= (log.push("&&="), 2);
    present ??= (log.push("??="), 2);
    expect([truthy, falsy, present]).toEqual([1, 0, "value"]);
    expect(log).toEqual([]);
  });

  test("an ||= that assigns evaluates the right-hand side, then throws TypeError", () => {
    const log = [];
    expect(() => {
      const falsy = 0;
      falsy ||= (log.push("value"), 2);
    }).toThrow(TypeError);
    expect(log).toEqual(["value"]);
  });

  test("an &&= that assigns evaluates the right-hand side, then throws TypeError", () => {
    const log = [];
    expect(() => {
      const truthy = 1;
      truthy &&= (log.push("value"), 2);
    }).toThrow(TypeError);
    expect(log).toEqual(["value"]);
  });

  test("a ??= that assigns evaluates the right-hand side, then throws TypeError", () => {
    const log = [];
    expect(() => {
      const missing = null;
      missing ??= (log.push("value"), 2);
    }).toThrow(TypeError);
    expect(log).toEqual(["value"]);
  });

  test("a captured const follows the same short-circuit", () => {
    const truthy = 1;
    const missing = undefined;
    expect(
      errorName(() => {
        truthy ||= 2;
      }),
    ).toBe("none");
    expect(
      errorName(() => {
        missing ??= 2;
      }),
    ).toBe("TypeError");
  });
});

describe("compound assignment to a const in its temporal dead zone", () => {
  test("an arithmetic operator throws ReferenceError before the right-hand side runs", () => {
    const log = [];
    expect(() => {
      target **= (log.push("value"), 2);
      const target = 1;
    }).toThrow(ReferenceError);
    expect(log).toEqual([]);
  });

  test("a logical operator throws ReferenceError before the right-hand side runs", () => {
    const log = [];
    expect(() => {
      target ??= (log.push("value"), 2);
      const target = 1;
    }).toThrow(ReferenceError);
    expect(() => {
      target ||= (log.push("value"), 2);
      const target = 0;
    }).toThrow(ReferenceError);
    expect(log).toEqual([]);
  });

  test("a captured const throws ReferenceError from a closure called early", () => {
    const run = () => {
      const bump = () => {
        target &= 1;
      };
      const before = errorName(bump);
      const target = 1;
      return [before, errorName(bump)];
    };
    expect(run()).toEqual(["ReferenceError", "TypeError"]);
  });
});
