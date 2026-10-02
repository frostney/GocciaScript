/*---
description: An update expression on a const converts its operand before it throws the assignment TypeError
features: [const, update-expressions]
---*/

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

const topLevelCalls = [];
const topLevelCounter = counting(topLevelCalls);
let topLevelUpdate = "none";
try {
  topLevelCounter++;
} catch (error) {
  topLevelUpdate = error.constructor.name;
}
const topLevelCallsAfterUpdate = topLevelCalls.length;

describe("update expressions on a const", () => {
  test("each form converts a local operand once, then throws TypeError", () => {
    const calls = [];
    const counter = counting(calls);
    const outcomes = [];
    try {
      counter++;
    } catch (error) {
      outcomes.push(error.constructor.name, calls.length);
    }
    try {
      ++counter;
    } catch (error) {
      outcomes.push(error.constructor.name, calls.length);
    }
    try {
      counter--;
    } catch (error) {
      outcomes.push(error.constructor.name, calls.length);
    }
    try {
      --counter;
    } catch (error) {
      outcomes.push(error.constructor.name, calls.length);
    }
    expect(outcomes).toEqual(["TypeError", 1, "TypeError", 2, "TypeError", 3, "TypeError", 4]);
    expect(typeof counter).toBe("object");
  });

  test("an update whose result is used converts once as well", () => {
    const calls = [];
    let result = "unassigned";
    expect(() => {
      const counter = counting(calls);
      result = counter++;
    }).toThrow(TypeError);
    expect(() => {
      const counter = counting(calls);
      result = --counter;
    }).toThrow(TypeError);
    expect(calls.length).toBe(2);
    expect(result).toBe("unassigned");
  });

  test("the conversion asks for a number", () => {
    const hints = [];
    expect(() => {
      const counter = {
        [Symbol.toPrimitive](hint) {
          hints.push(hint);
          return 1;
        },
      };
      counter++;
    }).toThrow(TypeError);
    expect(hints).toEqual(["number"]);
  });

  test("an error thrown by the conversion wins over the assignment TypeError", () => {
    const failing = () => ({
      valueOf() {
        throw new RangeError("conversion failed");
      },
    });
    expect(() => {
      const operand = failing();
      operand++;
    }).toThrow(RangeError);
    expect(() => {
      const operand = failing();
      --operand;
    }).toThrow(RangeError);
  });

  test("a number or BigInt operand throws TypeError", () => {
    expect(() => {
      const count = 1;
      count++;
    }).toThrow(TypeError);
    expect(() => {
      const count = 1n;
      --count;
    }).toThrow(TypeError);
  });

  test("a const in its dead zone throws ReferenceError before any conversion", () => {
    const calls = [];
    expect(() => {
      counter++;
      const counter = counting(calls);
    }).toThrow(ReferenceError);
    expect(calls.length).toBe(0);
  });

  test("a captured const converts once from the closure and from its own function", () => {
    const calls = [];
    const counter = counting(calls);
    const bump = () => {
      counter++;
    };
    expect(errorName(bump)).toBe("TypeError");
    expect(calls.length).toBe(1);
    let own = "none";
    try {
      --counter;
    } catch (error) {
      own = error.constructor.name;
    }
    expect(own).toBe("TypeError");
    expect(calls.length).toBe(2);
  });

  test("a top-level const converts once, directly and from a function", () => {
    expect(topLevelUpdate).toBe("TypeError");
    expect(topLevelCallsAfterUpdate).toBe(1);
    expect(
      errorName(() => {
        ++topLevelCounter;
      }),
    ).toBe("TypeError");
    expect(topLevelCalls.length).toBe(2);
  });
});
