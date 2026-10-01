/*---
description: Reads of a top-level const from functions and classes, before and after its initialization
features: [const-declaration, temporal-dead-zone]
---*/

const outcome = (read) => {
  try {
    return read();
  } catch (error) {
    return error.constructor.name;
  }
};

// Everything up to the declarations below runs while they are uninitialized.
const readLimitEarly = () => LIMIT;
const readTypeofEarly = () => typeof LIMIT;
class EarlyReader {
  read() {
    return LIMIT;
  }
}
const limitBeforeInitialization = outcome(readLimitEarly);
const typeofBeforeInitialization = outcome(readTypeofEarly);
const methodBeforeInitialization = outcome(() => new EarlyReader().read());
const directBeforeInitialization = outcome(() => LIMIT);

const LIMIT = 16;
const OFFSET = 3;
const LABEL = "limit";
const ENABLED = false;
const NOTHING = null;
const MISSING = undefined;
const NEGATIVE_ZERO = -0;
const NOT_A_NUMBER = NaN;
const UNBOUNDED = Infinity;
const FRACTION = 0.5;
const LARGE = 4294967296;
const HUGE = 12345678901234567890n;
const DERIVED = LIMIT * 2 + OFFSET;
const DERIVED_LABEL = LABEL + ":" + LIMIT;

describe("top-level const read", () => {
  test("a read before initialization throws, whatever compiled it", () => {
    expect(limitBeforeInitialization).toBe("ReferenceError");
    expect(typeofBeforeInitialization).toBe("ReferenceError");
    expect(methodBeforeInitialization).toBe("ReferenceError");
    expect(directBeforeInitialization).toBe("ReferenceError");
  });

  test("the same early readers return the value after initialization", () => {
    expect(readLimitEarly()).toBe(16);
    expect(readTypeofEarly()).toBe("number");
    expect(new EarlyReader().read()).toBe(16);
  });

  test("arrow functions and nested closures read it", () => {
    const read = () => LIMIT;
    const nested = () => () => () => LIMIT + OFFSET;
    expect(read()).toBe(16);
    expect(nested()()()).toBe(19);
  });

  test("class methods, accessors, static fields and field initializers read it", () => {
    class Reader {
      static shared = LIMIT;
      own = OFFSET;
      method() {
        return LIMIT;
      }
      get label() {
        return LABEL;
      }
      static create() {
        return LIMIT + OFFSET;
      }
    }
    const reader = new Reader();
    expect(Reader.shared).toBe(16);
    expect(reader.own).toBe(3);
    expect(reader.method()).toBe(16);
    expect(reader.label).toBe("limit");
    expect(Reader.create()).toBe(19);
  });

  test("every primitive kind keeps its value", () => {
    const read = () => [
      LIMIT,
      LABEL,
      ENABLED,
      NOTHING,
      MISSING,
      NEGATIVE_ZERO,
      NOT_A_NUMBER,
      UNBOUNDED,
      FRACTION,
      LARGE,
    ];
    const values = read();
    expect(values[0]).toBe(16);
    expect(values[1]).toBe("limit");
    expect(values[2]).toBe(false);
    expect(values[3]).toBe(null);
    expect(values[4]).toBe(undefined);
    expect(Object.is(values[5], -0)).toBe(true);
    expect(Number.isNaN(values[6])).toBe(true);
    expect(values[7]).toBe(Infinity);
    expect(values[8]).toBe(0.5);
    expect(values[9]).toBe(4294967296);
    expect((() => HUGE)()).toBe(12345678901234567890n);
    expect((() => HUGE + 1n)()).toBe(12345678901234567891n);
  });

  test("a const initialized from other consts keeps its computed value", () => {
    expect((() => DERIVED)()).toBe(35);
    expect((() => DERIVED_LABEL)()).toBe("limit:16");
    expect((() => `${LABEL}=${LIMIT}`)()).toBe("limit=16");
  });

  test("typeof and member access see the value", () => {
    expect((() => typeof LIMIT)()).toBe("number");
    expect((() => typeof LABEL)()).toBe("string");
    expect((() => typeof NOTHING)()).toBe("object");
    expect((() => typeof MISSING)()).toBe("undefined");
    expect((() => typeof HUGE)()).toBe("bigint");
    expect((() => LIMIT.toFixed(1))()).toBe("16.0");
    expect((() => LABEL.length)()).toBe(5);
  });

  test("a branch on a false const takes the other path", () => {
    const pick = () => {
      if (ENABLED) {
        return "enabled";
      }
      return "disabled";
    };
    expect(pick()).toBe("disabled");
    expect((() => (ENABLED ? 1 : 2))()).toBe(2);
    expect((() => ENABLED || LIMIT)()).toBe(16);
    expect((() => NOTHING ?? LABEL)()).toBe("limit");
  });

  test("assignment still throws and leaves the value unchanged", () => {
    expect(() => {
      LIMIT = 5;
    }).toThrow(TypeError);
    expect(() => {
      LIMIT += 1;
    }).toThrow(TypeError);
    expect(() => {
      LIMIT++;
    }).toThrow(TypeError);
    expect((() => LIMIT)()).toBe(16);
  });

  test("an inner declaration of the same name shadows it", () => {
    const inner = () => {
      const LIMIT = 5;
      return LIMIT;
    };
    const parameter = (LIMIT) => LIMIT;
    const mutable = () => {
      let LIMIT = 1;
      LIMIT = LIMIT + 1;
      return LIMIT;
    };
    expect(inner()).toBe(5);
    expect(parameter(7)).toBe(7);
    expect(mutable()).toBe(2);
    expect((() => LIMIT)()).toBe(16);
  });

  test("an inner declaration of the same name has its own dead zone", () => {
    const readInnerEarly = () => {
      const early = outcome(() => LIMIT);
      const LIMIT = 5;
      return [early, LIMIT];
    };
    expect(readInnerEarly()).toEqual(["ReferenceError", 5]);

    {
      const readBlockEarly = () => LIMIT;
      const early = outcome(readBlockEarly);
      const LIMIT = 7;
      expect([early, readBlockEarly()]).toEqual(["ReferenceError", 7]);
    }
  });

  test("it is not a property of the global object", () => {
    expect(globalThis.LIMIT).toBe(undefined);
    globalThis.LIMIT = 99;
    try {
      expect((() => LIMIT)()).toBe(16);
    } finally {
      delete globalThis.LIMIT;
    }
  });
});
