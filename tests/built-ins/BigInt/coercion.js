/*---
description: BigInt coercion behavior
features: [bigint]
---*/

test("Boolean coercion", () => {
  expect(Boolean(0n)).toBe(false);
  expect(Boolean(1n)).toBe(true);
  expect(Boolean(-1n)).toBe(true);
});

test("string concatenation with +", () => {
  expect("" + 42n).toBe("42");
  expect(42n + "").toBe("42");
  expect("value: " + 42n).toBe("value: 42");
});

test("typeof BigInt", () => {
  expect(typeof 0n).toBe("bigint");
  expect(typeof 42n).toBe("bigint");
});

test("0n is falsy", () => {
  expect(!0n).toBe(true);
});

test("1n is truthy", () => {
  expect(!!1n).toBe(true);
});

test("0n in conditional is falsy", () => {
  let reached = false;
  if (0n) {
    reached = true;
  }
  expect(reached).toBe(false);
});

test("1n in conditional is truthy", () => {
  let reached = false;
  if (1n) {
    reached = true;
  }
  expect(reached).toBe(true);
});

test("String() explicit conversion", () => {
  expect(String(42n)).toBe("42");
  expect(String(0n)).toBe("0");
  expect(String(-1n)).toBe("-1");
});

test("Number() explicitly converts BigInt", () => {
  expect(Number(1n)).toBe(1);
  expect(Number(-1n)).toBe(-1);
  expect(() => 1n * 1).toThrow();
});

test("Object() boxing", () => {
  const boxed = Object(1n);
  expect(typeof boxed).toBe("object");
  expect(boxed instanceof BigInt).toBe(true);
  expect(boxed.valueOf() === 1n).toBe(true);
  expect(BigInt.prototype.valueOf.call(boxed) === 1n).toBe(true);
  expect(boxed.toString()).toBe("1");
});

test("Object() boxed BigInt in arithmetic", () => {
  expect(Object(1n) - 1n === 0n).toBe(true);
  expect(Object(2n) * 3n === 6n).toBe(true);
});

test("boxed BigInt respects an explicit null prototype", () => {
  const boxed = Object(7n);

  Object.setPrototypeOf(boxed, null);

  expect(Object.getPrototypeOf(boxed)).toBeNull();
  expect(boxed.valueOf).toBeUndefined();
  expect(boxed.toString).toBeUndefined();
});

test("boxed BigInt ordinary ToPrimitive observes prototype accessors once", () => {
  const bigIntValueOf = BigInt.prototype.valueOf;
  const originalToString = Object.getOwnPropertyDescriptor(BigInt.prototype, "toString");
  const originalValueOf = Object.getOwnPropertyDescriptor(BigInt.prototype, "valueOf");
  let toStringGets = 0;
  let valueOfGets = 0;
  let valueOfCalls = 0;
  const valueOfFunction = {
    valueOfReplacement() {
      valueOfCalls++;
      return bigIntValueOf.call(this) * 2n;
    },
  }.valueOfReplacement;

  try {
    Object.defineProperty(BigInt.prototype, "toString", {
      configurable: true,
      get() {
        toStringGets++;
        return undefined;
      },
    });
    Object.defineProperty(BigInt.prototype, "valueOf", {
      configurable: true,
      get() {
        valueOfGets++;
        return valueOfFunction;
      },
    });

    expect("".concat(Object(1n))).toBe("2");
    expect(toStringGets).toBe(1);
    expect(valueOfGets).toBe(1);
    expect(valueOfCalls).toBe(1);
  } finally {
    Object.defineProperty(BigInt.prototype, "toString", originalToString);
    Object.defineProperty(BigInt.prototype, "valueOf", originalValueOf);
  }
});

describe("BigInt from large integral doubles is exact", () => {
  test("values beyond 17 significant decimal digits convert without rounding drift", () => {
    expect(BigInt(9007199254740991475711)).toBe(9007199254740990951424n);
  });

  test("negative large integral doubles convert exactly", () => {
    expect(BigInt(-9007199254740991475711)).toBe(-9007199254740990951424n);
  });
});
