/*---
description: Non-strict computed writes on a nullish base still throw TypeError before coercing the key
esid: sec-putvalue
features: [compat-non-strict-mode]
---*/

// Sloppy mode relaxes only ES2026 §6.2.5.6 PutValue step 3.e (a *failed* assignment
// throws only when [[Strict]] is true). Step 3.a — ToObject on the base — is
// unconditional, and it runs before step 3.c converts the referenced name with
// ToPropertyKey. So a nullish base must still throw a TypeError here, and the key's
// toString must never run.
//
// This file exists to cover the sloppy-only store path (OP_SET_INDEX_LOOSE in the
// bytecode VM), which the default strict test profile never reaches.

const capture = (fn) => {
  let coercions = 0;
  const key = {
    toString() {
      coercions++;
      throw new Error("key side effect");
    },
  };
  let error = null;
  try {
    fn(key);
  } catch (e) {
    error = e;
  }
  return { error, coercions };
};

describe("non-strict nullish-base computed access", () => {
  test("computed write on a null base throws TypeError before coercing the key", () => {
    const { error, coercions } = capture((key) => {
      const base = null;
      base[key] = 1;
    });

    expect(error instanceof TypeError).toBe(true);
    expect(coercions).toBe(0);
  });

  test("computed write on an undefined base throws TypeError before coercing the key", () => {
    const { error, coercions } = capture((key) => {
      const base = undefined;
      base[key] = 1;
    });

    expect(error instanceof TypeError).toBe(true);
    expect(coercions).toBe(0);
  });

  test("destructuring into a nullish computed target throws TypeError before coercing the key", () => {
    const { error, coercions } = capture((key) => {
      const base = null;
      [base[key]] = [1];
    });

    expect(error instanceof TypeError).toBe(true);
    expect(coercions).toBe(0);
  });

  test("computed read on a nullish base throws TypeError before coercing the key", () => {
    const { error, coercions } = capture((key) => {
      const base = null;
      return base[key];
    });

    expect(error instanceof TypeError).toBe(true);
    expect(coercions).toBe(0);
  });

  test("increment on a nullish base throws TypeError before coercing the key", () => {
    const { error, coercions } = capture((key) => {
      const base = null;
      base[key]++;
    });

    expect(error instanceof TypeError).toBe(true);
    expect(coercions).toBe(0);
  });

  test("compound assignment on a nullish base throws TypeError before coercing the key", () => {
    const { error, coercions } = capture((key) => {
      const base = undefined;
      base[key] += 1;
    });

    expect(error instanceof TypeError).toBe(true);
    expect(coercions).toBe(0);
  });

  test("the nullish write message names the operation and the base", () => {
    const { error } = capture((key) => {
      const base = null;
      base[key] = 1;
    });

    expect(error.message).toBe(
      "Cannot set properties of null (setting '<computed>')"
    );
  });

  test("a non-nullish base still writes and still coerces the key", () => {
    let coercions = 0;
    const key = {
      toString() {
        coercions++;
        return "a";
      },
    };
    const target = {};

    target[key] = 1;

    expect(target.a).toBe(1);
    expect(coercions).toBe(1);
  });
});
