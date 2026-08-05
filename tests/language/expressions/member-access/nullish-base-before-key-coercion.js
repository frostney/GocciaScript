/*---
description: A nullish base throws TypeError before the computed key is coerced to a property key
esid: sec-evaluate-property-access-with-expression-key
features: [optional-chaining, Symbol.toPrimitive]
---*/

// ES2026 §13.3.3 EvaluatePropertyAccessWithExpressionKey stores the *unconverted*
// key value in the Reference Record. §6.2.5.5 GetValue and §6.2.5.6 PutValue then
// run ToObject on the base (step 3.a) *before* ToPropertyKey on the referenced name
// (step 3.c). So a nullish base must throw a TypeError before the key expression's
// result is coerced, and the key's toString/valueOf must never run.
//
// These assertions use an explicit instanceof check rather than relying only on the
// toThrow matcher, so they stay meaningful regardless of matcher strictness.

const throwingKey = () => ({
  toString() {
    throw new Error("key side effect");
  },
});

// Returns { error, coercions } without depending on toThrow.
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

test("computed read on a null base throws TypeError before coercing the key", () => {
  const { error, coercions } = capture((key) => {
    const base = null;
    return base[key];
  });

  expect(error instanceof TypeError).toBe(true);
  expect(coercions).toBe(0);
});

test("computed read on an undefined base throws TypeError before coercing the key", () => {
  const { error, coercions } = capture((key) => {
    const base = undefined;
    return base[key];
  });

  expect(error instanceof TypeError).toBe(true);
  expect(coercions).toBe(0);
});

test("postfix increment on a nullish base throws TypeError before coercing the key", () => {
  const { error, coercions } = capture((key) => {
    const base = null;
    return base[key]++;
  });

  expect(error instanceof TypeError).toBe(true);
  expect(coercions).toBe(0);
});

test("prefix decrement on a nullish base throws TypeError before coercing the key", () => {
  const { error, coercions } = capture((key) => {
    const base = undefined;
    return --base[key];
  });

  expect(error instanceof TypeError).toBe(true);
  expect(coercions).toBe(0);
});

test("computed store on a nullish base throws TypeError before coercing the key", () => {
  const { error, coercions } = capture((key) => {
    const base = null;
    base[key] = 1;
  });

  expect(error instanceof TypeError).toBe(true);
  expect(coercions).toBe(0);
});

test("compound assignment on a nullish base throws TypeError before coercing the key", () => {
  const { error, coercions } = capture((key) => {
    const base = null;
    base[key] += 1;
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

test("delete on a nullish base throws TypeError before coercing the key", () => {
  const { error, coercions } = capture((key) => {
    const base = null;
    delete base[key];
  });

  expect(error instanceof TypeError).toBe(true);
  expect(coercions).toBe(0);
});

test("optional chaining short-circuits without coercing the key", () => {
  let coercions = 0;
  const key = {
    toString() {
      coercions++;
      return "a";
    },
  };
  const base = null;

  expect(base?.[key]).toBeUndefined();
  expect(coercions).toBe(0);
});

test("the key expression still evaluates before the nullish base check", () => {
  // §13.3.3 step 1 evaluates the key expression; only the ToPropertyKey
  // conversion is ordered after the base check.
  let evaluated = false;
  const base = null;
  let error = null;

  try {
    base[((evaluated = true), throwingKey())];
  } catch (e) {
    error = e;
  }

  expect(evaluated).toBe(true);
  expect(error instanceof TypeError).toBe(true);
});

test("a non-nullish base still coerces the computed key", () => {
  let coercions = 0;
  const key = {
    toString() {
      coercions++;
      return "a";
    },
  };

  expect({ a: 1 }[key]).toBe(1);
  expect(coercions).toBe(1);
});

test("a non-nullish base still surfaces a throwing key coercion", () => {
  let error = null;
  try {
    ({})[throwingKey()];
  } catch (e) {
    error = e;
  }

  expect(error instanceof TypeError).toBe(false);
  expect(error.message).toBe("key side effect");
});

test("assignment to a nullish computed target still evaluates the right-hand side first", () => {
  // §13.3.3 NOTE: for `a[b] = c`, ToPropertyKey is not performed until after
  // evaluation of `c`, and the §6.2.5.6 PutValue base check follows it. So the RHS
  // runs even though the store then throws.
  let rhsEvaluations = 0;
  const base = null;
  let error = null;

  try {
    base["a"] = (rhsEvaluations++, 1);
  } catch (e) {
    error = e;
  }

  expect(rhsEvaluations).toBe(1);
  expect(error instanceof TypeError).toBe(true);
});

test("a symbol key on a nullish base throws TypeError without a property lookup", () => {
  const key = Symbol("marker");
  const base = null;
  let error = null;

  try {
    base[key];
  } catch (e) {
    error = e;
  }

  expect(error instanceof TypeError).toBe(true);
});
