/*---
description: An unresolvable reference throws the intrinsic ReferenceError, whatever the global ReferenceError binding holds
features: [identifiers, ReferenceError]
---*/

// ES2026 §6.2.5.5 GetValue step 2 and §6.2.5.6 PutValue step 2.a throw a
// ReferenceError for an unresolvable reference: the realm's intrinsic
// %ReferenceError%, not whatever the global `ReferenceError` names.
const IntrinsicReferenceError = ReferenceError;

const withReplacedReferenceError = (run) => {
  globalThis.ReferenceError = class FakeReferenceError {
    constructor(message) {
      this.fake = message;
    }
  };
  try {
    return run();
  } finally {
    globalThis.ReferenceError = IntrinsicReferenceError;
  }
};

const caughtFrom = (run) => {
  try {
    run();
  } catch (error) {
    return error;
  }
  return undefined;
};

test("reading an undeclared name throws the intrinsic ReferenceError", () => {
  const error = withReplacedReferenceError(() =>
    caughtFrom(() => undeclaredReadTarget));
  expect(error instanceof IntrinsicReferenceError).toBe(true);
  expect(error.fake).toBeUndefined();
  expect(error.message).toBe("undeclaredReadTarget is not defined");
});

test("assigning an undeclared name throws the intrinsic ReferenceError", () => {
  const error = withReplacedReferenceError(() =>
    caughtFrom(() => {
      undeclaredAssignTarget = 1;
    }));
  expect(error instanceof IntrinsicReferenceError).toBe(true);
  expect(error.fake).toBeUndefined();
  expect(error.message).toBe("undeclaredAssignTarget is not defined");
});

test("compound-assigning an undeclared name throws the intrinsic ReferenceError", () => {
  const error = withReplacedReferenceError(() =>
    caughtFrom(() => {
      undeclaredCompoundTarget += 1;
    }));
  expect(error instanceof IntrinsicReferenceError).toBe(true);
  expect(error.fake).toBeUndefined();
  expect(error.message).toBe("undeclaredCompoundTarget is not defined");
});

test("logical-assigning an undeclared name throws the intrinsic ReferenceError", () => {
  const error = withReplacedReferenceError(() =>
    caughtFrom(() => {
      undeclaredLogicalTarget ??= 1;
    }));
  expect(error instanceof IntrinsicReferenceError).toBe(true);
  expect(error.message).toBe("undeclaredLogicalTarget is not defined");
});

test("typeof an undeclared name still reads undefined", () => {
  expect(typeof undeclaredTypeofTarget).toBe("undefined");
});

test("the ReferenceError names the unresolved identifier", () => {
  const error = caughtFrom(() => undeclaredPlainTarget);
  expect(error.name).toBe("ReferenceError");
  expect(error.message).toBe("undeclaredPlainTarget is not defined");
});
