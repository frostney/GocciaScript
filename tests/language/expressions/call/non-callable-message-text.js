/*---
description: A non-callable callee and a nullish property base are reported with the same message text in both execution modes
esid: sec-function-calls-runtime-semantics-evaluation
---*/

// ES2026 §13.3.6.1 step 6 (throw a TypeError when the callee is not callable)
// and §6.2.5.5 GetValue step 3.a (ToObject on a nullish base) say only *that*
// a TypeError is thrown, not what it says. The message is therefore an engine
// choice — and it used to be a different choice per execution mode: the
// tree-walk evaluator named the callee off the AST while the bytecode VM only
// had the runtime value, so `obj.missingMethod()` read
// "obj.missingMethod is not a function" interpreted and
// "undefined is not a function" compiled. Nullish property reads diverged the
// same way ("Cannot read property 'x' of undefined" vs Node's
// "Cannot read properties of undefined (reading 'x')").
//
// Both modes now build these strings from one shared formatter, so this file
// pins the text itself: it fails in whichever mode drifts.

const messageOf = (fn) => {
  try {
    fn();
  } catch (e) {
    return e.message;
  }
  return "<did not throw>";
};

describe("non-callable callee messages", () => {
  test("names a member callee as written", () => {
    const obj = {};
    expect(messageOf(() => obj.missingMethod())).toBe(
      "obj.missingMethod is not a function",
    );
  });

  test("names a computed member callee as written", () => {
    const obj = {};
    const key = "missing";
    expect(messageOf(() => obj[key]())).toBe("obj[key] is not a function");
  });

  test("names a member callee on a primitive receiver", () => {
    const num = 5;
    expect(messageOf(() => num.foo())).toBe("num.foo is not a function");
  });

  test("names a chained member callee in full", () => {
    const o = { a: {} };
    expect(messageOf(() => o.a.b())).toBe("o.a.b is not a function");
  });

  test("names an identifier callee without quoting it", () => {
    const notFn = 3;
    expect(messageOf(() => notFn())).toBe("notFn is not a function");
  });

  test("names an optional member callee including the ?. token", () => {
    const obj = {};
    expect(messageOf(() => obj?.missing())).toBe(
      "obj?.missing is not a function",
    );
  });

  test("names a spread call's callee", () => {
    const obj = {};
    const args = [1, 2];
    expect(messageOf(() => obj.missingMethod(...args))).toBe(
      "obj.missingMethod is not a function",
    );
  });

  test("falls back to the value type when the callee is an expression", () => {
    expect(messageOf(() => (3)())).toBe("3 is not a function");
  });
});

describe("non-constructor callee messages", () => {
  test("names an identifier constructor as written", () => {
    const und = undefined;
    expect(messageOf(() => new und())).toBe("und is not a constructor");
  });

  test("names a member constructor as written", () => {
    const obj = {};
    expect(messageOf(() => new obj.Missing())).toBe(
      "obj.Missing is not a constructor",
    );
  });
});

describe("nullish property access messages", () => {
  test("uses the 'Cannot read properties of undefined' form", () => {
    const und = undefined;
    expect(messageOf(() => und.x)).toBe(
      "Cannot read properties of undefined (reading 'x')",
    );
  });

  test("uses the 'Cannot read properties of null' form", () => {
    const nul = null;
    expect(messageOf(() => nul.x)).toBe(
      "Cannot read properties of null (reading 'x')",
    );
  });

  test("reports the missing property of an intermediate undefined", () => {
    const obj = {};
    expect(messageOf(() => obj.missing.deeper)).toBe(
      "Cannot read properties of undefined (reading 'deeper')",
    );
  });

  test("reports a computed key on a nullish base", () => {
    const nul = null;
    const key = "missing";
    expect(messageOf(() => nul[key])).toBe(
      "Cannot read properties of null (reading 'missing')",
    );
  });
});

describe("unresolved identifier messages", () => {
  test("uses the 'is not defined' form", () => {
    expect(messageOf(() => missingGlobalBinding)).toBe(
      "missingGlobalBinding is not defined",
    );
  });
});
