/*---
description: Thousands of case clauses that each declare a const keep each binding's TDZ, closures and fall-through values
features: [switch-statement, unsafe-function-constructor]
---*/

// All case clauses of a switch share one scope (ES2026 §14.12.2, the
// CaseBlock's LexicallyScopedDeclarations), and a clause can be entered
// without running the clauses before it, so each binding is in its TDZ until
// its own declaration runs. The switches here are generated, so that a
// compiler that tracks the clauses' bindings is checked at scale.

const CLAUSES = 2000;

// A function whose switch has CLAUSES clauses, clause i declaring
// `const c<i> = i * 10` and then the code body(i) returns.
const makeSwitch = (body) => {
  const clauses = Array.from(
    { length: CLAUSES },
    (_, i) => `case ${i}: const c${i} = ${i} * 10; ${body(i)}`,
  );
  return new Function(
    "which",
    `switch (which) {\n${clauses.join("\n")}\n}\nreturn "none";`,
  );
};

describe("many case clauses", () => {
  test("a clause entered directly reads its own binding", () => {
    const read = makeSwitch((i) => `return c${i};`);
    expect(read(0)).toBe(0);
    expect(read(1)).toBe(10);
    expect(read(CLAUSES - 1)).toBe((CLAUSES - 1) * 10);
  });

  test("a clause entered directly sees every earlier clause's binding in its TDZ", () => {
    const read = makeSwitch((i) => (i === 0 ? "return c0;" : `return c${i - 1};`));
    expect(read(0)).toBe(0);
    expect(() => read(1)).toThrow(ReferenceError);
    expect(() => read(CLAUSES / 2)).toThrow(ReferenceError);
    expect(() => read(CLAUSES - 1)).toThrow(ReferenceError);
  });

  test("falling through runs each declaration on the way", () => {
    // No clause returns until the last, so entering at k declares c<k> to
    // the end and leaves the earlier ones in their TDZ.
    const read = makeSwitch((i) =>
      i === CLAUSES - 1 ? `return [c${CLAUSES - 1}, c${CLAUSES - 2}];` : "",
    );
    expect(read(0)).toEqual([(CLAUSES - 1) * 10, (CLAUSES - 2) * 10]);
    expect(read(CLAUSES - 2)).toEqual([(CLAUSES - 1) * 10, (CLAUSES - 2) * 10]);
    expect(() => read(CLAUSES - 1)).toThrow(ReferenceError);
  });

  test("a closure created in a clause captures that clause's binding", () => {
    const capture = makeSwitch((i) => `return () => c${i} + 1;`);
    expect(capture(0)()).toBe(1);
    expect(capture(CLAUSES - 1)()).toBe((CLAUSES - 1) * 10 + 1);
  });

  test("a closure over an earlier clause's binding throws until that declaration runs", () => {
    const capture = makeSwitch((i) => (i === 0 ? "" : `return () => c${i - 1};`));
    // Entered at 0: c0 is declared, then clause 1 returns a closure over it.
    expect(capture(0)()).toBe(0);
    // Entered at 1: the closure reads c0, which was never initialized.
    expect(() => capture(1)()).toThrow(ReferenceError);
  });
});
