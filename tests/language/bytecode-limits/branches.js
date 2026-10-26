/*---
description: A branch that jumps further than the bytecode encodes is a catchable SyntaxError, never a jump to the wrong place
features: [ShadowRealm]
---*/

// Conditional jumps hold a signed 16-bit offset. One over a longer branch
// used to wrap around: `if (k)` over 40,000 statements took the body for a
// falsy k, and a switch with 20,000 or more cases returned wrong values,
// looped forever or crashed.
const JUMP_LIMIT = "A branch jumps over more than";
const realm = new ShadowRealm();
const statements = (count) => "r = r + 1; ".repeat(count);
const operands = (count) => Array.from({ length: count }, () => "k").join(", ");
const cases = (count) =>
  Array.from({ length: count }, (_, i) => `case ${i}: return ${i};`).join(" ");

const compileError = (source) => {
  try {
    realm.evaluate(source);
  } catch (error) {
    return error;
  }
  return undefined;
};

const expectJumpLimit = (source) => {
  const error = compileError(source);
  expect(error instanceof SyntaxError).toBe(true);
  expect(error.message.includes(JUMP_LIMIT)).toBe(true);
};

describe("branch length limit", () => {
  test("branches over 10,000 statements run", () => {
    expect(realm.evaluate(`const g = (k) => { let r = 0; if (k) { ${statements(10000)} } return r; }; g(1) + ":" + g(0)`)).toBe("10000:0");
    expect(realm.evaluate(`const g = (k) => k && (${operands(10000)}, 1); g(1) + ":" + g(0)`)).toBe("1:0");
    expect(realm.evaluate(`const g = (k) => k ? (${operands(10000)}, 1) : 2; g(1) + ":" + g(0)`)).toBe("1:2");
  });

  test("a switch with 10,000 cases runs", () => {
    expect(realm.evaluate(`const g = (k) => { switch (k) { ${cases(10000)} } return -1; }; [g(0), g(1), g(9999), g(10000)].join()`)).toBe("0,1,9999,-1");
  });

  test("if, && and ?: over 40,000 statements or operands are a SyntaxError", () => {
    expectJumpLimit(`const g = (k) => { let r = 0; if (k) { ${statements(40000)} } return r; }; g(0)`);
    expectJumpLimit(`const g = (k) => { let r = 0; if (k === 1) { ${statements(40000)} } return r; }; g(0)`);
    expectJumpLimit(`const g = (k) => k && (${operands(40000)}, 1); g(0)`);
    expectJumpLimit(`const g = (k) => k ? (${operands(40000)}, 1) : 2; g(0)`);
  });

  test("a switch with 20,000 cases is a SyntaxError", () => {
    expectJumpLimit(`const g = (k) => { switch (k) { ${cases(20000)} } return -1; }; g(0)`);
  });

  test("for...of and try over 70,000 statements are a SyntaxError", () => {
    expectJumpLimit(`const g = (a) => { let r = 0; for (const v of a) { ${statements(70000)} } return r; }; g([])`);
    expectJumpLimit(`const g = () => { let r = 0; try { ${statements(70000)} } catch (e) { r = -1; } return r; }; g()`);
  });

  test("a do...while over 40,000 statements is a SyntaxError", () => {
    expectJumpLimit(`const g = () => { let r = 0; do { ${statements(40000)} } while (r < 0); return r; }; g()`);
  });
});
