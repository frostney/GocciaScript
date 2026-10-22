/*---
description: Statements and binding patterns nested past the supported depth are a catchable SyntaxError
features: [ShadowRealm, Goccia]
---*/

// Every nested statement counts as a level of nesting, and so does every
// nested binding pattern: blocks, `if` bodies and `else if` chains, function
// bodies, and `[[x]] = ...` patterns. Blocks nested 30,000 deep used to crash
// the process.
const LIMIT_MESSAGE = "Nesting exceeds the supported limit of 1024 levels";
const DEEP = 30000;
const realm = new ShadowRealm();

const expectNestingError = (source) => {
  let error;
  try {
    realm.evaluate(source);
  } catch (caught) {
    error = caught;
  }
  expect(error instanceof SyntaxError).toBe(true);
  expect(error.message.includes(LIMIT_MESSAGE)).toBe(true);
};

const shapes = {
  blocks: [(d) => `let r = 0; ${"{ ".repeat(d)}r = 1;${" }".repeat(d)} r`, 1],
  "else if chains": [(d) => `const v = -1; let r = 0; ${"if (v === 0) {} else ".repeat(d)}{ r = 1; } r`, 1],
  "if bodies": [(d) => `let r = 0; ${"if (true) ".repeat(d)}r = 1; r`, 1],
  "arrow function bodies": [(d) => `typeof (${"() => { return ".repeat(d)}1${"; }".repeat(d)})`, "function"],
  "array binding patterns": [(d) => `const ${"[".repeat(d)}x${"]".repeat(d)} = ${"[".repeat(d)}1${"]".repeat(d)}; x`, 1],
  "object binding patterns": [(d) => `const ${"{ o: ".repeat(d)}x${" }".repeat(d)} = ${"{ o: ".repeat(d)}1${" }".repeat(d)}; x`, 1],
  "destructuring assignments": [(d) => `let x; (${"[".repeat(d)}x${"]".repeat(d)} = ${"[".repeat(d)}1${"]".repeat(d)}); x`, 1],
};

describe("statement and pattern nesting limit", () => {
  for (const [name, [source, expected]] of Object.entries(shapes)) {
    test(`${name} nested 400 deep evaluate`, () => {
      expect(realm.evaluate(source(400))).toBe(expected);
    });

    test(`${name} nested ${DEEP} deep are a SyntaxError`, () => {
      expectNestingError(source(DEEP));
    });
  }

  test("1,024 nested blocks parse and 1,025 do not", () => {
    // The outermost block statement is the first level.
    expect(realm.evaluate(`${"{ ".repeat(1024)}${"} ".repeat(1024)}1`)).toBe(1);
    expectNestingError(`${"{ ".repeat(1025)}${"} ".repeat(1025)}1`);
  });
});
