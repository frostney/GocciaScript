/*---
description: Basic template literal support
features: [template-literals]
---*/

test("basic template literals", () => {
  const simple = `hello world`;
  expect(simple).toBe("hello world");

  const multiline = `line 1
  line 2
  line 3`;
  expect(multiline).toBe("line 1\n  line 2\n  line 3");

  const withBackticks = `This has \`backticks\` in it`;
  expect(withBackticks).toBe("This has `backticks` in it");

  const empty = ``;
  expect(empty).toBe("");
});

test("escaped dollar-brace produces literal text", () => {
  const x = 42;
  expect(`\${x}`).toBe("${x}");
});

test("escaped backslash before real interpolation", () => {
  const x = 42;
  expect(`\\${x}`).toBe("\\42");
});

test("mixed escaped and real interpolations", () => {
  const a = 1;
  const b = 2;
  expect(`\${a} ${b}`).toBe("${a} 2");
});

test("escaped dollar without brace is literal dollar", () => {
  expect(`price: \$5`).toBe("price: $5");
});

test("triple backslash before dollar-brace is literal backslash plus literal dollar-brace", () => {
  const x = 42;
  expect(`\\\${x}`).toBe("\\${x}");
});

