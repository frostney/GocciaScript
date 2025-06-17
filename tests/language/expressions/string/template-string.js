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
  expect(multiline).toBe("line 1\nline 2\nline 3");

  const withBackticks = `This has \`backticks\` in it`;
  expect(withBackticks).toBe("This has `backticks` in it");

  const empty = ``;
  expect(empty).toBe("");
});
