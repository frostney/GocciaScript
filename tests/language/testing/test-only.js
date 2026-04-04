/*---
description: test.only and it.only focus execution to the selected tests
features: [Test Assertions]
---*/

const focusedTests = [];

test("skips ordinary tests before focused ones", () => {
  throw new Error("ordinary test should not run when only is present");
});

test.only("runs the focused test", () => {
  focusedTests.push("test.only");
  expect(focusedTests).toEqual(["test.only"]);
});

it.only("runs focused it aliases too", () => {
  focusedTests.push("it.only");
  expect(focusedTests).toEqual(["test.only", "it.only"]);
});

test("skips ordinary tests after focused ones", () => {
  throw new Error("ordinary test should not run when only is present");
});
