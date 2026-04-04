/*---
description: describe.only focuses execution to the selected suite
features: [Test Assertions]
---*/

const describeOnlyEvents = [];

describe("ordinary suite", () => {
  test("does not run", () => {
    throw new Error("non-focused suite should not run");
  });
});

describe.only("focused suite", () => {
  beforeAll(() => {
    describeOnlyEvents.push("beforeAll");
  });

  test("runs tests in the focused suite", () => {
    describeOnlyEvents.push("test");
    expect(describeOnlyEvents).toEqual(["beforeAll", "test"]);
  });
});

test("skips ordinary top-level tests when describe.only exists", () => {
  throw new Error("top-level test should not run when describe.only is present");
});
