/*---
description: describe.each expands parameterized suites
features: [Test Assertions]
---*/

const describeEachEvents = [];

describe.each([
  ["one", 1],
  ["two", 2],
])("suite %s", (label, value) => {
  beforeAll(() => {
    describeEachEvents.push("before:" + label);
  });

  test("passes row values into the suite callback", () => {
    describeEachEvents.push("test:" + label);
    expect(value > 0).toBe(true);
  });
});

test("runs each generated suite in registration order", () => {
  expect(describeEachEvents).toEqual([
    "before:one",
    "test:one",
    "before:two",
    "test:two",
  ]);
});
