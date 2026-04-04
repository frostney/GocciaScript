/*---
description: afterAll runs once after the tests in its suite
features: [Test Assertions]
---*/

const afterAllEvents = [];

describe("afterAll", () => {
  afterAll(() => {
    afterAllEvents.push("afterAll");
  });

  test("runs the suite test before afterAll", () => {
    afterAllEvents.push("test");
  });
});

test("runs afterAll before the next top-level test executes", () => {
  expect(afterAllEvents).toEqual(["test", "afterAll"]);
});
