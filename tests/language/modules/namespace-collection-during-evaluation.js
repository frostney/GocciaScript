/*---
description: >
  A collection while a module is still evaluating, with that module's
  namespace object alive, does not read its uninitialized exports.
features: [Goccia.gc]
---*/

import {
  value,
  readThroughNamespace,
} from "./helpers/self-namespace-collect.js";

describe("a collection while a module with a namespace object evaluates", () => {
  test("the module finishes evaluating", () => {
    expect(value).toBe("initialized");
  });

  test("its namespace object reads the initialized export", () => {
    expect(readThroughNamespace()).toBe("initialized");
  });
});
