/*---
description: >
  A collection while a module in an import cycle evaluates leaves the cycle's
  bindings intact. Marking a namespace object whose module still has exports
  in their temporal dead zone neither raises the ReferenceError a read of them
  would, nor stops the module that collected from evaluating.
features: [modules]
---*/

import { collectedDuringEvaluation, readA, valueA } from "./helpers/collection-cycle-a.js";

describe("a collection during an import cycle", () => {
  test("the module that collected evaluated to the end", () => {
    expect(collectedDuringEvaluation).toBe(true);
  });

  test("the cycle's exports are initialized once their module ran", () => {
    expect(valueA).toBe("from A");
    expect(readA()).toEqual({
      value: "from A",
      counter: 2,
      default: "default from A",
      classLabel: "class A",
    });
  });

  test("the cycle's bindings survive a later collection", () => {
    Goccia.gc();
    expect(readA().value).toBe("from A");
  });
});
