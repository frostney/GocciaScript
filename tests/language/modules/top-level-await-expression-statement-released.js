/*---
description: >
  A top-level expression statement that awaited a value does not keep it
  reachable while the module stays suspended on a later await (ES2026 §16.2.1.5.3.4
  ExecuteAsyncModule; §27.7.5.3 Await): the registers above the live bindings
  are cleared after the statement, as they are for a statement in a function.
features: [top-level-await, Goccia.gc]
---*/

import { exportedGrown, exportedLength, grown, size } from "./helpers/tla-awaited-argument-released.js";

describe("an awaited value at the top level of a module", () => {
  test("is reclaimed once its statement has finished", () => {
    expect(grown).toBeLessThan(size / 2);
  });

  test("is reclaimed once its exported declaration has finished", () => {
    expect(exportedLength).toBe(size);
    expect(exportedGrown).toBeLessThan(size / 2);
  });
});
