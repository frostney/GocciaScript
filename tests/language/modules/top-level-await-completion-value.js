/*---
description: A module with top-level await is evaluated as an async block (ES2026 §27.7.5.2 AsyncBlockStart), so the value its last statement leaves is never adopted as a promise result
features: [top-level-await]
---*/

import { handledValue } from "./helpers/tla-last-statement-handled-rejection.js";
import {
  thenableValue,
  thenCalls,
} from "./helpers/tla-last-statement-thenable.js";

describe("the last statement of a module with top-level await", () => {
  test("a handled rejected promise does not fail the module's evaluation", () => {
    expect(handledValue).toBe("loaded");
  });

  test("a thenable is not called", async () => {
    expect(thenableValue).toBe("loaded");
    await null;
    await null;
    expect(thenCalls).toBe(0);
  });
});
