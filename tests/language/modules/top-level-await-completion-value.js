/*---
description: A module with top-level await is evaluated as an async block (ES2026 §27.7.5.2 AsyncBlockStart), so the value its last statement leaves is never adopted as a promise result
features: [top-level-await]
---*/

import { handledValue } from "./helpers/tla-last-statement-handled-rejection.js";
import {
  thenableValue,
  thenCalls,
} from "./helpers/tla-last-statement-thenable.js";
import { afterDependencyValue } from "./helpers/tla-after-async-dependency-handled-rejection.js";
import {
  afterDependencyThenableValue,
  afterDependencyThenCalls,
} from "./helpers/tla-after-async-dependency-thenable.js";

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

  test("after an asynchronous dependency, a handled rejected promise does not fail the module", () => {
    expect(afterDependencyValue).toBe("loaded");
  });

  test("after an asynchronous dependency, a thenable is not called", async () => {
    expect(afterDependencyThenableValue).toBe("loaded");
    await null;
    await null;
    expect(afterDependencyThenCalls).toBe(0);
  });

  test("a dynamic import of such a module resolves to its namespace", async () => {
    const namespace =
      await import("./helpers/tla-after-async-dependency-handled-rejection.js");
    expect(namespace.afterDependencyValue).toBe("loaded");
  });

  test("a last value that is a promise then saw does not fail the module", async () => {
    const originalThen = Promise.prototype.then;
    Promise.prototype.then = {
      then(onFulfilled, onRejected) {
        globalThis.capturedByThen = this;
        return originalThen.call(this, onFulfilled, onRejected);
      },
    }.then;
    let namespace;
    try {
      namespace =
        await import("./helpers/tla-last-statement-captured-promise.js");
    } finally {
      Promise.prototype.then = originalThen;
    }
    expect(namespace.capturedValue).toBe("loaded");
  });
});
