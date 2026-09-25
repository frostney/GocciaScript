/*---
description: Hoisted async function declarations propagate a callee's throw after resuming from await
features: [modules, compat-function]
---*/

import {
  boom,
  catchesAfterAwait,
  throwsAfterAwait,
  throwsBeforeAwait,
} from "./helpers/hoisted-async-throw.js";

describe("hoisted async function declarations propagate callee throws", () => {
  test("rejects with the thrown value when the throw follows an await", async () => {
    let caught = null;
    try {
      await throwsAfterAwait();
    } catch (error) {
      caught = error;
    }
    expect(caught).toBe(boom);
    expect(caught.name).toBe("Boom");
    expect(caught.message).toBe("boom");
  });

  test("rejects with the thrown value when the throw precedes any await", async () => {
    let caught = null;
    try {
      await throwsBeforeAwait();
    } catch (error) {
      caught = error;
    }
    expect(caught).toBe(boom);
    expect(caught.message).toBe("boom");
  });

  test("catch inside the resumed body binds the thrown value itself", async () => {
    const caught = await catchesAfterAwait();
    expect(caught).toBe(boom);
    expect(caught.name).toBe("Boom");
    expect(caught.message).toBe("boom");
  });
});
