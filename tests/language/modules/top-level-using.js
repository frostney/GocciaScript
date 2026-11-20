/*---
description: A module disposes its top-level using and await using resources once its body completes (ES2027 §16.2.1.7.3.2 ExecuteModule, §7.5.4 DisposeResources)
features: [explicit-resource-management, modules, top-level-await]
---*/

import { log, read } from "../../../fixtures/modules/top-level-using.js";

const entryLog = [];

// A closure the body creates before the declaration runs.
const readEntrySync = () => entrySync.name;
let earlyRead;
try {
  readEntrySync();
} catch (e) {
  earlyRead = e;
}

using entrySync = {
  name: "entry",
  [Symbol.dispose]() {
    entryLog.push("dispose sync");
  },
};
await using entryAsync = {
  async [Symbol.asyncDispose]() {
    await null;
    entryLog.push("dispose async");
  },
};

entryLog.push("body");

describe("a using declaration at a module's top level", () => {
  test("an imported module disposes in reverse order after its body", () => {
    expect(log).toEqual(["body", "dispose sync", "dispose async"]);
  });

  test("the binding stays readable after its disposal", () => {
    expect(read()).toBe("still bound");
  });

  test("a closure created before the declaration reads the binding once it runs", () => {
    expect(earlyRead instanceof ReferenceError).toBe(true);
    expect(readEntrySync()).toBe("entry");
  });

  test("the entry module disposes before its evaluation settles", () => {
    expect(entryLog).toEqual(["body", "dispose async", "dispose sync"]);
  });

  test("a dispose error rejects the module's evaluation", async () => {
    let caught;
    try {
      await import("../../../fixtures/modules/top-level-using-dispose-throws.js");
    } catch (e) {
      caught = e;
    }
    expect(caught instanceof SuppressedError).toBe(false);
    expect(caught.message).toBe("dispose");
    expect(globalThis.topLevelUsingDisposeThrowsLog).toEqual([
      "body", "dispose",
    ]);
  });

  test("a body that throws still disposes, and a dispose error suppresses it", async () => {
    let caught;
    try {
      await import("../../../fixtures/modules/top-level-using-body-throws.js");
    } catch (e) {
      caught = e;
    }
    expect(caught instanceof SuppressedError).toBe(true);
    expect(caught.error.message).toBe("dispose");
    expect(caught.suppressed.message).toBe("body");
    expect(globalThis.topLevelUsingBodyThrowsLog).toEqual(["body", "dispose"]);
  });
});
