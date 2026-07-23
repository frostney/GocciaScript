/*---
description: Transitive module graphs preserve live import bindings
features: [modules, compat-function]
---*/

import {
  forwardedCounter,
  readCounter,
  resetCounter,
  setCounter,
  twoHopWordCount,
} from "./helpers/live-bindings-middle.js";
import { threeHopWordCount } from "./helpers/live-bindings-outer.js";
import {
  counter as reExportedCounter,
  increment as incrementReExportedCounter,
} from "./helpers/live-bindings-re-export.js";
import * as liveNamespace from "./helpers/live-bindings-star-re-export.js";
import { observedCycleRead } from "./helpers/live-bindings-cycle-a.js";
import { observedLinkedCycle } from
  "../../../../fixtures/modules/live-bindings-link-parent.js";

describe("transitive module live bindings", () => {
  test("two-hop imports remain initialized in hoisted functions", () => {
    expect(twoHopWordCount("one two three")).toBe(3);
  });

  test("three-hop imports remain initialized in hoisted functions", () => {
    expect(threeHopWordCount("one two three four")).toBe(4);
  });

  test("named imports read the current exported binding", () => {
    resetCounter();
    expect(readCounter()).toBe(0);
    setCounter(2);
    expect(readCounter()).toBe(2);
  });

  test("exports of imported names remain live", () => {
    resetCounter();
    setCounter(3);
    expect(forwardedCounter).toBe(3);
  });

  test("indirect re-exports remain live", () => {
    resetCounter();
    incrementReExportedCounter();
    expect(reExportedCounter).toBe(1);
  });

  test("module namespace reads remain live through star re-exports", () => {
    resetCounter();
    liveNamespace.setCounter(4);
    expect(liveNamespace.counter).toBe(4);
  });

  test("cyclic imports preserve the temporal dead zone", () => {
    expect(observedCycleRead).toBe("ReferenceError");
  });

  test("cyclic dependencies evaluate after parent imports are linked", () => {
    expect(observedLinkedCycle).toBe("linked:linked");
  });
});
