/*---
description: >
  An entry with no explicit source type that has a top-level import or
  export declaration runs as a Module (ADR 0140). When a module it imports
  imports it back, the import gets the entry's own Module Record, which is
  evaluating, so each module in the cycle evaluates once (ES2026
  §16.2.1.6.1.3.1 InnerModuleEvaluation, step 3). Node.js runs the same pair
  of files the same way.
features: [modules]
---*/

import { peerValue, readEntryValue } from "../../../fixtures/modules/detected-entry-cycle-peer.js";

globalThis.detectedEntryCycleRuns = (globalThis.detectedEntryCycleRuns ?? 0) + 1;

export const entryValue = "entry";

const valueAtEntryBody = peerValue;
const topLevelThis = this;
const metaUrl = import.meta.url;

describe("an entry with module syntax in an import cycle", () => {
  test("reads the peer's export, which the peer initialized first", () => {
    expect(valueAtEntryBody).toBe("y");
  });

  test("the peer reads the entry's own binding", () => {
    expect(readEntryValue()).toBe("entry");
  });

  test("the entry body runs once", () => {
    expect(globalThis.detectedEntryCycleRuns).toBe(1);
  });

  test("top-level this is undefined, as in a module", () => {
    expect(topLevelThis).toBeUndefined();
  });

  test("import.meta is available", () => {
    expect(metaUrl.endsWith("/entry-cycle.js")).toBe(true);
  });
});
