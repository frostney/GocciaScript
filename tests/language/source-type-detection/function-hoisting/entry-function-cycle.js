/*---
description: >
  A detected Module entry's function declarations are instantiated before
  any module it imports evaluates (ES2026 §16.2.1.7.3.1
  InitializeEnvironment), so a peer in a cycle with the entry can call one
  while it evaluates, and the entry body then reads the peer's result. Node.js
  computes "yh" for the same files.
features: [modules, compat-function]
---*/

import { peerValue } from "../../../../fixtures/modules/detected-entry-function-peer.js";

globalThis.detectedEntryFunctionCycleRuns =
  (globalThis.detectedEntryFunctionCycleRuns ?? 0) + 1;

export function entryFunction() {
  return "h";
}

const valueAtEntryBody = peerValue;

describe("an entry with module syntax and a function declaration in a cycle", () => {
  test("the peer calls the entry's function while it evaluates", () => {
    expect(valueAtEntryBody).toBe("yh");
  });

  test("the entry body runs once", () => {
    expect(globalThis.detectedEntryFunctionCycleRuns).toBe(1);
  });
});
