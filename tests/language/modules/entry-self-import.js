// An entry module that imports itself gets its own Module Record back
// (ES2026 §16.2.1.10 HostLoadImportedModule), which InnerModuleEvaluation
// (§16.2.1.6.1.3.1) does not evaluate a second time.
import { selfMarker as importedMarker } from "./entry-self-import.js";
import * as selfNamespace from "./entry-self-import.js";

globalThis.entrySelfImportRuns = (globalThis.entrySelfImportRuns ?? 0) + 1;

export const selfMarker = {};

describe("entry module that imports itself", () => {
  test("evaluates its body once", () => {
    expect(globalThis.entrySelfImportRuns).toBe(1);
  });

  test("the import resolves to the entry's own bindings", () => {
    expect(importedMarker).toBe(selfMarker);
    expect(selfNamespace.selfMarker).toBe(selfMarker);
  });

  test("a dynamic import after evaluation reuses the entry record", async () => {
    const namespace = await import("./entry-self-import.js");
    expect(namespace).toBe(selfNamespace);
    expect(namespace.selfMarker).toBe(selfMarker);
    expect(globalThis.entrySelfImportRuns).toBe(1);
  });
});
