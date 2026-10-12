// The entry and a helper import each other (entry -> helper -> entry). The
// helper's import of the entry resolves to the entry record that is already
// evaluating (ES2026 §16.2.1.6.1.3.1 InnerModuleEvaluation step 3), so the
// entry body runs once, after the helper, and the helper sees the entry's
// bindings live: in their temporal dead zone while the entry has not run.
import { entryReadsAtHelperEvaluation, readEntryNow } from "../../../fixtures/modules/entry-cycle-peer.js";

globalThis.entryCycleOrder = [...(globalThis.entryCycleOrder ?? []), "entry"];

export const entryValue = "entry value";
export let entryCounter = 1;
export default "entry default";

entryCounter = 2;

describe("entry module in an import cycle", () => {
  test("each module body evaluates once, the helper first", () => {
    expect(globalThis.entryCycleOrder).toEqual(["helper", "entry"]);
  });

  test("the helper saw the entry's exports in their temporal dead zone", () => {
    expect(entryReadsAtHelperEvaluation).toEqual({
      value: "ReferenceError",
      counter: "ReferenceError",
      default: "ReferenceError",
    });
  });

  test("the helper reads the entry's bindings live", () => {
    expect(readEntryNow()).toEqual({
      value: "entry value",
      counter: 2,
      default: "entry default",
    });
  });
});
