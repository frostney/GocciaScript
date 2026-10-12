/*---
description: A module in a cycle with the entry sees the entry's var exports as undefined and its function declarations as callable before the entry body runs
features: [modules, compat-function, compat-var]
---*/

// ES2026 §16.2.1.7.3.1 InitializeEnvironment initializes var bindings to
// undefined and instantiates function declarations before any requested
// module evaluates; only lexical bindings stay in their temporal dead zone.
import { readsAtPeerEvaluation } from "../../../../fixtures/modules/entry-hoisting-peer.js";

export var entryVar = "assigned";
export function entryFunction() {
  return "entry function";
}
export default function () {
  return "entry default function";
}
export let entryLet = "let";

describe("entry module exports seen from a cycle before the entry runs", () => {
  test("a var export is undefined", () => {
    expect(readsAtPeerEvaluation.varValue).toBe("undefined");
  });

  test("an exported function declaration is callable", () => {
    expect(readsAtPeerEvaluation.functionResult).toBe("entry function");
  });

  test("an anonymous default function declaration is callable", () => {
    expect(readsAtPeerEvaluation.defaultResult).toBe("entry default function");
  });

  test("a let export is in its temporal dead zone", () => {
    expect(readsAtPeerEvaluation.letValue).toBe("ReferenceError");
  });
});
