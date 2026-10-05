// ES2026 §16.2.1.13 GetModuleNamespace creates a module's namespace object
// once: every namespace import of the entry, a peer's included, and an
// `export * as` of the entry is that one object, however late the entry's
// exports are bound.
import { peerNamespace } from "../../../fixtures/modules/entry-namespace-peer.js";
import * as selfNamespace from "./entry-namespace-identity.js";
import { ownNamespace } from "./entry-namespace-identity.js";

export * as ownNamespace from "./entry-namespace-identity.js";
export { peerValue } from "../../../fixtures/modules/entry-namespace-peer.js";
export const entryValue = "entry";

describe("entry module namespace identity", () => {
  test("a peer's namespace import of the entry is the entry's namespace", () => {
    expect(peerNamespace).toBe(selfNamespace);
  });

  test("an export * as of the entry itself is the entry's namespace", () => {
    expect(ownNamespace).toBe(selfNamespace);
    expect(selfNamespace.ownNamespace).toBe(selfNamespace);
  });

  test("a dynamic import of the entry returns the same namespace", async () => {
    expect(await import("./entry-namespace-identity.js")).toBe(selfNamespace);
  });

  test("the namespace has every export, re-exports included", () => {
    expect(Object.keys(selfNamespace).sort()).toEqual(["entryValue", "ownNamespace", "peerValue"]);
  });
});
