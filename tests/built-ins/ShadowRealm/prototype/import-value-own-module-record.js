/*---
description: A ShadowRealm's importValue loads a module record of the realm's own, even for a module the caller's realm has loaded
features: [ShadowRealm]
---*/

// A Module Record belongs to one realm (ES2026 16.2.1.5 [[Realm]]), and
// ShadowRealmImportValue loads the module with the ShadowRealm as the
// referrer, so the module is evaluated again in that realm: its state is the
// realm's, and its code reads the realm's global object.
import { bump, marker } from "../helpers/import-value-counter.js";

describe("ShadowRealm.prototype.importValue module records", () => {
  test("a module the caller has loaded is evaluated again in the ShadowRealm", async () => {
    bump();
    bump();
    const realm = new ShadowRealm();
    const childBump = await realm.importValue("../helpers/import-value-counter.js", "bump");
    expect(childBump()).toBe(1);
    expect(bump()).toBe(3);
    expect(childBump()).toBe(2);
  });

  test("the realm's module reads the realm's global object", async () => {
    globalThis.__importValueRealmMarker = "caller";
    const realm = new ShadowRealm();
    realm.evaluate('globalThis.__importValueRealmMarker = "shadow"');
    const childMarker = await realm.importValue("../helpers/import-value-counter.js", "marker");
    expect(childMarker()).toBe("shadow");
    expect(marker()).toBe("caller");
  });

  test("two imports in one ShadowRealm share that realm's record", async () => {
    const realm = new ShadowRealm();
    const first = await realm.importValue("../helpers/import-value-counter.js", "bump");
    const second = await realm.importValue("../helpers/import-value-counter.js", "bump");
    expect(first()).toBe(1);
    expect(second()).toBe(2);
  });
});
