/*---
description: The modules prelude loads data and text modules without looking up anything a script can replace
features: [dynamic-import, import-attributes]
---*/

import { expectClean, installTripwire } from "./helpers/tripwire.js";

const wire = installTripwire();
afterAll(() => wire.uninstall());

test("JSON, CSV and text modules imported after a script could change the intrinsics", async () => {
  const json = expectClean(await wire.watchAsync(() => import("./helpers/module-fixture.json")));
  expect([json.a, json.default]).toEqual([1, 2]);
  const csv = expectClean(await wire.watchAsync(() => import("./helpers/module-fixture.csv")));
  expect(csv["0"]).toEqual({ a: "1", b: "2" });
  const text = expectClean(await wire.watchAsync(() => import("./helpers/module-fixture.txt", { with: { type: "text" } })));
  expect(text.default.startsWith("line one")).toBe(true);
});
