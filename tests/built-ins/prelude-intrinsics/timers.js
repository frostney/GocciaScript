/*---
description: The timers prelude schedules and runs timers without looking up anything a script can replace
features: [setTimeout]
---*/

import { expectClean, installTripwire } from "./helpers/tripwire.js";

const wire = installTripwire();
afterAll(() => wire.uninstall());

test("setTimeout, setInterval and their clears", async () => {
  let fired = 0;
  const id = expectClean(wire.invoke("globalThis.setTimeout", undefined, (a) => { fired += a; }, 0, 2));
  const iv = expectClean(wire.invoke("globalThis.setInterval", undefined, () => {}, 5));
  expectClean(wire.invoke("globalThis.clearInterval", undefined, iv));
  const gone = expectClean(wire.invoke("globalThis.setTimeout", undefined, () => { fired += 100; }, 0));
  expectClean(wire.invoke("globalThis.clearTimeout", undefined, gone));
  const later = new Promise((resolve) => { setTimeout(resolve, 1); });
  await later;
  expect(fired).toBe(2);
  expect(typeof id).toBe("number");
});
