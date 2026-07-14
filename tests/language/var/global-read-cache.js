/*---
description: Global var identifier reads observe live values after the bytecode cache warms
features: [compat-var]
---*/

var __gocciaCachedGlobalVar = 1;

const readCachedGlobalVar = () => __gocciaCachedGlobalVar;

test("cached global var reads observe later assignments", () => {
  expect(readCachedGlobalVar()).toBe(1);

  __gocciaCachedGlobalVar = 2;

  expect(readCachedGlobalVar()).toBe(2);
});
