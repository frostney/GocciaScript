/*---
description: console.count and console.time keep their label tables across a collection
features: [console]
---*/

// The console keeps its count and timer labels in objects nothing else
// references. A collection between two calls must not free them.
const churn = () => {
  Goccia.gc();
  Array.from({ length: 2000 }, (_, i) => ({ i, s: "x" + i, a: [i] }));
  Goccia.gc();
};

test("console.count keeps its labels across a collection", () => {
  expect(console.count("collection-count")).toBeUndefined();
  churn();
  expect(console.count("collection-count")).toBeUndefined();
  expect(console.countReset("collection-count")).toBeUndefined();
});

test("console.time keeps its timers across a collection", () => {
  expect(console.time("collection-timer")).toBeUndefined();
  churn();
  expect(console.timeLog("collection-timer")).toBeUndefined();
  expect(console.timeEnd("collection-timer")).toBeUndefined();
});
