/*---
description: Script await bindings support computed access and calls at top level
features: [async-await]
---*/
const await = (value) => value + 1;
await[0] = "index";
await.k = "key";
const indexed = await[0];
const keyed = await["k"];
const called = await(1);
const emptyCall = await();
test("top-level await is a Script identifier for computed access and calls", () => {
  expect(indexed).toBe("index");
  expect(keyed).toBe("key");
  expect(called).toBe(2);
  expect(Number.isNaN(emptyCall)).toBe(true);
});
