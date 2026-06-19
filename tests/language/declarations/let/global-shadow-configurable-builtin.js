/*---
description: global lexical declarations may shadow configurable global object properties
features: [let-declaration]
---*/

let Array = "lexical Array";

test("top-level let Array shadows the configurable global property", () => {
  expect(Array).toBe("lexical Array");
  expect(typeof globalThis.Array).toBe("function");
});
