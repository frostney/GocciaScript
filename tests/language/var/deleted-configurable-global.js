/*---
description: Global var redeclaration does not recreate a deleted configurable property
features: [compat-var]
---*/

const originalParseInt = globalThis.parseInt;
var parseInt = originalParseInt;
const deletedParseInt = delete globalThis.parseInt;
let redeclarationErrorName = "";

try {
  var parseInt = 2;
} catch (error) {
  redeclarationErrorName = error.name;
}

test("strict var initializer does not recreate a deleted global property", () => {
  expect(deletedParseInt).toBe(true);
  expect(redeclarationErrorName).toBe("ReferenceError");
  expect(Object.hasOwn(globalThis, "parseInt")).toBe(false);
});
