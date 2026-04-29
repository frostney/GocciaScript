/*---
description: String.prototype.toUpperCase basic functionality
features: [String.prototype.toUpperCase]
---*/

test("String.prototype.toUpperCase converts to uppercase", () => {
  expect("hello".toUpperCase()).toBe("HELLO");
  expect("World".toUpperCase()).toBe("WORLD");
  expect("".toUpperCase()).toBe("");
});

test("String.prototype.toUpperCase maps Unicode characters", () => {
  expect("éöσ".toUpperCase()).toBe("ÉÖΣ");
});

test("String.prototype.toUpperCase coerces non-string receivers", () => {
  expect(String.prototype.toUpperCase.call(true)).toBe("TRUE");
});
