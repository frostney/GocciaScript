/*---
description: String.prototype.toLowerCase converts to lowercase
features: [String.prototype.toLowerCase]
---*/

test("String.prototype.toLowerCase converts to lowercase", () => {
  expect("HELLO".toLowerCase()).toBe("hello");
  expect("WoRLd".toLowerCase()).toBe("world");
  expect("".toLowerCase()).toBe("");
});

test("String.prototype.toLowerCase maps Unicode characters", () => {
  expect("ÉÖΣ".toLowerCase()).toBe("éöσ");
});

test("String.prototype.toLowerCase coerces non-string receivers", () => {
  expect(String.prototype.toLowerCase.call(true)).toBe("true");
});
