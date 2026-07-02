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
  expect("ÉÖΣ".toLowerCase()).toBe("éöς");
});

test("String.prototype.toLowerCase applies Unicode special casing", () => {
  expect("\u0130".toLowerCase()).toBe("i\u0307");
  expect("A\u03A3".toLowerCase()).toBe("a\u03C2");
  expect("A\u03A3B".toLowerCase()).toBe("a\u03C3b");
});

test("String.prototype.toLowerCase coerces non-string receivers", () => {
  expect(String.prototype.toLowerCase.call(true)).toBe("true");
});
