/*---
description: >
  Without compat-annex-b, a ShadowRealm's global object has no escape or
  unescape, as the host's has none.
features: [ShadowRealm, escape, unescape]
---*/

test("a new realm has no escape or unescape", () => {
  const realm = new ShadowRealm();
  expect(realm.evaluate("typeof escape")).toBe("undefined");
  expect(realm.evaluate("typeof unescape")).toBe("undefined");
});
