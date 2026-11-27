/*---
description: >
  Under compat-annex-b, a ShadowRealm's global object has escape and unescape
  as well as the host's.
features: [ShadowRealm, compat-annex-b, escape, unescape]
---*/

describe("escape and unescape in a new realm", () => {
  test("are functions there", () => {
    const realm = new ShadowRealm();
    expect(realm.evaluate("typeof escape")).toBe("function");
    expect(realm.evaluate("typeof unescape")).toBe("function");
  });

  test("are that realm's own functions", () => {
    const realm = new ShadowRealm();
    expect(realm.evaluate("escape('a b\\u20AC')")).toBe("a%20b%u20AC");
    expect(realm.evaluate("unescape('%u20AC%41')")).toBe("\u20ACA");
  });
});
