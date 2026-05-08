/*---
description: Intl[Symbol.toStringTag]
features: [Intl]
---*/

const isIntl = typeof Intl !== "undefined";

describe.runIf(isIntl)("Intl[Symbol.toStringTag]", () => {
  test("toStringTag is 'Intl'", () => {
    expect(Intl[Symbol.toStringTag]).toBe("Intl");
  });

  test("Object.prototype.toString returns [object Intl]", () => {
    expect(Object.prototype.toString.call(Intl)).toBe("[object Intl]");
  });
});
