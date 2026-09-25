/*---
description: pattern-match subjects survive a collecting Symbol.customMatcher getter
features: [Goccia, pattern-matching, Symbol.customMatcher]
---*/

// A value or extractor pattern boxes a primitive subject into a fresh number
// object, then reads matcher[Symbol.customMatcher]. When that read runs a user
// getter (or proxy trap), a collection forced from it can sweep the still-
// unrooted subject before it is passed to the matcher, so the matcher sees a
// freed block instead of the original value.

const collect = () => {
  Goccia.gc();
  Goccia.gc();
};

const churn = () => {
  const junk = Array.from({ length: 256 }, (_, i) => ({ i, s: `${i}` }));
  return junk.length;
};

describe("pattern-match subject under collection", () => {
  test("value pattern subject survives a collecting matcher getter", () => {
    const Matcher = {};
    Object.defineProperty(Matcher, Symbol.customMatcher, {
      get() {
        churn();
        collect();
        return (subject) => subject === 2.5;
      },
      configurable: true,
    });

    expect(2.5 is Matcher).toBe(true);
    expect(3.5 is Matcher).toBe(false);
    expect(123456.75 is Matcher).toBe(false);
  });

  test("extractor pattern subject survives a collecting matcher getter", () => {
    const Ext = {};
    Object.defineProperty(Ext, Symbol.customMatcher, {
      get() {
        churn();
        collect();
        return (subject) => [subject, subject * 2];
      },
      configurable: true,
    });

    let first = 0;
    let second = 0;
    if (2.5 is Ext(const a, const b)) {
      first = a;
      second = b;
    }
    expect(first).toBe(2.5);
    expect(second).toBe(5);
  });
});
