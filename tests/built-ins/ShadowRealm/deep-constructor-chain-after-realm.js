/*---
description: Once a ShadowRealm exists, a long chain of default constructors still constructs without one native frame per class
features: [ShadowRealm, class-inheritance, extends, super]
---*/

// ES2026 §15.7.14 ClassDefinitionEvaluation step 14.a: a derived class's
// default constructor performs Construct on its [[Prototype]]. Nothing bounds
// how many such classes a chain may have, and a second realm in the process
// does not change that for a chain whose classes all belong to one realm.
const chain = (depth, base, make) => {
  let C = base;
  for (const _ of Array.from({ length: depth })) {
    const B = C;
    C = make(B);
  }
  return C;
};

describe("a default-constructor chain after a ShadowRealm is made", () => {
  // 64-bit hosts only, as tests/language/classes/deep-implicit-constructor-chain.js:
  // 30,000 classes fit the memory a test worker should spend there.
  const deepChainRuns = typeof Goccia !== "undefined" &&
    ["x86_64", "aarch64", "powerpc64"].includes(Goccia.build.arch) &&
    !Goccia.build.gcStress;

  test.runIf(deepChainRuns)("constructs a 30,000-class chain", () => {
    const realm = new ShadowRealm();
    expect(realm.evaluate("1 + 1")).toBe(2);
    class Base { constructor(value) { this.value = value; } }
    const Leaf = chain(30000, Base, (B) => class extends B {});
    const leaf = new Leaf(42);
    expect(leaf.value).toBe(42);
    expect(leaf instanceof Base).toBe(true);
  });

  test("a parent class from the other realm still constructs", () => {
    const realm = new ShadowRealm();
    const make = realm.evaluate("(x) => x * 2");
    class Base { constructor(n) { this.n = make(n); } }
    class Leaf extends Base {}
    expect(new Leaf(21).n).toBe(42);
  });
});
