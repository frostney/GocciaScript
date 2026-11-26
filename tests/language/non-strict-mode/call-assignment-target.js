/*---
description: Non-strict code may assign to a call; the call runs, then a ReferenceError (Annex B)
features: [compat-function, compat-non-strict-mode]
---*/

const make = () => {
  const log = [];
  const f = () => { log.push("f"); return {}; };
  const g = () => { log.push("g"); return 1; };
  return { log, f, g };
};

test("an assignment to a call calls it and throws before the value", () => {
  const { log, f, g } = make();
  expect(() => { f() = g(); }).toThrow(ReferenceError);
  expect(log).toEqual(["f"]);
});

test("a compound assignment to a call throws the same way", () => {
  const { log, f, g } = make();
  expect(() => { f() += g(); }).toThrow(ReferenceError);
  expect(log).toEqual(["f"]);
});

test("updating a call throws after calling it", () => {
  const { log, f } = make();
  expect(() => { f()++; }).toThrow(ReferenceError);
  expect(() => { --f(); }).toThrow(ReferenceError);
  expect(log).toEqual(["f", "f"]);
});

test("a parenthesized call is a call target too", () => {
  const { log, f } = make();
  expect(() => { (f()) = 1; }).toThrow(ReferenceError);
  expect(log).toEqual(["f"]);
});
