/*---
description: An object literal has no property limit of its own, and a function holds more than 65,535 constants
features: [ShadowRealm, Function]
---*/

// Each distinct property name and string value is a constant of the function
// the literal is in. A function's constants are not limited to 65,535: an
// object literal with more distinct names and values evaluates.
const realm = new ShadowRealm();
const list = (count, item, separator = ", ") =>
  Array.from({ length: count }, (_, i) => item(i)).join(separator);
// The same list from a replacement pattern in which $& is the item's index,
// built without a guest callback per item.
const numbered = (count, pattern) => [...Array(count).keys()].join(", ").replace(/\d+/g, pattern);

// A GC-stress build collects every 8 instructions, so a guest callback per
// item, or evaluating a literal, costs its size times the live heap: these
// sizes take over half an hour there. A stress run evaluates a smaller literal
// and skips the 66,000-property one; the sources that only fail to compile are
// built by numbered and run in every build.
const gcStress = typeof Goccia !== "undefined" && Goccia.build.gcStress;
const distinctNames = gcStress ? 2000 : 20000;

describe("object literals and the constant limit", () => {
  test(`${distinctNames} distinct property names evaluate`, () => {
    expect(realm.evaluate(`const o = { ${numbered(distinctNames, "p$&: 0")} }; Object.keys(o).length`)).toBe(distinctNames);
  });

  test.runIf(!gcStress)("66,000 properties that repeat a few names evaluate", () => {
    expect(realm.evaluate(`const o = { ${list(66000, (i) => `p${i % 100}: ${i % 1000}`)} }; Object.keys(o).length + ":" + o.p99`)).toBe("100:999");
  });

  test("33,000 properties with distinct names and string values evaluate", () => {
    expect(realm.evaluate(`const o = { ${numbered(33000, 'p$&: "v$&"')} }; Object.keys(o).length + ":" + o.p32999`))
      .toBe("33000:v32999");
  });

  test("a literal of 66,000 constants inside a function made by new Function runs", () => {
    const make = new Function(`return () => ({ ${numbered(33000, 'p$&: "v$&"')} });`);
    expect(make()().p32999).toBe("v32999");
  });
});
