/*---
description: |
  A String object has its characters and its length as own properties
  (ES2026 §10.4.3 String exotic objects): a character for each canonical
  index inside the string, non-writable and non-configurable, ahead of any
  property added later.
features: [String, Object.getOwnPropertyNames, Reflect.deleteProperty]
---*/

describe("own properties of a String object", () => {
  test("a String object reports the same indices as own properties", () => {
    const boxed = Object("ab");
    expect(Object.getOwnPropertyNames(boxed)).toEqual(["0", "1", "length"]);
    expect(Object.keys(boxed)).toEqual(["0", "1"]);
    expect(Object.hasOwn(boxed, "0")).toBe(true);
    expect(Object.hasOwn(boxed, "1")).toBe(true);
    expect(Object.hasOwn(boxed, "2")).toBe(false);
    expect(Object.hasOwn(boxed, "01")).toBe(false);
    expect(Object.hasOwn(boxed, "-1")).toBe(false);
    expect(Object.hasOwn(boxed, "length")).toBe(true);
    expect(Object.getOwnPropertyDescriptor(boxed, "1")).toEqual({
      value: "b",
      writable: false,
      enumerable: true,
      configurable: false,
    });
    expect(Object.getOwnPropertyDescriptor(boxed, "2")).toBe(undefined);
    expect(Object.getOwnPropertyDescriptor(boxed, "01")).toBe(undefined);
    expect(Reflect.deleteProperty(boxed, "0")).toBe(false);
    expect(Reflect.deleteProperty(boxed, "length")).toBe(false);
    expect(Reflect.deleteProperty(boxed, "5")).toBe(true);
    expect(Reflect.deleteProperty(boxed, "01")).toBe(true);
  });

  test("an expando index beyond the string sorts after the characters", () => {
    const boxed = Object("ab");
    boxed[5] = "five";
    boxed.name = "label";
    boxed["03"] = "text key";
    expect(Object.getOwnPropertyNames(boxed)).toEqual(["0", "1", "5", "length", "name", "03"]);
    expect(boxed[5]).toBe("five");
  });

  test("a ten-digit expando index is still an index", () => {
    const boxed = Object("ab");
    boxed.name = "label";
    boxed["2147483647"] = "largest";
    boxed["1000000000"] = "ten digits";
    expect(Object.getOwnPropertyNames(boxed)).toEqual(["0", "1", "1000000000", "2147483647", "length", "name"]);
  });
});
