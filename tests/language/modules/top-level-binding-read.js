/*---
description: A function in an imported module reads that module's top-level bindings repeatedly from the same site
features: [modules]
---*/

import {
  bumpImportedCounter,
  readAfterInitialization,
  readBeforeInitialization,
  readImportedCounter,
  readLate,
  readLimit,
  readSettings,
  setLate,
} from "./helpers/top-level-binding-read.js";

describe("top-level binding reads in a module that has imports", () => {
  test("the same read throws before initialization and succeeds after", () => {
    expect(readBeforeInitialization).toBe("ReferenceError");
    expect(readAfterInitialization).toBe("first");
  });

  test("repeated reads observe every reassignment", () => {
    const values = [
      "second",
      3,
      2147483647,
      2147483648,
      -2147483648,
      -2147483649,
      1.5,
      NaN,
      Infinity,
      -Infinity,
      undefined,
      null,
      true,
      false,
      -0,
      0,
      1,
      "last",
    ];
    for (const value of values) {
      setLate(value);
      expect(Object.is(readLate(), value)).toBe(true);
    }
  });

  test("repeated reads of a const return the same value", () => {
    expect([readLimit(), readLimit(), readLimit()]).toEqual([16, 16, 16]);
    expect(readSettings()).toBe(readSettings());
    expect(readSettings().scale).toBe(3);
  });

  test("repeated reads of an imported binding stay live", () => {
    const before = readImportedCounter();
    bumpImportedCounter();
    bumpImportedCounter();
    expect(readImportedCounter()).toBe(before + 2);
  });
});
