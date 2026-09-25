/*---
description: >
  The complete testing API is importable from goccia:test, so a suite can name
  what it uses instead of relying on the runner installing globals.
features: [modules, runtime-modules]
---*/

import * as testing from "goccia:test";
import {
  afterAll,
  afterEach,
  beforeAll,
  beforeEach,
  describe,
  expect,
  it,
  mock,
  onTestFinished,
  spyOn,
  test,
} from "goccia:test";

describe("goccia:test", () => {
  test("exports the registration helpers", () => {
    expect(typeof describe).toBe("function");
    expect(typeof test).toBe("function");
    expect(typeof it).toBe("function");
    expect(typeof expect).toBe("function");
  });

  test("exports every lifecycle hook", () => {
    expect(typeof beforeAll).toBe("function");
    expect(typeof beforeEach).toBe("function");
    expect(typeof afterEach).toBe("function");
    expect(typeof afterAll).toBe("function");
    expect(typeof onTestFinished).toBe("function");
  });

  test("exports the mocking helpers", () => {
    expect(typeof mock).toBe("function");
    expect(typeof spyOn).toBe("function");
  });

  test("carries the modifiers on test and describe", () => {
    expect(typeof test.each).toBe("function");
    expect(typeof test.skip).toBe("function");
    expect(typeof test.todo).toBe("function");
    expect(typeof test.only).toBe("function");
    expect(typeof describe.each).toBe("function");
    expect(typeof describe.skip).toBe("function");
    expect(typeof describe.only).toBe("function");
  });

  test("namespace import sees the same surface", () => {
    expect(typeof testing.describe).toBe("function");
    expect(typeof testing.expect).toBe("function");
    expect(testing.test).toBe(test);
  });

  test("imported helpers drive the same registry as the globals", () => {
    // Both directions across the import boundary. Asserting an imported mock
    // with the imported expect only proves the module is self-consistent —
    // it would still pass if the module got its own private registry.
    const viaImport = mock();
    viaImport("value");
    globalThis.expect(viaImport).toHaveBeenCalledWith("value");

    const viaGlobal = globalThis.mock();
    viaGlobal("other");
    expect(viaGlobal).toHaveBeenCalledWith("other");

    // The two spellings are the same function object, not two wrappers over
    // separate state.
    expect(globalThis.mock).toBe(mock);
    expect(globalThis.expect).toBe(expect);
  });
});
