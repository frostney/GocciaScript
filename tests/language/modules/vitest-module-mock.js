/*---
description: >
  vi.mock with a factory replaces a module for the whole test file: the test's
  own import, code under test importing the same module, a default export, and
  the source-order rule that lets a later vi.unmock or vi.mock win.
features: [modules, runtime-modules]
---*/

import { describe, expect, test, vi, vitest } from "vitest";

import add, { label } from "./helpers/mock-target.js";
import { add as mockedAdd } from "./helpers/mock-target.js";
import { callAdd, readLabel } from "./helpers/mock-consumer.js";
import { label as unmockedLabel } from "./helpers/mock-unmock-target.js";
import { label as remockedLabel } from "./helpers/mock-remock-target.js";

// Written after every import on purpose: vi.mock is hoisted, so the mock is in
// place before any of the modules above is loaded.
vi.mock("./helpers/mock-target.js", () => ({
  label: "MOCKED",
  add: vi.fn(),
  default: { kind: "mocked" },
}));

// mock then unmock: the last directive wins, so the real module survives.
vi.mock("./helpers/mock-unmock-target.js", () => ({ label: "SHOULD-NOT-WIN" }));
vi.unmock("./helpers/mock-unmock-target.js");

// unmock then mock, spelled through the `vitest` alias of the same namespace
// and through a non-normalized path — both are hoisted, and the address is
// resolved, so the two spellings name the same module.
vitest.unmock("./helpers/mock-remock-target.js");
vitest.mock("./helpers/mock-remock-target.js", () => ({ label: "FIRST" }));
vi.mock("./helpers/../helpers/mock-remock-target.js", () => ({ label: "LAST" }));

describe("vi.mock factory form", () => {
  test("replaces the module for the test file's own import", () => {
    expect(label).toBe("MOCKED");
  });

  test("replaces the module for code under test importing it too", () => {
    expect(readLabel()).toBe("MOCKED");
  });

  test("the test file and the code under test share one mock instance", () => {
    expect(mockedAdd.mock.calls.length).toBe(0);

    callAdd(2, 3);

    expect(mockedAdd.mock.calls.length).toBe(1);
    expect(mockedAdd).toHaveBeenCalledWith(2, 3);
    // A vi.fn() with no implementation returns undefined, so the consumer sees
    // the mock rather than the real `add`.
    expect(callAdd(2, 3)).toBe(undefined);
  });

  test("a default key becomes the module's default export", () => {
    expect(add.kind).toBe("mocked");
  });

  test("a later vi.unmock wins over an earlier vi.mock", () => {
    expect(unmockedLabel).toBe("REAL-UNMOCKED");
  });

  test("a later vi.mock wins over an earlier vi.unmock and an earlier mock", () => {
    expect(remockedLabel).toBe("LAST");
  });

  test("vi.mock is a no-op by the time the call itself runs", () => {
    // The hoisted pre-pass already did the work; the runtime member exists so
    // a suite written against Vitest still runs unchanged.
    expect(vi.mock("./helpers/mock-target.js", () => ({}))).toBe(undefined);
    expect(vi.unmock("./helpers/mock-target.js")).toBe(undefined);
  });

  test("vitest is the same namespace object as vi", () => {
    expect(vitest).toBe(vi);
  });
});
