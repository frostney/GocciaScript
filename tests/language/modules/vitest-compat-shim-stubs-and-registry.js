/*---
description: >
  vi.stubGlobal / vi.unstubAllGlobals and the vi.clearAllMocks /
  vi.resetAllMocks / vi.restoreAllMocks registry, matching Vitest's semantics
  for what each one drops and what it keeps.
features: [modules, runtime-modules]
---*/

import { describe, expect, test, vi } from "vitest";

describe("vi global stubs", () => {
  test("stubs a global that did not exist and removes it again", () => {
    expect("stubbedAbsent" in globalThis).toBe(false);

    vi.stubGlobal("stubbedAbsent", 7);
    expect(globalThis.stubbedAbsent).toBe(7);

    vi.unstubAllGlobals();
    // Deleted, not left behind as an undefined global: Vitest restores the
    // absence, and `"name" in globalThis` is how feature detection reads it.
    expect("stubbedAbsent" in globalThis).toBe(false);
  });

  test("restores the original value, not the previous stub", () => {
    globalThis.stubbedPresent = "original";

    vi.stubGlobal("stubbedPresent", "first");
    vi.stubGlobal("stubbedPresent", "second");
    expect(globalThis.stubbedPresent).toBe("second");

    vi.unstubAllGlobals();
    expect(globalThis.stubbedPresent).toBe("original");

    delete globalThis.stubbedPresent;
  });

  test("unwinds several stubs at once", () => {
    globalThis.stubbedPair = "kept";

    vi.stubGlobal("stubbedPair", "changed");
    vi.stubGlobal("stubbedSolo", "added");

    vi.unstubAllGlobals();

    expect(globalThis.stubbedPair).toBe("kept");
    expect("stubbedSolo" in globalThis).toBe(false);

    delete globalThis.stubbedPair;
  });

  test("a second unstub after everything is unwound is a no-op", () => {
    vi.stubGlobal("stubbedTwice", 1);
    vi.unstubAllGlobals();
    vi.unstubAllGlobals();

    expect("stubbedTwice" in globalThis).toBe(false);
  });
});

describe("vi mock registry", () => {
  test("clearAllMocks drops recorded calls and keeps the implementation", () => {
    const fn = vi.fn(() => "implementation");
    fn("first");

    expect(fn).toHaveBeenCalledTimes(1);

    vi.clearAllMocks();

    expect(fn).toHaveBeenCalledTimes(0);
    expect(fn()).toBe("implementation");
  });

  test("resetAllMocks drops calls and later implementations", () => {
    const fn = vi.fn(() => "created with");
    fn.mockReturnValue("overridden");
    fn("first");

    expect(fn()).toBe("overridden");

    vi.resetAllMocks();

    expect(fn).toHaveBeenCalledTimes(0);
    // Vitest reinstates the implementation the mock was created with rather
    // than leaving it returning undefined.
    expect(fn()).toBe("created with");
  });

  test("resetAllMocks leaves a mock created without an implementation empty", () => {
    const fn = vi.fn();
    fn.mockReturnValue("overridden");

    vi.resetAllMocks();

    expect(fn()).toBe(undefined);
  });

  test("restoreAllMocks reverts spies to the method they replaced", () => {
    const target = { read: () => "real" };
    const spy = vi.spyOn(target, "read").mockReturnValue("spied");

    expect(target.read()).toBe("spied");

    vi.restoreAllMocks();

    expect(target.read()).toBe("real");
    // The call made while the spy was installed still counts: restoring puts
    // the original method back, it does not erase what was recorded.
    expect(spy).toHaveBeenCalledTimes(1);
  });

  test("restoreAllMocks leaves bare mocks alone", () => {
    const fn = vi.fn(() => "still here");
    fn("recorded");

    vi.restoreAllMocks();

    expect(fn()).toBe("still here");
    // Only spies are restored; a vi.fn mock keeps its calls too.
    expect(fn).toHaveBeenCalledWith("recorded");
  });

  test("the registry spans every mock created through vi", () => {
    const first = vi.fn();
    const second = vi.fn();
    first("a");
    second("b");

    vi.clearAllMocks();

    expect(first).toHaveBeenCalledTimes(0);
    expect(second).toHaveBeenCalledTimes(0);
  });
});
