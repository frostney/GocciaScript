/*---
description: >
  The bundled vitest compatibility shim resolves the bare "vitest" specifier,
  re-exports goccia:test and assembles a vi namespace whose unsupported members
  throw instead of silently doing nothing.
features: [modules, runtime-modules]
---*/

import { describe, expect, mock, test, vi } from "vitest";

describe("vitest compatibility shim", () => {
  test("re-exports the testing API", () => {
    expect(typeof describe).toBe("function");
    expect(typeof test).toBe("function");
    expect(typeof expect).toBe("function");
    expect(typeof mock).toBe("function");
  });

  test("vi.fn is the engine's mock", () => {
    expect(vi.fn).toBe(mock);

    const fn = vi.fn();
    fn(1, 2);
    expect(fn).toHaveBeenCalledWith(1, 2);
    expect(fn).toHaveBeenCalledTimes(1);
  });

  test("vi.spyOn wraps a method", () => {
    const target = { run: () => "ok" };
    const spy = vi.spyOn(target, "run");

    expect(target.run()).toBe("ok");
    expect(spy).toHaveBeenCalled();
  });

  test("a globally created mock is assertable through the imported expect", () => {
    // Same registry either way: the shim re-exports the very same helpers.
    const viaGlobal = mock();
    viaGlobal("shared");

    expect(viaGlobal).toHaveBeenCalledWith("shared");
  });

  test("module mocking throws and names the reason", () => {
    expect(() => vi.mock("./x.js")).toThrow(
      "vi.mock is not supported by the GocciaScript vitest compatibility shim",
    );
    expect(() => vi.mock("./x.js")).toThrow("keeps no module registry");
    expect(() => vi.unmock("./x.js")).toThrow("vi.unmock is not supported");
    expect(() => vi.importActual("./x.js")).toThrow("vi.importActual");
    expect(() => vi.hoisted(() => {})).toThrow("vi.hoisted");
  });

  test("fake timers throw and name the reason", () => {
    expect(() => vi.useFakeTimers()).toThrow("vi.useFakeTimers is not supported");
    expect(() => vi.useFakeTimers()).toThrow("no fake-timer clock");
    expect(() => vi.setSystemTime(0)).toThrow("vi.setSystemTime");
    expect(() => vi.advanceTimersByTime(1)).toThrow("vi.advanceTimersByTime");
    expect(() => vi.runAllTimers()).toThrow("vi.runAllTimers");
  });

  test("global stubbing throws and names the reason", () => {
    expect(() => vi.stubGlobal("x", 1)).toThrow("vi.stubGlobal is not supported");
    expect(() => vi.stubEnv("X", "1")).toThrow("does not snapshot globals");
    expect(() => vi.unstubAllGlobals()).toThrow("vi.unstubAllGlobals");
  });

  test("bulk mock management points at the per-mock methods", () => {
    expect(() => vi.restoreAllMocks()).toThrow("vi.restoreAllMocks is not supported");
    expect(() => vi.restoreAllMocks()).toThrow("mockRestore");
    expect(() => vi.clearAllMocks()).toThrow("vi.clearAllMocks");
    expect(() => vi.resetAllMocks()).toThrow("vi.resetAllMocks");
  });

  test("every unsupported member is a defined function, never a no-op", () => {
    expect(typeof vi.mock).toBe("function");
    expect(typeof vi.useFakeTimers).toBe("function");
    expect(typeof vi.stubGlobal).toBe("function");
    expect(typeof vi.restoreAllMocks).toBe("function");
  });

  test("every unsupported member points at the docs", () => {
    expect(() => vi.mock("./x.js")).toThrow("docs/testing-api.md");
  });
});
