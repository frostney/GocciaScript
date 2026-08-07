/*---
description: >
  The bundled vitest compatibility shim resolves the bare "vitest" specifier,
  re-exports goccia:test and assembles a vi namespace whose unsupported members
  throw instead of silently doing nothing.
features: [modules, runtime-modules]
---*/

import { describe, expect, mock, test, vi, vitest } from "vitest";

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

  test("vi.mock and vi.unmock are hoisted, so the calls themselves are no-ops", () => {
    // The mocking itself happens before the file is parsed; see
    // vitest-module-mock.js for the behaviour these calls stand in for.
    expect(vi.mock("./x.js", () => ({}))).toBe(undefined);
    expect(vi.unmock("./x.js")).toBe(undefined);
  });

  test("vitest is exported as an alias of the vi namespace", () => {
    expect(vitest).toBe(vi);
    expect(vitest.fn).toBe(mock);
  });

  test("the mocking members that are not implemented throw and name the reason", () => {
    expect(() => vi.doMock("./x.js")).toThrow(
      "vi.doMock is not supported by the GocciaScript vitest compatibility shim",
    );
    expect(() => vi.doMock("./x.js")).toThrow("module-cache eviction");
    expect(() => vi.doUnmock("./x.js")).toThrow("vi.doUnmock");
    expect(() => vi.resetModules()).toThrow("vi.resetModules");
    expect(() => vi.importActual("./x.js")).toThrow("vi.importActual");
    expect(() => vi.importActual("./x.js")).toThrow(
      "the real module is not reachable once it is mocked",
    );
    expect(() => vi.importMock("./x.js")).toThrow("vi.importMock");
    expect(() => vi.mocked({})).toThrow("vi.mocked");
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
    expect(typeof vi.importActual).toBe("function");
    expect(typeof vi.useFakeTimers).toBe("function");
    expect(typeof vi.stubGlobal).toBe("function");
    expect(typeof vi.restoreAllMocks).toBe("function");
  });

  test("every unsupported member points at the docs", () => {
    expect(() => vi.importActual("./x.js")).toThrow("docs/testing-api.md");
  });
});
