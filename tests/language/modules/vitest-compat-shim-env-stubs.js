/*---
description: >
  vi.stubEnv / vi.unstubAllEnvs write to whatever process.env the host injected,
  with Vitest's semantics — String coercion, an undefined value deleting, and
  the first stub of a name recording what to restore.
features: [modules, runtime-modules]
---*/

import { describe, expect, test, vi } from "vitest";

// GocciaScript has no process of its own. A suite gets one from the host —
// `--global` / `--globals` on the runner, or by defining it as this file does,
// which keeps the test self-contained.
globalThis.process = { env: {} };

describe("process.env", () => {
  test("an unstubbed variable reads as undefined rather than throwing", () => {
    expect(process.env.NEVER_STUBBED).toBe(undefined);
  });

  test("vi.stubEnv reports what is missing when there is no process.env", () => {
    const saved = globalThis.process;
    globalThis.process = undefined;
    try {
      expect(() => vi.stubEnv("X", "1")).toThrow("needs a process.env");
      expect(() => vi.stubEnv("X", "1")).toThrow("--global");
      expect(() => vi.unstubAllEnvs()).toThrow("needs a process.env");
    } finally {
      globalThis.process = saved;
    }
  });
});

describe("vi.stubEnv", () => {
  test("stubs a variable and unstubs it back to absent", () => {
    expect("STUBBED_ABSENT" in process.env).toBe(false);

    vi.stubEnv("STUBBED_ABSENT", "value");
    expect(process.env.STUBBED_ABSENT).toBe("value");

    vi.unstubAllEnvs();
    expect("STUBBED_ABSENT" in process.env).toBe(false);
  });

  test("restores the original value, not the previous stub", () => {
    process.env.STUBBED_PRESENT = "original";

    vi.stubEnv("STUBBED_PRESENT", "first");
    vi.stubEnv("STUBBED_PRESENT", "second");
    expect(process.env.STUBBED_PRESENT).toBe("second");

    vi.unstubAllEnvs();
    expect(process.env.STUBBED_PRESENT).toBe("original");

    delete process.env.STUBBED_PRESENT;
  });

  test("coerces the value to a string", () => {
    vi.stubEnv("STUBBED_PORT", 8080);
    expect(process.env.STUBBED_PORT).toBe("8080");

    vi.stubEnv("STUBBED_FLAG", true);
    expect(process.env.STUBBED_FLAG).toBe("true");

    vi.unstubAllEnvs();
  });

  test("an undefined value deletes the variable", () => {
    process.env.STUBBED_DOOMED = "present";

    vi.stubEnv("STUBBED_DOOMED", undefined);
    expect("STUBBED_DOOMED" in process.env).toBe(false);

    vi.unstubAllEnvs();
    expect(process.env.STUBBED_DOOMED).toBe("present");

    delete process.env.STUBBED_DOOMED;
  });

  test("unwinds several variables at once", () => {
    process.env.STUBBED_KEPT = "kept";

    vi.stubEnv("STUBBED_KEPT", "changed");
    vi.stubEnv("STUBBED_ADDED", "added");

    vi.unstubAllEnvs();

    expect(process.env.STUBBED_KEPT).toBe("kept");
    expect("STUBBED_ADDED" in process.env).toBe(false);

    delete process.env.STUBBED_KEPT;
  });

  test("a second unstub after everything is unwound is a no-op", () => {
    vi.stubEnv("STUBBED_TWICE", "1");
    vi.unstubAllEnvs();
    vi.unstubAllEnvs();

    expect("STUBBED_TWICE" in process.env).toBe(false);
  });

  test("both members return vi for chaining", () => {
    expect(vi.stubEnv("STUBBED_CHAIN", "a")).toBe(vi);
    expect(vi.unstubAllEnvs()).toBe(vi);
  });

  test("a dynamic lookup sees the stub", () => {
    const read = (name) => process.env[name];

    vi.stubEnv("STUBBED_DYNAMIC", "found");
    expect(read("STUBBED_DYNAMIC")).toBe("found");

    vi.unstubAllEnvs();
    expect(read("STUBBED_DYNAMIC")).toBe(undefined);
  });
});
