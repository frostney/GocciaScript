import { describe, expect, test } from "bun:test";
import { buildEngineArgs } from "@/lib/engine-args";
import type { VendorFeatureSet } from "@/lib/vendor-manifest";

const OPTIONS = { asi: false, compatVar: false, compatFunction: false };

/** A 0.14-shaped binary: the ADR 0122 flag names plus config-trust options. */
const CAPABILITY_FEATURES: VendorFeatureSet = {
  loader: [
    "--allow-net",
    "--compat-asi",
    "--deny-read",
    "--ignore-config-permissions",
    "--max-instructions",
    "--max-memory",
    "--max-stack",
    "--mode",
    "--output",
    "--timeout",
  ],
  testRunner: [
    "--allow-net",
    "--compat-asi",
    "--deny-read",
    "--ignore-config-permissions",
    "--max-instructions",
    "--max-memory",
    "--max-stack",
    "--mode",
    "--no-progress",
    "--no-results",
    "--output",
    "--timeout",
  ],
};

/** A 0.13-shaped binary: no config trust, pre-0.14 flag names. */
const PRE_CAPABILITY_FEATURES: VendorFeatureSet = {
  loader: [
    "--allowed-host",
    "--compat-asi",
    "--max-instructions",
    "--max-memory",
    "--mode",
    "--no-host-filesystem",
    "--output",
    "--stack-size",
    "--timeout",
  ],
  testRunner: [
    "--allowed-host",
    "--compat-asi",
    "--max-instructions",
    "--max-memory",
    "--mode",
    "--no-host-filesystem",
    "--no-progress",
    "--no-results",
    "--output",
    "--stack-size",
    "--timeout",
  ],
};

describe("buildEngineArgs", () => {
  test("ignores config permission requests when the engine advertises it", () => {
    for (const kind of ["loader", "testRunner"] as const) {
      const args = buildEngineArgs(OPTIONS, CAPABILITY_FEATURES, kind);
      expect(args).toContain("--ignore-config-permissions");
      expect(args).toContain("--deny-read");
      expect(args).toContain("--allow-net=icanhazdadjoke.com");
      expect(args).toContain("--max-stack=2000");
    }
  });

  test("omits --ignore-config-permissions on engines without config trust", () => {
    const args = buildEngineArgs(OPTIONS, PRE_CAPABILITY_FEATURES, "loader");
    expect(args).not.toContain("--ignore-config-permissions");
    expect(args).toContain("--no-host-filesystem");
    expect(args).toContain("--stack-size=2000");
    expect(args).toEqual(
      expect.arrayContaining(["--allowed-host", "icanhazdadjoke.com"]),
    );
  });

  test("omits --ignore-config-permissions when the manifest has no probe data", () => {
    const args = buildEngineArgs(OPTIONS, undefined, "loader");
    expect(args).not.toContain("--ignore-config-permissions");
    expect(args[0]).toBe("--no-host-filesystem");
  });

  test("puts the ASI flag first and adds bytecode mode on request", () => {
    const args = buildEngineArgs(
      { ...OPTIONS, asi: true, mode: "bytecode" },
      CAPABILITY_FEATURES,
      "loader",
    );
    expect(args[0]).toBe("--compat-asi");
    expect(args.at(-1)).toBe("--mode=bytecode");
  });

  test("names the interpreter explicitly, since newer engines default to bytecode", () => {
    for (const kind of ["loader", "testRunner"] as const) {
      const args = buildEngineArgs(
        { ...OPTIONS, mode: "interpreted" },
        CAPABILITY_FEATURES,
        kind,
      );
      expect(args.at(-1)).toBe("--mode=interpreted");
      expect(args.filter((arg) => arg.startsWith("--mode"))).toHaveLength(1);
    }
  });

  test("sends no mode flag to a binary that does not advertise --mode", () => {
    const noMode: VendorFeatureSet = {
      loader: CAPABILITY_FEATURES.loader.filter((flag) => flag !== "--mode"),
      testRunner: CAPABILITY_FEATURES.testRunner,
    };
    for (const mode of ["bytecode", "interpreted"] as const) {
      const args = buildEngineArgs({ ...OPTIONS, mode }, noMode, "loader");
      expect(args.some((arg) => arg.startsWith("--mode"))).toBe(false);
    }
    expect(
      buildEngineArgs({ ...OPTIONS, mode: "bytecode" }, noMode, "testRunner"),
    ).toContain("--mode=bytecode");
  });

  test("leaves the mode to the engine when the request names none", () => {
    const args = buildEngineArgs(OPTIONS, CAPABILITY_FEATURES, "loader");
    expect(args.some((arg) => arg.startsWith("--mode"))).toBe(false);
  });
});
