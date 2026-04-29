import { describe, expect, test } from "bun:test";
import {
  findVersion,
  isFlagSupported,
  listPlaygroundVersions,
  type VendorFeatureSet,
  type VendorManifest,
} from "@/lib/vendor-manifest";

const MODERN_FEATURES: VendorFeatureSet = {
  loader: [
    "--allowed-host",
    "--asi",
    "--compat-function",
    "--compat-var",
    "--max-instructions",
    "--max-memory",
    "--mode",
    "--output",
    "--stack-size",
    "--timeout",
  ],
  testRunner: [
    "--allowed-host",
    "--asi",
    "--compat-function",
    "--compat-var",
    "--max-instructions",
    "--max-memory",
    "--mode",
    "--no-progress",
    "--no-results",
    "--output",
    "--stack-size",
    "--timeout",
  ],
};

/** 0.6.1's actual probed flag set (verified by running both binaries with
 *  `--help` / no-args at vendor time). The infra flags `--max-memory`,
 *  `--max-instructions`, `--stack-size`, `--allowed-host` are absent — the
 *  API drops them silently for this version. */
const LEGACY_061_FEATURES: VendorFeatureSet = {
  loader: ["--asi", "--mode", "--output", "--timeout"],
  testRunner: ["--asi", "--mode", "--no-progress", "--no-results", "--output"],
};

const SAMPLE_MANIFEST: VendorManifest = {
  defaultVersion: "nightly",
  versions: [
    {
      tag: "0.7.0",
      publishedAt: "2026-04-29T15:29:05Z",
      binaries: {
        loader: "0.7.0/GocciaScriptLoader",
        testRunner: "0.7.0/GocciaTestRunner",
      },
      features: MODERN_FEATURES,
    },
    {
      tag: "0.6.1",
      publishedAt: "2026-03-12T11:00:00Z",
      binaries: {
        loader: "0.6.1/ScriptLoader",
        testRunner: "0.6.1/TestRunner",
      },
      features: LEGACY_061_FEATURES,
    },
    {
      tag: "0.5.1",
      publishedAt: "2025-12-04T09:00:00Z",
      binaries: {
        loader: "0.5.1/ScriptLoader",
        testRunner: "0.5.1/TestRunner",
      },
    },
    {
      tag: "nightly",
      isPrerelease: true,
      publishedAt: "2026-04-29T01:51:50Z",
      binaries: {
        loader: "nightly/GocciaScriptLoader",
        testRunner: "nightly/GocciaTestRunner",
      },
      features: MODERN_FEATURES,
    },
  ],
};

describe("findVersion", () => {
  test("matches a tag stored without `v` prefix", () => {
    expect(findVersion(SAMPLE_MANIFEST, "0.7.0")?.tag).toBe("0.7.0");
  });

  test("matches the same tag with a `v` prefix on input", () => {
    expect(findVersion(SAMPLE_MANIFEST, "v0.7.0")?.tag).toBe("0.7.0");
  });

  test("matches the rolling nightly tag literally", () => {
    expect(findVersion(SAMPLE_MANIFEST, "nightly")?.tag).toBe("nightly");
  });

  test("returns the legacy-named binaries for pre-0.7.0 entries", () => {
    // Older archives ship `ScriptLoader` / `TestRunner` (no Goccia prefix) —
    // the manifest preserves the original archive filename so the API can
    // spawn the right binary without per-version naming logic.
    const entry = findVersion(SAMPLE_MANIFEST, "0.6.1");
    expect(entry?.binaries).toEqual({
      loader: "0.6.1/ScriptLoader",
      testRunner: "0.6.1/TestRunner",
    });
  });

  test("returns null for a tag not in the manifest", () => {
    expect(findVersion(SAMPLE_MANIFEST, "v999.0.0")).toBeNull();
    expect(findVersion(SAMPLE_MANIFEST, "")).toBeNull();
  });

  test("does NOT confuse `v0.7` with `v0.7.0`", () => {
    expect(findVersion(SAMPLE_MANIFEST, "0.7")).toBeNull();
  });

  test("returns null when the manifest has no versions", () => {
    const empty: VendorManifest = { defaultVersion: "nightly", versions: [] };
    expect(findVersion(empty, "nightly")).toBeNull();
  });
});

describe("isFlagSupported", () => {
  test("ignores positional args (the test file path passes through)", () => {
    expect(isFlagSupported(MODERN_FEATURES, "/tmp/x.js", "loader")).toBe(true);
    expect(
      isFlagSupported(MODERN_FEATURES, "<inline-test.js>", "testRunner"),
    ).toBe(true);
  });

  test("returns true when no features are present (legacy manifest)", () => {
    // A manifest from before the probe was added: don't filter, preserves
    // pre-feature behavior.
    expect(isFlagSupported(undefined, "--max-memory=1024", "loader")).toBe(
      true,
    );
  });

  test("looks up `--name` even when the arg carries `=value`", () => {
    expect(
      isFlagSupported(MODERN_FEATURES, "--max-memory=33554432", "loader"),
    ).toBe(true);
    expect(isFlagSupported(MODERN_FEATURES, "--mode=bytecode", "loader")).toBe(
      true,
    );
  });

  test("rejects modern infra flags on a legacy version", () => {
    // Picking 0.6.1 + the playground's standard sandbox flags: each of
    // these gets dropped silently, the engine sees only what it understands.
    expect(
      isFlagSupported(LEGACY_061_FEATURES, "--max-memory=33554432", "loader"),
    ).toBe(false);
    expect(
      isFlagSupported(LEGACY_061_FEATURES, "--max-instructions=1000", "loader"),
    ).toBe(false);
    expect(
      isFlagSupported(LEGACY_061_FEATURES, "--stack-size=2000", "loader"),
    ).toBe(false);
    expect(
      isFlagSupported(LEGACY_061_FEATURES, "--allowed-host", "loader"),
    ).toBe(false);
  });

  test("accepts flags the legacy version DOES advertise", () => {
    expect(isFlagSupported(LEGACY_061_FEATURES, "--asi", "loader")).toBe(true);
    expect(
      isFlagSupported(LEGACY_061_FEATURES, "--mode=bytecode", "loader"),
    ).toBe(true);
    expect(
      isFlagSupported(LEGACY_061_FEATURES, "--timeout=5000", "loader"),
    ).toBe(true);
    expect(
      isFlagSupported(LEGACY_061_FEATURES, "--output=json", "loader"),
    ).toBe(true);
  });

  test("loader and testRunner sets are independent", () => {
    // testRunner has `--no-progress`, loader doesn't (it's a runner-only flag).
    expect(
      isFlagSupported(LEGACY_061_FEATURES, "--no-progress", "loader"),
    ).toBe(false);
    expect(
      isFlagSupported(LEGACY_061_FEATURES, "--no-progress", "testRunner"),
    ).toBe(true);
  });
});

describe("listPlaygroundVersions", () => {
  test("preserves manifest order — newest stable first, nightly last", () => {
    expect(listPlaygroundVersions(SAMPLE_MANIFEST)).toEqual([
      "0.7.0",
      "0.6.1",
      "0.5.1",
      "nightly",
    ]);
  });

  test("returns an empty array for an empty manifest", () => {
    expect(
      listPlaygroundVersions({ defaultVersion: "nightly", versions: [] }),
    ).toEqual([]);
  });

  test("includes nightly when it's the only entry", () => {
    expect(
      listPlaygroundVersions({
        defaultVersion: "nightly",
        versions: [
          {
            tag: "nightly",
            isPrerelease: true,
            binaries: {
              loader: "nightly/GocciaScriptLoader",
              testRunner: "nightly/GocciaTestRunner",
            },
          },
        ],
      }),
    ).toEqual(["nightly"]);
  });
});
