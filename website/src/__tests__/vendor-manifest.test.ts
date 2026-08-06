import { describe, expect, test } from "bun:test";
import { spawnSync } from "node:child_process";
import { mkdtempSync, readFileSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import path from "node:path";
import {
  checkVendorManifestFloor,
  findVersion,
  isFlagSupported,
  isPublicExecutionSafe,
  listPlaygroundVersions,
  normalizeManifest,
  parseAdvertisedFlags,
  pickVendorManifestSource,
  resolveAsiFlag,
  resolvePublicDefaultVersion,
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
    "--no-host-filesystem",
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
    "--no-host-filesystem",
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
  defaultVersion: "0.7.0",
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

describe("public execution safety", () => {
  test("requires the filesystem-disable capability on both binaries", () => {
    expect(isPublicExecutionSafe(SAMPLE_MANIFEST.versions[0])).toBe(true);
    expect(isPublicExecutionSafe(SAMPLE_MANIFEST.versions[1])).toBe(false);
    expect(isPublicExecutionSafe(SAMPLE_MANIFEST.versions[2])).toBe(false);
  });

  test("falls forward to the first safe version when the configured default is unsafe", () => {
    expect(
      resolvePublicDefaultVersion({
        ...SAMPLE_MANIFEST,
        defaultVersion: "0.6.1",
      }),
    ).toBe("0.7.0");
  });

  test("preserves an unmanifested default for override and local probing", () => {
    expect(
      resolvePublicDefaultVersion({
        defaultVersion: "nightly",
        versions: [],
      }),
    ).toBe("nightly");
  });
});

describe("parseAdvertisedFlags", () => {
  test("extracts unique long options from help output", () => {
    expect(
      parseAdvertisedFlags(
        "Usage: runner [--no-host-filesystem] --timeout=<ms> --timeout=10",
      ),
    ).toEqual(["--no-host-filesystem", "--timeout"]);
  });
});

describe("listPlaygroundVersions", () => {
  test("preserves manifest order while hiding engines without the boundary", () => {
    expect(listPlaygroundVersions(SAMPLE_MANIFEST)).toEqual([
      "0.7.0",
      "nightly",
    ]);
  });

  test("returns an empty array for an empty manifest", () => {
    expect(
      listPlaygroundVersions({ defaultVersion: "nightly", versions: [] }),
    ).toEqual([]);
  });

  test("hides an unprobed nightly entry", () => {
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
    ).toEqual([]);
  });
});

describe("resolveAsiFlag", () => {
  // ASI's engine flag was renamed `--asi` -> `--compat-asi` after 0.7.x.
  const NEW_FEATURES: VendorFeatureSet = {
    loader: ["--compat-asi", "--compat-var", "--mode"],
    testRunner: ["--compat-asi", "--mode"],
  };

  test("uses --compat-asi when the binary advertises it (0.8.0+ / nightly)", () => {
    expect(resolveAsiFlag(NEW_FEATURES, "loader")).toBe("--compat-asi");
    expect(resolveAsiFlag(NEW_FEATURES, "testRunner")).toBe("--compat-asi");
  });

  test("falls back to --asi for pre-0.8.0 binaries that only advertise it", () => {
    expect(resolveAsiFlag(LEGACY_061_FEATURES, "loader")).toBe("--asi");
    expect(resolveAsiFlag(MODERN_FEATURES, "loader")).toBe("--asi");
  });

  test("defaults to the modern name when features are unprobed (local dev)", () => {
    expect(resolveAsiFlag(undefined, "loader")).toBe("--compat-asi");
  });

  test("returns null when the binary advertises neither (server omits ASI)", () => {
    const neither: VendorFeatureSet = { loader: ["--mode"], testRunner: [] };
    expect(resolveAsiFlag(neither, "loader")).toBeNull();
  });

  test("resolves loader and testRunner independently", () => {
    const mixed: VendorFeatureSet = {
      loader: ["--compat-asi"],
      testRunner: ["--asi"],
    };
    expect(resolveAsiFlag(mixed, "loader")).toBe("--compat-asi");
    expect(resolveAsiFlag(mixed, "testRunner")).toBe("--asi");
  });
});

describe("checkVendorManifestFloor", () => {
  const NIGHTLY_ONLY: VendorManifest = {
    defaultVersion: "nightly",
    versions: [
      {
        tag: "nightly",
        isPrerelease: true,
        binaries: {
          loader: "nightly/GocciaScriptLoader",
          testRunner: "nightly/GocciaTestRunner",
        },
        features: MODERN_FEATURES,
      },
    ],
  };

  test("accepts a set with a public-execution-safe stable release", () => {
    expect(
      checkVendorManifestFloor({
        defaultVersion: "0.11.0",
        versions: [
          {
            tag: "0.11.0",
            binaries: {
              loader: "0.11.0/GocciaScriptLoader",
              testRunner: "0.11.0/GocciaTestRunner",
            },
            features: MODERN_FEATURES,
          },
          ...NIGHTLY_ONLY.versions,
        ],
      }),
    ).toBeNull();
  });

  test("rejects a nightly-only set — the release regression that shipped 0.11.0", () => {
    // Every precedence-picked stable failed to vendor. The playground would
    // deploy offering `nightly` and nothing else, with no build-time signal.
    expect(checkVendorManifestFloor(NIGHTLY_ONLY)?.code).toBe(
      "NO_STABLE_VENDORED",
    );
  });

  test("rejects an empty set", () => {
    expect(
      checkVendorManifestFloor({ defaultVersion: "nightly", versions: [] })
        ?.code,
    ).toBe("NO_STABLE_VENDORED");
  });

  test("rejects a set where nothing advertises the sandbox boundary", () => {
    // `listPlaygroundVersions` would return [] — a version picker with no
    // entries at all.
    expect(
      checkVendorManifestFloor({
        defaultVersion: "0.6.1",
        versions: [
          {
            tag: "0.6.1",
            binaries: {
              loader: "0.6.1/ScriptLoader",
              testRunner: "0.6.1/TestRunner",
            },
            features: LEGACY_061_FEATURES,
          },
        ],
      })?.code,
    ).toBe("NO_PUBLIC_SAFE_ENGINE");
  });

  test("rejects a set whose only safe engine is a prerelease", () => {
    // Stables vendored fine but all predate the #1057 boundary, so the
    // dropdown is nightly-only again — same user-visible failure.
    expect(
      checkVendorManifestFloor({
        defaultVersion: "0.10.0",
        versions: [
          {
            tag: "0.10.0",
            binaries: {
              loader: "0.10.0/GocciaScriptLoader",
              testRunner: "0.10.0/GocciaTestRunner",
            },
            features: LEGACY_061_FEATURES,
          },
          ...NIGHTLY_ONLY.versions,
        ],
      })?.code,
    ).toBe("NO_PUBLIC_SAFE_STABLE");
  });

  test("reports a message naming the reason, for the failing build log", () => {
    expect(checkVendorManifestFloor(NIGHTLY_ONLY)?.message).toContain(
      "no stable release could be vendored",
    );
  });
});

describe("pickVendorManifestSource", () => {
  const GENERATED = {
    defaultVersion: "0.11.0",
    versions: [
      {
        tag: "0.11.0",
        binaries: {
          loader: "0.11.0/GocciaScriptLoader",
          testRunner: "0.11.0/GocciaTestRunner",
        },
        features: MODERN_FEATURES,
      },
    ],
  };

  test("falls back to the static import when vendor/ is unreadable", () => {
    // The playground page's bundle: `vendor/**` is traced into the API routes
    // only, so the on-disk read returns null and the page must still know the
    // versions. This is the bug that left the picker showing `nightly`.
    expect(
      pickVendorManifestSource(null, GENERATED).versions.map((v) => v.tag),
    ).toEqual(["0.11.0"]);
  });

  test("prefers the on-disk manifest, which is the truth about spawnable binaries", () => {
    const disk = {
      defaultVersion: "nightly",
      versions: [
        {
          tag: "nightly",
          binaries: {
            loader: "nightly/GocciaScriptLoader",
            testRunner: "nightly/GocciaTestRunner",
          },
          features: MODERN_FEATURES,
        },
      ],
    };
    expect(pickVendorManifestSource(disk, GENERATED).versions[0].tag).toBe(
      "nightly",
    );
  });

  test("ignores an empty or malformed disk manifest", () => {
    expect(
      pickVendorManifestSource({ versions: [] }, GENERATED).versions,
    ).toHaveLength(1);
    expect(
      pickVendorManifestSource("not json at all", GENERATED).versions,
    ).toHaveLength(1);
  });

  test("returns the empty manifest when neither source has entries", () => {
    expect(pickVendorManifestSource(null, null)).toEqual({
      defaultVersion: "nightly",
      versions: [],
    });
  });
});

describe("generated manifest artifact", () => {
  const generatedPath = path.join(
    import.meta.dir,
    "..",
    "generated",
    "vendor-manifest.json",
  );

  test("exists and normalizes, whether empty or freshly vendored", () => {
    // `postinstall` guarantees the file exists (empty), `prebuild` fills it
    // in. Either state has to survive `normalizeManifest`, because the static
    // import is a hard build dependency of the playground page.
    const raw = JSON.parse(readFileSync(generatedPath, "utf8"));
    const manifest = normalizeManifest(raw);
    expect(typeof manifest.defaultVersion).toBe("string");
    expect(Array.isArray(manifest.versions)).toBe(true);
  });

  test("postinstall writes an empty placeholder, and never clobbers a vendored one", async () => {
    const dir = mkdtempSync(path.join(tmpdir(), "goccia-generated-"));
    const target = path.join(dir, "nested", "vendor-manifest.json");
    const run = () =>
      spawnSync(
        "bun",
        [
          path.join(
            import.meta.dir,
            "..",
            "..",
            "scripts",
            "ensure-generated-manifest.ts",
          ),
        ],
        {
          encoding: "utf8",
          env: { ...process.env, GOCCIA_GENERATED_MANIFEST_PATH: target },
        },
      );

    expect(run().status).toBe(0);
    expect(normalizeManifest(JSON.parse(readFileSync(target, "utf8")))).toEqual(
      {
        defaultVersion: "nightly",
        versions: [],
      },
    );

    // A `bun install` after a vendoring run must not throw the versions away.
    writeFileSync(
      target,
      JSON.stringify({ defaultVersion: "0.11.0", versions: [] }),
    );
    expect(run().status).toBe(0);
    expect(JSON.parse(readFileSync(target, "utf8")).defaultVersion).toBe(
      "0.11.0",
    );
    rmSync(dir, { recursive: true, force: true });
  });

  test("the server loader imports it statically", () => {
    // Regression guard for the original fault: a `process.cwd()` read is
    // invisible to the bundler's file tracing, so the playground page shipped
    // without any manifest. The static import is what makes the manifest
    // reachable from every bundle by construction — a runtime-only read here
    // would silently reintroduce the empty version picker.
    const source = readFileSync(
      path.join(import.meta.dir, "..", "lib", "vendor-manifest-server.ts"),
      "utf8",
    );
    expect(source).toContain('from "@/generated/vendor-manifest.json"');
  });
});
