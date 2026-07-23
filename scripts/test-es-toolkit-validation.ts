#!/usr/bin/env bun

import { mkdtempSync, readFileSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import {
  classifySemanticProbe,
  downloadTarball,
  extractVerifiedPackage,
  integrityCacheKey,
  matchesKnownBytecodeDivergence,
  parseSingleMarker,
  resolveArchiveEntry,
  validateTarballUrl,
  verifyIntegrity,
} from "./run_es_toolkit_validation";

function assert(condition: unknown, message: string): asserts condition {
  if (!condition) throw new Error(message);
}

function assertThrows(action: () => unknown, pattern: RegExp): void {
  try {
    action();
  } catch (error) {
    const message = error instanceof Error ? error.message : String(error);
    assert(pattern.test(message), `unexpected error: ${message}`);
    return;
  }
  throw new Error("expected action to throw");
}

async function assertRejects(action: () => Promise<unknown>, pattern: RegExp): Promise<void> {
  try {
    await action();
  } catch (error) {
    const message = error instanceof Error ? error.message : String(error);
    assert(pattern.test(message), `unexpected error: ${message}`);
    return;
  }
  throw new Error("expected action to reject");
}

const approvedUrl = "https://registry.npmjs.org/es-toolkit/-/es-toolkit-1.49.0.tgz";
validateTarballUrl(approvedUrl);
for (const rejectedUrl of [
  "http://registry.npmjs.org/es-toolkit.tgz",
  "https://example.com/es-toolkit.tgz",
  "file:///etc/passwd",
  "https://registry.npmjs.org.evil.example/es-toolkit.tgz",
  "https://registry.npmjs.org@evil.example/es-toolkit.tgz",
]) {
  assertThrows(() => validateTarballUrl(rejectedUrl), /must use https:\/\/registry\.npmjs\.org/);
}

const originalFetch = globalThis.fetch;
try {
  const requests: string[] = [];
  globalThis.fetch = (async (input: string | URL | Request) => {
    const url = String(input);
    requests.push(url);
    if (requests.length === 1) {
      return new Response(null, {
        status: 302,
        headers: { location: "/es-toolkit/-/redirected.tgz" },
      });
    }
    return new Response("redirected package");
  }) as typeof fetch;
  const downloaded = await downloadTarball(approvedUrl);
  assert(new TextDecoder().decode(downloaded) === "redirected package", "approved redirect must download");
  assert(requests.length === 2, "approved redirect must make exactly two requests");

  globalThis.fetch = (async () => new Response(null, {
    status: 302,
    headers: { location: "https://example.com/es-toolkit.tgz" },
  })) as typeof fetch;
  await assertRejects(() => downloadTarball(approvedUrl), /must use https:\/\/registry\.npmjs\.org/);

  let redirectRequests = 0;
  globalThis.fetch = (async () => {
    redirectRequests++;
    return new Response(null, {
      status: 302,
      headers: { location: approvedUrl },
    });
  }) as typeof fetch;
  await assertRejects(() => downloadTarball(approvedUrl), /exceeded 10 redirects/);
  assert(redirectRequests === 11, "redirect limit must stop after 11 requests");
} finally {
  globalThis.fetch = originalFetch;
}

const integrityData = new TextEncoder().encode("pinned package");
const integrity = `sha512-${new Bun.CryptoHasher("sha512").update(integrityData).digest("base64")}`;
verifyIntegrity(integrityData, integrity);
assertThrows(() => verifyIntegrity(new TextEncoder().encode("changed"), integrity), /does not match/);
assert(
  integrityCacheKey(integrity) === `es-toolkit-${new Bun.CryptoHasher("sha256").update(integrity).digest("hex")}`,
  "cache key must derive from the pinned integrity",
);

const extractionDirectory = mkdtempSync(join(tmpdir(), "goccia-es-toolkit-extraction-test-"));
try {
  const archive = new Bun.Archive({
    "package/package.json": JSON.stringify({ name: "es-toolkit", version: "1.49.0" }),
    "package/dist/probe.mjs": "export const value = 1;",
  }, { compress: "gzip" });
  const packageRoot = await extractVerifiedPackage(await archive.bytes(), extractionDirectory);
  assert(JSON.parse(readFileSync(join(packageRoot, "package.json"), "utf8")).name === "es-toolkit", "safe archive must extract");
  for (const unsafePath of ["../escape", "/absolute", "C:/absolute", "..\\escape"]) {
    assertThrows(() => resolveArchiveEntry(extractionDirectory, unsafePath), /unsafe path/);
  }
} finally {
  rmSync(extractionDirectory, { recursive: true, force: true });
}

const probe = {
  id: "example",
  file: "example.mjs",
  knownBytecodeDivergence: {
    errorPattern: "^ReferenceError: Cannot access '.+' before initialization$",
    reason: "known bytecode boundary",
  },
};
const completedRun = (mode: "interpreted" | "bytecode", status: "pass" | "fail", error?: string) => ({
  mode,
  transport: "completed" as const,
  exitCode: status === "pass" ? 0 : 1,
  stdout: "",
  stderr: "",
  marker: { id: "example", status, ...(error ? { error } : {}) },
  markerError: null,
});
const passingRuns = {
  interpreted: completedRun("interpreted", "pass"),
  bytecode: completedRun("bytecode", "pass"),
};
assert(classifySemanticProbe(probe, passingRuns)[0] === "semantic-pass", "matching passes must be semantic passes");
const failingRuns = {
  interpreted: completedRun("interpreted", "fail", "same semantic failure"),
  bytecode: completedRun("bytecode", "fail", "same semantic failure"),
};
assert(classifySemanticProbe(probe, failingRuns)[0] === "semantic-failure", "matching failures must be semantic failures");
const divergentRuns = {
  interpreted: completedRun("interpreted", "pass"),
  bytecode: completedRun("bytecode", "fail", "ReferenceError: Cannot access 'x' before initialization"),
};
const classification = classifySemanticProbe(probe, divergentRuns)[0];
assert(classification === "bytecode-divergence", "different mode outcomes must be bytecode divergence");
assert(matchesKnownBytecodeDivergence(probe, classification, divergentRuns), "exact known divergence must match");
const missingMarkerRuns = {
  ...passingRuns,
  bytecode: {
    ...passingRuns.bytecode,
    marker: null,
    markerError: "expected one marker, found 0",
  },
};
assert(
  classifySemanticProbe(probe, missingMarkerRuns)[0] === "bytecode-divergence",
  "a marker from only one mode must be bytecode divergence",
);
const mismatchedMarkerRuns = {
  ...passingRuns,
  bytecode: {
    ...passingRuns.bytecode,
    marker: { id: "different-probe", status: "pass" },
  },
};
assert(
  classifySemanticProbe(probe, mismatchedMarkerRuns)[0] === "harness-failure",
  "mismatched marker ids must be harness failures",
);
const timedOutRuns = {
  ...passingRuns,
  interpreted: {
    ...passingRuns.interpreted,
    transport: "timeout" as const,
    exitCode: null,
    marker: null,
    markerError: "probe exceeded 1s",
  },
};
assert(classifySemanticProbe(probe, timedOutRuns)[0] === "harness-failure", "timeouts must be harness failures");

assert(parseSingleMarker("Marker:{\"ok\":true}\n", "Marker:")[0]?.ok === true, "single marker must parse");
assert(parseSingleMarker("", "Marker:")[1]?.includes("found 0"), "missing marker must be reported");

console.log("es-toolkit validation runner checks passed");
