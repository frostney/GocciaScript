#!/usr/bin/env bun

import { mkdtempSync, readFileSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import {
  classifySemanticProbe,
  downloadTarball,
  extractVerifiedPackage,
  integrityCacheKey,
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

const probe = { id: "example", file: "example.mjs" };
const completedRun = (status: "pass" | "fail", error?: string) => ({
  transport: "completed" as const,
  exitCode: status === "pass" ? 0 : 1,
  stdout: "",
  stderr: "",
  marker: { id: "example", status, ...(error ? { error } : {}) },
  markerError: null,
});
const passingRun = completedRun("pass");
assert(classifySemanticProbe(probe, passingRun)[0] === "semantic-pass", "a passing probe must be a semantic pass");
const failingRun = completedRun("fail", "semantic failure");
assert(classifySemanticProbe(probe, failingRun)[0] === "semantic-failure", "a failing probe must be a semantic failure");
const missingMarkerRun = {
  ...passingRun,
  marker: null,
  markerError: "expected one marker, found 0",
};
assert(
  classifySemanticProbe(probe, missingMarkerRun)[0] === "harness-failure",
  "a probe without a result marker must be a harness failure",
);
const mismatchedMarkerRun = {
  ...passingRun,
  marker: { id: "different-probe", status: "pass" },
};
assert(
  classifySemanticProbe(probe, mismatchedMarkerRun)[0] === "harness-failure",
  "mismatched marker ids must be harness failures",
);
const invalidStatusRun = {
  ...passingRun,
  marker: { id: "example", status: "skipped" },
};
assert(
  classifySemanticProbe(probe, invalidStatusRun)[0] === "harness-failure",
  "a marker status other than pass or fail must be a harness failure",
);
const timedOutRun = {
  ...passingRun,
  transport: "timeout" as const,
  exitCode: null,
  marker: null,
  markerError: "probe exceeded 1s",
};
assert(classifySemanticProbe(probe, timedOutRun)[0] === "harness-failure", "timeouts must be harness failures");
const crashedRun = { ...passingRun, exitCode: 139 };
assert(classifySemanticProbe(probe, crashedRun)[0] === "harness-failure", "native crashes must be harness failures");

assert(parseSingleMarker("Marker:{\"ok\":true}\n", "Marker:")[0]?.ok === true, "single marker must parse");
assert(parseSingleMarker("", "Marker:")[1]?.includes("found 0"), "missing marker must be reported");

console.log("es-toolkit validation runner checks passed");
