#!/usr/bin/env bun
/**
 * test-cli-ffi-callback-slots.ts
 *
 * FFI callback slot exhaustion and reuse, checked in a process of its own.
 *
 * The callback slots (MAX_FFI_CALLBACK_SLOTS in
 * source/units/Goccia.FFI.CallbackSlots.pas) are shared by the whole process.
 * GocciaTestRunner runs the files of a directory run on worker threads of one
 * process, so a slot-exhaustion test under tests/ races every other FFI file:
 * a callback another worker holds makes one of its creates fail early, and
 * while it holds every slot, the other worker's callbacks fail instead. Run on
 * its own (the runner reports jobCount 1), the file is the only code taking
 * slots, so no other thread can claim the slot it releases, and the check can
 * assert that exactly that slot is handed out again.
 *
 * Needs the FFI fixture library (./fixtures/ffi/build.sh; the CI cli job
 * builds it). Run from the repository root:
 *
 *   bun run scripts/test-cli-ffi-callback-slots.ts
 */

import { existsSync, readFileSync, writeFileSync } from "fs";
import { join } from "path";

import { TESTRUNNER } from "./test-cli/binaries";
import { clean, mkdtemp } from "./test-cli/tmpdir";

const SLOT_LIMIT = 64;
const EXPECTED_TESTS = 2;
const EXPECTED_ASSERTIONS = 8;

const FIXTURE_DIR = "fixtures/ffi";
const FIXTURE_LIBRARY = join(
  FIXTURE_DIR,
  process.platform === "win32"
    ? "libfixture.dll"
    : process.platform === "darwin"
      ? "libfixture.dylib"
      : "libfixture.so",
);

const TEST_SOURCE = `describe("FFI callback slots", () => {
  const lib = FFI.open("./fixtures/ffi/libfixture" + FFI.suffix);
  const I32Callback = FFI.callback({ args: ["i32"], returns: "i32" });
  const invoke = lib.bind("ffi_v2_call_i32_callback", {
    args: [I32Callback, "i32"],
    returns: "i32",
  });

  afterAll(() => lib.close());

  // Creates callbacks until a create throws. One attempt more than the limit
  // keeps the loop finite even if the limit stops being enforced. The file has
  // the process to itself, but the tests do not depend on every slot starting
  // free: they need the table full and at least one slot of their own.
  const fillSlots = (callbacks) => {
    for (const unused of new Array(${SLOT_LIMIT + 1})) {
      try {
        callbacks.push(I32Callback.create((value) => value));
      } catch (error) {
        return error;
      }
    }
    return undefined;
  };

  const closeAll = (callbacks) => {
    for (const callback of callbacks) {
      if (!callback.closed) callback.close();
    }
  };

  test("throws a RangeError once every callback slot is taken", () => {
    const callbacks = [];

    try {
      const exhaustion = fillSlots(callbacks);

      expect(exhaustion).toBeInstanceOf(RangeError);
      expect(exhaustion.message).toBe(
        "FFI supports a maximum of ${SLOT_LIMIT} active callbacks per process",
      );
      expect(callbacks.length).toBeGreaterThan(0);
      expect(callbacks.length).toBeLessThanOrEqual(${SLOT_LIMIT});
    } finally {
      closeAll(callbacks);
    }
  });

  test("hands a released slot to the next callback", () => {
    const callbacks = [];

    try {
      expect(fillSlots(callbacks)).toBeInstanceOf(RangeError);

      // Every other slot is held, so the next create can only get this one.
      const released = callbacks[Math.floor(callbacks.length / 2)];
      const releasedAddress = released.address;
      released.close();
      const replacement = I32Callback.create((value) => value + 1);
      callbacks.push(replacement);

      expect(replacement.address).toBe(releasedAddress);
      expect(invoke(replacement, 41)).toBe(42);
      expect(() => I32Callback.create((value) => value)).toThrow(RangeError);
    } finally {
      closeAll(callbacks);
    }
  });
});
`;

type RunReport = {
  ok: boolean;
  jobCount: number;
  totalFiles: number;
  totalTests: number;
  passed: number;
  failed: number;
  skipped: number;
  suiteErrors: number;
  assertions: number;
};

if (!existsSync(FIXTURE_LIBRARY))
  throw new Error(
    `${FIXTURE_LIBRARY} is missing; build it with ./fixtures/ffi/build.sh before running this check`,
  );

const scratch = mkdtemp("goccia-ffi-callback-slots-");
const problems: string[] = [];
let output = "";

try {
  const testFile = join(scratch, "callback-slots.js");
  writeFileSync(testFile, TEST_SOURCE);

  console.log("FFI callback slot exhaustion and reuse...");
  const outPath = join(scratch, "report.json");
  const proc = Bun.spawnSync(
    [
      TESTRUNNER,
      testFile,
      `--allow-ffi=${FIXTURE_DIR}`,
      "--no-progress",
      `--output=${outPath}`,
    ],
    { stdout: "pipe", stderr: "pipe" },
  );
  output = `${proc.stdout.toString()}${proc.stderr.toString()}`;
  if (proc.exitCode !== 0 || !existsSync(outPath)) {
    problems.push(`exit code ${proc.exitCode}`);
  } else {
    const report = JSON.parse(readFileSync(outPath, "utf8")) as RunReport;
    if (!report.ok) problems.push("report is not ok");
    // The check is only meaningful while the file has the process to itself.
    if (report.jobCount !== 1) problems.push(`jobCount ${report.jobCount}, expected 1`);
    if (report.totalFiles !== 1) problems.push(`totalFiles ${report.totalFiles}, expected 1`);
    if (report.totalTests !== EXPECTED_TESTS || report.passed !== EXPECTED_TESTS)
      problems.push(
        `${report.passed}/${report.totalTests} tests passed, expected ${EXPECTED_TESTS}/${EXPECTED_TESTS}`,
      );
    if (report.failed !== 0 || report.skipped !== 0 || report.suiteErrors !== 0)
      problems.push(
        `failed ${report.failed}, skipped ${report.skipped}, suite errors ${report.suiteErrors}`,
      );
    if (report.assertions !== EXPECTED_ASSERTIONS)
      problems.push(`${report.assertions} assertions ran, expected ${EXPECTED_ASSERTIONS}`);
  }
} finally {
  clean(scratch);
}

if (problems.length > 0)
  throw new Error(`FFI callback slot check failed: ${problems.join("; ")}\n${output}`);
console.log("FFI callback slot exhaustion and reuse OK");
