// Client-safe: no imports beyond the pure percent helper. The dashboard
// component ("use client") imports this module, so it must never pull in the
// Blob store, `next/cache` or `node:zlib` that `test262-dashboard.ts` uses.
import { compatibilityPercent } from "@/lib/test262-percent";

/** How a run executed the test: the same file runs once per mode it allows. */
export type Test262Mode = "non-strict" | "strict";

export const TEST262_MODES: readonly Test262Mode[] = ["non-strict", "strict"];

export const TEST262_MODE_LABELS: Record<Test262Mode, string> = {
  "non-strict": "Non-strict",
  strict: "Strict",
};

export type Test262ModeCounts = {
  run: number;
  passed: number;
  failed: number;
  wrapperInfra: number;
  timeouts: number;
};

/** Per-mode counts. Absent (`undefined`) on reports from before the runner
 *  recorded the mode; never derive it for those. */
export type Test262ModeBreakdown = Record<Test262Mode, Test262ModeCounts>;

/** The one rule for a mode's pass rate: `98.0%`, or `n/a` when the mode (or the
 *  whole breakdown) has no runs, so an empty mode never reads as `0.0%`. */
export function test262ModeRate(
  byMode: Test262ModeBreakdown | null | undefined,
  mode: Test262Mode,
): string {
  const counts = byMode?.[mode];
  return counts && counts.run > 0
    ? compatibilityPercent(counts.passed, counts.run)
    : "n/a";
}

/** Per-mode rates for prose, e.g. `non-strict 98.0%, strict 97.5%`. Null when
 *  the report does not record modes: the figures are unavailable, not
 *  derivable from the combined total. */
export function test262ModeRates(
  byMode: Test262ModeBreakdown | null | undefined,
  separator = ", ",
): string | null {
  if (!byMode) return null;
  return TEST262_MODES.map(
    (mode) =>
      `${TEST262_MODE_LABELS[mode].toLowerCase()} ${test262ModeRate(byMode, mode)}`,
  ).join(separator);
}
