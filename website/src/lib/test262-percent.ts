export function compatibilityPercent(passed: number, totalRun: number): string {
  if (totalRun <= 0) return "0.0%";
  return `${((passed / totalRun) * 100).toFixed(1)}%`;
}

type ModeFigures = { run: number; passed: number };

/** Per-mode pass rates for prose, e.g. `non-strict 98.0%, strict 97.5%`.
 *  Null when the report does not record modes: the figures are unavailable,
 *  not derivable from the combined total. */
export function compatibilityModeRates(
  byMode: { "non-strict": ModeFigures; strict: ModeFigures } | null | undefined,
): string | null {
  if (!byMode) return null;
  return `non-strict ${compatibilityPercent(
    byMode["non-strict"].passed,
    byMode["non-strict"].run,
  )}, strict ${compatibilityPercent(byMode.strict.passed, byMode.strict.run)}`;
}
