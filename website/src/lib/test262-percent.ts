export function compatibilityPercent(passed: number, totalRun: number): string {
  if (totalRun <= 0) return "0.0%";
  return `${((passed / totalRun) * 100).toFixed(1)}%`;
}
