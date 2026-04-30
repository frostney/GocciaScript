/** Subset of the runner's `memory` object surfaced to the client. The runner
 *  emits a much richer payload (see Goccia.CLI.JSON.Reporter.pas), but the
 *  website only needs four numbers from `gc` (peak live, allocation count,
 *  GC count, freed-object count) plus the FPC heap delta. `memory` itself
 *  may be `null` when stats are off. */
export type MemoryJson = {
  gc?: {
    peakLiveBytes?: number;
    allocatedDuringRunBytes?: number;
    collections?: number;
    collectedObjects?: number;
  };
  heap?: {
    deltaAllocatedBytes?: number;
  };
};

/** Compact byte formatter for run-output tail lines (e.g. `124 KB`,
 *  `2.34 MB`). Uses KiB/MiB internally but labels them KB/MB to match how
 *  developer tools typically present memory. */
export function formatBytes(n: number): string {
  if (n < 1024) return `${n} B`;
  if (n < 1024 * 1024) return `${(n / 1024).toFixed(1)} KB`;
  return `${(n / (1024 * 1024)).toFixed(2)} MB`;
}

/** Build the ` · X KB GC peak · Y MB heap allocated · N GCs (M freed)`
 *  suffix that gets appended to a runner output's exit/timing line.
 *
 *  Returns "" when no data is available so callers can append the result
 *  unconditionally. Each segment is individually conditional:
 *
 *    - peak (GC live heap):    rendered when > 0
 *    - heap allocated (delta): rendered when > 0; can be 0 / negative for
 *                              short scripts where the FPC heap shrunk or
 *                              didn't grow, in which case it's silently
 *                              omitted rather than displayed as "0 B"
 *    - GC count + freed:       always rendered as a pair when count > 0;
 *                              showing one without the other is misleading
 */
export function formatMemorySegments(
  memory: MemoryJson | null | undefined,
): string {
  if (!memory) return "";

  const peakBytes = memory.gc?.peakLiveBytes;
  const heapAllocated = memory.heap?.deltaAllocatedBytes;
  const collections = memory.gc?.collections;
  const collectedObjects = memory.gc?.collectedObjects;

  let result = "";
  if (peakBytes !== undefined && peakBytes > 0) {
    result += ` · ${formatBytes(peakBytes)} GC peak`;
  }
  if (heapAllocated !== undefined && heapAllocated > 0) {
    result += ` · ${formatBytes(heapAllocated)} heap allocated`;
  }
  // GC count and freed-object count are always emitted together: showing
  // one without the other is misleading. If either is missing (e.g. the
  // normalize layer dropped a malformed field), skip the whole segment
  // rather than rendering a half-truth. `collectedObjects === 0` is a
  // valid value (a GC ran but freed nothing — all live objects still
  // reachable), so only `collections` is gated on `> 0`.
  if (
    collections !== undefined &&
    collectedObjects !== undefined &&
    collections > 0
  ) {
    result += ` · ${collections} GC${
      collections === 1 ? "" : "s"
    } (${collectedObjects.toLocaleString()} freed)`;
  }
  return result;
}
