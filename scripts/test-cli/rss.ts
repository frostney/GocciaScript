/**
 * Peak resident-set-size measurement for a child process.
 *
 * The memory-budget sections in test-cli-apps.ts exist to prove that a refused
 * allocation is refused *before* it is committed. An exit-code assertion cannot
 * prove that: a gate that fired only after the process had already grown to
 * 700 MB would still print the same error and still exit non-zero, so a test
 * that checks only the message passes in exactly the case it was written to
 * catch. Resident memory is the observable that distinguishes the two, so
 * these helpers make it assertable.
 *
 * There is no portable API for a child's peak RSS, so two mechanisms are tried
 * in order and the working one is cached for the run:
 *
 * - `/usr/bin/time`, which reports the child's `ru_maxrss`. Flag and units
 *   differ by platform — BSD `-l` prints bytes, GNU `-v` prints kilobytes —
 *   and GNU time is a separate package that a minimal Linux image may not
 *   carry, so its presence is probed once against `/bin/echo` rather than
 *   assumed. Both variants exit with the child's status, which callers rely on.
 * - Polling `ps -o rss=` (kilobytes on macOS and Linux alike) from the parent
 *   while the child runs, keeping the maximum. Sampling can miss a spike
 *   between polls, which is the safe direction for an upper-bound assertion
 *   and the unsafe one for a lower-bound assertion — noted at the one place
 *   that asserts a lower bound.
 *
 * Windows has neither, so there `peakRssBytes` is null and callers fall back to
 * asserting the refusal alone. That is a real coverage gap on one platform
 * rather than a silent one: `assertPeakRssBelow` prints when it skips, and
 * treats an unmeasurable run on any *other* platform as a failure, so a broken
 * measurement cannot quietly turn these assertions into no-ops.
 */

export type PeakRssRun = {
  exitCode: number;
  /** stdout and stderr concatenated, including any `/usr/bin/time` report. */
  output: string;
  peakRssBytes: number | null;
  method: "time" | "sampler" | "unsupported";
};

type TimeSpec = {
  prefix: string[];
  parse: (output: string) => number | null;
};

const MIB = 1024 * 1024;

export function formatMib(bytes: number): string {
  return `${(bytes / MIB).toFixed(1)} MiB`;
}

function timeSpecForPlatform(): TimeSpec {
  if (process.platform === "darwin") {
    return {
      prefix: ["/usr/bin/time", "-l"],
      // BSD time prints "<bytes>  maximum resident set size".
      parse: (output) => {
        const match = output.match(/^\s*(\d+)\s+maximum resident set size/m);
        return match ? Number(match[1]) : null;
      },
    };
  }
  return {
    prefix: ["/usr/bin/time", "-v"],
    // GNU time prints "Maximum resident set size (kbytes): <kb>".
    parse: (output) => {
      const match = output.match(/Maximum resident set size \(kbytes\):\s*(\d+)/);
      return match ? Number(match[1]) * 1024 : null;
    },
  };
}

let probedTimeSpec: TimeSpec | null | undefined;

function usableTimeSpec(): TimeSpec | null {
  if (probedTimeSpec !== undefined) return probedTimeSpec;
  const spec = timeSpecForPlatform();
  try {
    const probe = Bun.spawnSync([...spec.prefix, "/bin/echo", "probe"], {
      stdout: "pipe",
      stderr: "pipe",
    });
    const output = probe.stdout.toString() + probe.stderr.toString();
    probedTimeSpec = probe.exitCode === 0 && spec.parse(output) !== null ? spec : null;
  } catch {
    probedTimeSpec = null;
  }
  return probedTimeSpec;
}

async function runSampled(argv: string[]): Promise<PeakRssRun> {
  const child = Bun.spawn(argv, { stdout: "pipe", stderr: "pipe" });
  // Drain both pipes concurrently with the run; a full pipe buffer would
  // deadlock a child that outlives it.
  const stdout = new Response(child.stdout).text();
  const stderr = new Response(child.stderr).text();

  let peak = 0;
  const sample = () => {
    const ps = Bun.spawnSync(["ps", "-o", "rss=", "-p", String(child.pid)], {
      stdout: "pipe",
      stderr: "pipe",
    });
    const kilobytes = Number.parseInt(ps.stdout.toString().trim(), 10);
    if (Number.isFinite(kilobytes) && kilobytes > 0) peak = Math.max(peak, kilobytes * 1024);
  };

  sample();
  const timer = setInterval(sample, 15);
  const exitCode = await child.exited;
  clearInterval(timer);

  return {
    exitCode,
    output: (await stdout) + (await stderr),
    peakRssBytes: peak > 0 ? peak : null,
    method: "sampler",
  };
}

/** Runs `argv` to completion and reports its peak RSS where the platform allows. */
export async function runWithPeakRss(argv: string[]): Promise<PeakRssRun> {
  if (process.platform === "win32") {
    const proc = Bun.spawnSync(argv, { stdout: "pipe", stderr: "pipe" });
    return {
      exitCode: proc.exitCode,
      output: proc.stdout.toString() + proc.stderr.toString(),
      peakRssBytes: null,
      method: "unsupported",
    };
  }

  const spec = usableTimeSpec();
  if (spec) {
    const proc = Bun.spawnSync([...spec.prefix, ...argv], { stdout: "pipe", stderr: "pipe" });
    const output = proc.stdout.toString() + proc.stderr.toString();
    return { exitCode: proc.exitCode, output, peakRssBytes: spec.parse(output), method: "time" };
  }

  return runSampled(argv);
}

function requireMeasurement(run: PeakRssRun, label: string): number | null {
  if (run.peakRssBytes !== null) return run.peakRssBytes;
  if (run.method === "unsupported") {
    console.log(`  (peak RSS is not measurable on ${process.platform}; ${label}: refusal only)`);
    return null;
  }
  throw new Error(
    `Could not measure peak RSS for ${label} (mechanism: ${run.method}). ` +
      `Neither /usr/bin/time nor 'ps -o rss=' produced a reading, so this ` +
      `assertion would silently become a no-op. Fix the measurement rather ` +
      `than dropping the assertion.`,
  );
}

/**
 * Asserts the run never grew past `limitBytes`.
 *
 * This is the "refused before it happens" assertion: the limit is not the
 * budget itself — the process has an interpreter, a heap and the allocations
 * it made before the refusal — but a ceiling far below what the same script
 * reaches when nothing stops it.
 */
export function assertPeakRssBelow(run: PeakRssRun, label: string, limitBytes: number): void {
  const peak = requireMeasurement(run, label);
  if (peak === null) return;
  if (peak > limitBytes) {
    throw new Error(
      `${label} peaked at ${formatMib(peak)} resident, past the ${formatMib(limitBytes)} ` +
        `ceiling this assertion holds. The refusal happened, but not before the ` +
        `memory was committed — which is the whole point of the gate.`,
    );
  }
}

/**
 * Asserts the run grew past `floorBytes`.
 *
 * Used to pin a *documented* hole rather than a guarantee, so that closing the
 * hole fails loudly and forces the documentation to be revisited. Sampling can
 * undershoot, so the floor must sit far below the observed peak.
 */
export function assertPeakRssAbove(run: PeakRssRun, label: string, floorBytes: number): void {
  const peak = requireMeasurement(run, label);
  if (peak === null) return;
  if (peak < floorBytes) {
    throw new Error(
      `${label} peaked at only ${formatMib(peak)} resident, below the ${formatMib(floorBytes)} ` +
        `floor this assertion holds. That is good news, not a bug: it means the ` +
        `aggregate gap documented in ADR 0106 Amendment 1 has narrowed or closed. ` +
        `Re-measure, update the ADR and docs/garbage-collector.md, then move this floor.`,
    );
  }
}
