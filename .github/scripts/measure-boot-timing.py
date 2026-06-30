#!/usr/bin/env python3
"""Measure empty-script engine-boot wall-clock for GocciaScriptLoader in both
execution modes and emit a small JSON summary the PR suite-timing comment
renders.

Boot wall-clock on a shared runner is dominated by a fixed process /
dynamic-loader floor, so we report the min (least-contended run) alongside the
median over many runs rather than a single timing. The two binaries' modes are
measured back-to-back so transient load affects both alike.

Usage: measure-boot-timing.py <loader> <output.json>
Env:   BOOT_TIMING_RUNS (default 60), BOOT_TIMING_WARMUP (default 10)
"""
import json
import os
import statistics
import subprocess
import sys
import tempfile
import time


def measure(loader: str, script: str, mode_args: list[str], runs: int, warmup: int) -> dict:
    samples = []
    for i in range(warmup + runs):
        start = time.perf_counter()
        subprocess.run(
            [loader, script, *mode_args],
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
        )
        elapsed_ms = (time.perf_counter() - start) * 1000.0
        if i >= warmup:
            samples.append(elapsed_ms)
    return {
        "minMs": round(min(samples), 2),
        "medianMs": round(statistics.median(samples), 2),
    }


def main() -> int:
    if len(sys.argv) != 3:
        print("usage: measure-boot-timing.py <loader> <output.json>", file=sys.stderr)
        return 2
    loader, out = sys.argv[1], sys.argv[2]
    runs = int(os.environ.get("BOOT_TIMING_RUNS", "60"))
    warmup = int(os.environ.get("BOOT_TIMING_WARMUP", "10"))

    with tempfile.TemporaryDirectory() as tmp:
        script = os.path.join(tmp, "empty.js")
        with open(script, "w", encoding="utf-8") as handle:
            handle.write("0;\n")

        report = {
            "runs": runs,
            "warmup": warmup,
            "interpreted": measure(loader, script, [], runs, warmup),
            "bytecode": measure(loader, script, ["--mode=bytecode"], runs, warmup),
        }

    with open(out, "w", encoding="utf-8") as handle:
        json.dump(report, handle, indent=2)
        handle.write("\n")
    print(json.dumps(report, indent=2))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
