#!/usr/bin/env python3
"""Build every full-CI program with bounded concurrency and isolated FPC units."""

import argparse
from concurrent.futures import ThreadPoolExecutor
from pathlib import Path
import subprocess
import sys
import tempfile
import time


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--jobs", type=int, required=True)
    parser.add_argument("cpu")
    parser.add_argument("os")
    parser.add_argument("flags", nargs=argparse.REMAINDER)
    args = parser.parse_args()
    if args.jobs < 1:
        parser.error("--jobs must be positive")

    root = Path.cwd()
    entrypoints = sorted(root.glob("source/app/*.dpr"))
    tests = sorted(
        path
        for tree in ("source/units", "source/shared", "source/app")
        for path in root.joinpath(tree).rglob("*.Test.pas")
    )
    compliance = sorted(root.glob("source/app/compliance/*.dpr"))
    if not entrypoints or not tests or not compliance:
        parser.error("expected application entrypoints, Pascal tests and compliance runners")
    sources = entrypoints + tests + compliance
    names = [source.stem for source in sources]
    if len(set(names)) != len(names):
        parser.error("program basenames must be unique in the build output directory")

    build = root / "build"
    logs = build / "ci-logs"
    logs.mkdir(parents=True, exist_ok=True)
    scratch = build / "ci-compiled"
    scratch.mkdir(exist_ok=True)
    flags = args.flags[1:] if args.flags[:1] == ["--"] else args.flags
    common = [
        "fpc", f"-P{args.cpu}", f"-T{args.os}",
        "-O4", "-dPRODUCTION", "-Xs", "-CX", "-XX", "-B",
        *[f"-Fu{root / path}" for path in (
            "source/units", "source/generated", "source/shared", "source/app",
            "souffle",
        )],
        *[f"-Fi{root / path}" for path in (
            "source/units", "source/shared", "souffle",
        )],
        f"-FE{build}", *flags,
    ]

    def compile_program(source):
        # FPC's PPUs, resource lists and linker scratch files must not be shared
        # between programs, even when the compiler options are identical.
        started = time.monotonic()
        log = logs / f"{source.stem}.log"
        # These units cannot be reused by another program. Release their disk
        # space after linking, including when the compiler fails.
        with tempfile.TemporaryDirectory(prefix=source.stem + "-", dir=scratch) as units:
            command = common + [f"-FU{units}"]
            if source in compliance:
                command.append(f"-Fu{source.parent}")
            command.append(str(source))
            with log.open("w") as output:
                result = subprocess.run(command, cwd=units, stdout=output, stderr=subprocess.STDOUT)
        elapsed = time.monotonic() - started
        # Keep compiler output off the runner's live log pipe: a full build can
        # emit hundreds of thousands of repeated diagnostics. On failure retain
        # the complete diagnostic, and on success report the compiler's totals.
        output = log.read_text(errors="replace")
        totals = [line for line in output.splitlines() if "lines compiled," in line]
        message = f"{source.relative_to(root)}: {elapsed:.1f}s, exit {result.returncode}"
        if result.returncode:
            message += "\n" + output
        elif totals:
            message += "\n" + "\n".join(totals)
        print(message, flush=True)
        return result.returncode

    print(f"Building {len(entrypoints)} applications, {len(tests)} Pascal tests and "
          f"{len(compliance)} compliance runners with {args.jobs} compiler processes", flush=True)
    started = time.monotonic()
    with ThreadPoolExecutor(max_workers=args.jobs) as pool:
        results = list(pool.map(compile_program, sources))
    failures = sum(result != 0 for result in results)
    print(f"Built {len(sources)} programs in {time.monotonic() - started:.1f}s; "
          f"{failures} failed", flush=True)
    return int(failures != 0)


if __name__ == "__main__":
    sys.exit(main())
