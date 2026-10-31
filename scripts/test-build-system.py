#!/usr/bin/env python3
"""Exercise build.pas against real FPC programs in a disposable checkout.

Run on a Unix FPC host: python3 scripts/test-build-system.py
The compiler wrapper records process lifetimes, then invokes the real compiler.
"""
import json
import os
import resource
from pathlib import Path
import shutil
import subprocess
import tempfile

REPO = Path(__file__).resolve().parents[1]
FPC = shutil.which("fpc")
if not FPC:
    raise SystemExit("fpc is required")

with tempfile.TemporaryDirectory(prefix="goccia-build-check-") as directory:
    root = Path(directory)
    tool_source = root / "build-tool.pas"
    tool_source.write_text((REPO / "build.pas").read_text().replace("#!/", "//#!/", 1))
    subprocess.run([FPC, "-Fu./source/shared", f"-FU{root}", f"-FE{root}",
                    str(tool_source)], cwd=REPO, check=True, capture_output=True)
    tool = root / "build-tool"
    for folder in ("source/shared", "source/units", "source/app", "trace", "bin"):
        (root / folder).mkdir(parents=True, exist_ok=True)
    (root / "config.cfg").write_text("-FEbuild\n-FUbuild/compiled\n")
    for name in ("Alpha", "Beta", "Other"):
        (root / f"source/shared/{name}.Test.pas").write_text(
            f"program {name}; begin WriteLn('ran {name}'); end.\n")
    wrapper = root / "bin/fpc"
    # build.pas asks fpc for its target (-iTP -iTO) before every compile; only
    # compiles are traced and delayed.
    wrapper.write_text("#!/usr/bin/env python3\n" +
        "import json, os, pathlib, subprocess, sys, time\n" +
        f"if '-iTP' in sys.argv: os.execv({FPC!r}, [{FPC!r}] + sys.argv[1:])\n" +
        f"trace = pathlib.Path({str(root / 'trace')!r}) / str(os.getpid())\n" +
        "start = time.monotonic()\n" +
        "time.sleep(float(os.environ.get('GOCCIA_BUILD_CHECK_DELAY', '0')))\n" +
        f"result = subprocess.run([{FPC!r}] + sys.argv[1:])\n" +
        "trace.write_text(json.dumps({'start': start, 'end': time.monotonic(), 'args': sys.argv[1:]}))\n" +
        "sys.exit(result.returncode)\n")
    wrapper.chmod(0o755)
    env = dict(os.environ, PATH=str(root / "bin") + os.pathsep + os.environ["PATH"])

    def run(*args, success=True):
        result = subprocess.run([str(tool), *args], cwd=root, env=env,
                                capture_output=True, text=True, timeout=60)
        output = result.stdout + result.stderr
        assert (result.returncode == 0) == success, (args, result.returncode, output)
        return output

    def traces():
        return [json.loads(path.read_text()) for path in (root / "trace").iterdir()]

    def clear_trace():
        for path in (root / "trace").iterdir():
            path.unlink()

    output = run("tests", "--test=ALPHA.TEST.PAS", "--run-tests")
    assert "ran Alpha" in output and "ran Beta" not in output
    assert len(traces()) == 1
    clear_trace()
    output = run("tests", "--test=alpha", "--test=beta", "--jobs=2", "--run-tests")
    assert "ran Alpha" in output and "ran Beta" in output and "ran Other" not in output
    records = traces()
    assert len(records) == 2
    unit_dirs = [next(arg for arg in record["args"] if arg.startswith("-FU")) for record in records]
    assert len(set(unit_dirs)) == 2, unit_dirs
    if (os.cpu_count() or 1) >= 2:
        assert "jobs=2" in output, output
        assert max(r["start"] for r in records) < min(r["end"] for r in records), records
    clear_trace()
    run("tests", "--jobs=1")
    records = sorted(traces(), key=lambda record: record["start"])
    assert len(records) == 3
    assert all(a["end"] <= b["start"] for a, b in zip(records, records[1:]))

    clear_trace()
    output = run("tests", "--jobs=2147483647")
    jobs = int(output.split("jobs=", 1)[1].split(".", 1)[0])
    assert 1 <= jobs <= min(3, os.cpu_count() or 1), output

    # Account CPU rather than wall time: a loaded CI host may take longer,
    # but waiting for a quiet compiler must not burn a core for two seconds.
    before = resource.getrusage(resource.RUSAGE_CHILDREN)
    env["GOCCIA_BUILD_CHECK_DELAY"] = "2"
    try:
        run("tests", "--test=Alpha", "--jobs=1")
    finally:
        del env["GOCCIA_BUILD_CHECK_DELAY"]
    after = resource.getrusage(resource.RUSAGE_CHILDREN)
    cpu = after.ru_utime + after.ru_stime - before.ru_utime - before.ru_stime
    assert cpu < 1, f"quiet compiler wait consumed {cpu:.2f}s CPU"

    # Unselected targets, tests, legacy caches and stress caches survive clean.
    sentinels = ["targets/runner", "targets/repl", "tests/source/shared/Beta.Test",
                 "gc-stress/targets/repl", "layout-probe/targets/repl", ""]
    for folder in sentinels:
        cache = root / "build/compiled" / folder
        cache.mkdir(parents=True, exist_ok=True)
        (cache / "keep.ppu").write_text("sentinel")
    selected = root / "build/compiled/tests/source/shared/Alpha.Test/remove.ppu"
    selected.write_text("sentinel")
    run("--clean", "tests", "--test=Alpha")
    assert not selected.exists()
    assert all((root / "build/compiled" / folder / "keep.ppu").exists() for folder in sentinels)
    (root / "source/app/GocciaREPL.dpr").write_text("program REPL; begin end.\n")
    run("--clean", "repl")
    assert not (root / "build/compiled/targets/repl/keep.ppu").exists()
    assert (root / "build/compiled/gc-stress/targets/repl/keep.ppu").exists()
    run("--gc-stress", "--clean", "repl")
    assert not (root / "build/compiled/gc-stress/targets/repl/keep.ppu").exists()
    assert (root / "build/compiled/targets/runner/keep.ppu").exists()
    assert (root / "build/compiled/layout-probe/targets/repl/keep.ppu").exists()

    clear_trace()
    for args in [("--clean", "tests", "--test=absent"),
                 ("--clean", "repl", "--test=Alpha"),
                 ("tests", "--test="), ("tests", "--jobs=0"),
                 ("tests", "--jobs=invalid"), ("--clean", "unknown")]:
        run(*args, success=False)
    assert not traces()
    assert (root / "build/compiled/targets/runner/keep.ppu").exists()

    # Genuine compile failures are reported after active compilers finish.
    (root / "source/shared/Alpha.Test.pas").write_text("program Bad; begin invalid syntax end.\n")
    output = run("tests", "--jobs=2", "--run-tests", success=False)
    assert "Test build failed:" in output and "Running build" not in output
    assert "retrying once" not in output
    # A stale signature still retries once and propagates the second failure.
    (root / "source/shared/Alpha.Test.pas").write_text("program Bad; {$FATAL Internal error planted} begin end.\n")
    clear_trace()
    output = run("tests", "--test=Alpha", success=False)
    assert len(traces()) == 2 and output.count("retrying once") == 1
    # Running is optional, and a test failure is a build-tool failure.
    (root / "source/shared/Alpha.Test.pas").write_text("program Bad; begin Halt(7); end.\n")
    run("tests", "--test=Alpha")
    output = run("tests", "--test=Alpha", "--run-tests", success=False)
    assert "Test program failed:" in output

    # The targetless invocation still builds the complete original target set.
    apps = ["GocciaRunner", "GocciaScriptLoaderBare", "GocciaFuzzHarness",
            "GocciaTestRunner", "GocciaTest262Runner", "GocciaWasmTestRunner",
            "GocciaBenchmarkRunner", "GocciaBundler", "GocciaREPL",
            "compliance/GocciaTOMLComplianceRunner"]
    for app in apps:
        path = root / "source/app" / (app + ".dpr")
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text("program App; begin end.\n")
    (root / "fixtures/ffi").mkdir(parents=True)
    fixture = root / "fixtures/ffi/build.sh"
    fixture.write_text("#!/bin/sh\nexit 0\n")
    fixture.chmod(0o755)
    clear_trace()
    run("--clean")
    assert len(traces()) == len(apps) + 3
    assert not (root / "build/compiled/targets/runner/keep.ppu").exists()
    assert (root / "build/compiled/keep.ppu").exists()

print("Build-system checks passed (real FPC selection, parallel isolation, clean and failures)")
