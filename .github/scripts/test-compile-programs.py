#!/usr/bin/env python3
"""Exercise the CI compiler scheduler through its command-line interface."""

import json
import os
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest

SCRIPT = Path(__file__).with_name("compile-programs.py")
COMPILER = r'''#!/usr/bin/env python3
import json, os, pathlib, sys, time
source = pathlib.Path(sys.argv[-1])
start = time.monotonic()
time.sleep(0.2)
record = {"source": str(source), "args": sys.argv[1:], "cwd": os.getcwd(),
          "start": start, "end": time.monotonic()}
pathlib.Path(os.environ["RECORDS"], source.stem + ".json").write_text(json.dumps(record))
print("123 lines compiled, 0.2 sec")
if source.stem == os.environ.get("FAIL_PROGRAM"):
    print("deliberate compiler failure")
    sys.exit(7)
'''


class CompileProgramsTest(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory(prefix="ci build ")
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name)
        self.sources = [
            "source/app/First.dpr", "source/app/Second.dpr",
            "source/units/Unit.Test.pas", "source/shared/Shared.Test.pas",
            "source/app/cli/CLI.Test.pas", "source/app/compliance/Compliance.dpr",
        ]
        for name in self.sources:
            path = self.root / name
            path.parent.mkdir(parents=True, exist_ok=True)
            path.write_text("program Fixture; begin end.\n")
        compiler = self.root / "bin/fpc"
        compiler.parent.mkdir()
        compiler.write_text(COMPILER)
        compiler.chmod(0o755)
        self.records = self.root / "records"
        self.records.mkdir()
        self.env = dict(os.environ, PATH=f"{compiler.parent}{os.pathsep}{os.environ['PATH']}",
                        RECORDS=str(self.records))

    def run_build(self, jobs=2):
        return subprocess.run(
            [sys.executable, str(SCRIPT), "--jobs", str(jobs), "x86_64", "linux", "--", "-Xd"],
            cwd=self.root, env=self.env, capture_output=True, text=True,
        )

    def test_builds_every_program_with_bounded_concurrency_and_private_units(self):
        result = self.run_build()
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        records = [json.loads(path.read_text()) for path in self.records.glob("*.json")]
        self.assertEqual({str(Path(r["source"]).relative_to(self.root)) for r in records},
                         set(self.sources))
        self.assertEqual(len({r["cwd"] for r in records}), len(self.sources))
        for record in records:
            for flag in ["-Px86_64", "-Tlinux", "-O4", "-dPRODUCTION", "-Xs", "-CX", "-XX", "-B", "-Xd"]:
                self.assertIn(flag, record["args"])
            self.assertIn("-FU" + record["cwd"], record["args"])
        events = sorted([(r["start"], 1) for r in records] + [(r["end"], -1) for r in records])
        active = peak = 0
        for _, delta in events:
            active += delta
            peak = max(peak, active)
        self.assertEqual(peak, 2)
        self.assertIn("6 programs", result.stdout)
        self.assertIn("0 failed", result.stdout)
        self.assertEqual(list((self.root / "build/ci-compiled").iterdir()), [])

    def test_compiler_failure_is_fatal_and_preserves_diagnostics(self):
        self.env["FAIL_PROGRAM"] = "First"
        result = self.run_build()
        self.assertEqual(result.returncode, 1)
        self.assertIn("deliberate compiler failure", result.stdout)
        self.assertIn("1 failed", result.stdout)
        self.assertEqual(len(list(self.records.glob("*.json"))), len(self.sources))
        self.assertIn("deliberate compiler failure",
                      (self.root / "build/ci-logs/First.log").read_text())

    def test_refuses_missing_program_groups_and_colliding_output_names(self):
        for jobs in [0, -1]:
            self.assertNotEqual(self.run_build(jobs).returncode, 0)
        collision = self.root / "source/app/compliance/First.dpr"
        collision.write_text("program First; begin end.\n")
        self.assertIn("basenames must be unique", self.run_build().stderr)
        collision.unlink()
        (self.root / "source/app/compliance/Compliance.dpr").unlink()
        self.assertIn("expected application entrypoints", self.run_build().stderr)
        self.assertEqual(list(self.records.glob("*.json")), [])


if __name__ == "__main__":
    unittest.main()
