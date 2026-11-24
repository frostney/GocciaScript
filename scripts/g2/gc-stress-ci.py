#!/usr/bin/env python3
"""GC stress for CI (gc-stress.yml): finds values a collection moved or freed while only a Pascal local held them.

Three parts, each comparing a stressed run with a plain run of the same binaries, so a test that fails both ways
is a known failure and not a finding:

1. tests/: scripts/g2/gcstress-tests.py runs every file through `g2 --test` plainly and with a 16 KB nursery and
   --gc-verify (a collection every few kilobytes, the heap checked after each). BAD lines are findings; TIMEOUT
   lines are not (verifying a deep heap at every collection is slow).
2. scripts/g2/regress/*.js through `g2 --gc-verify` (plus a file's `// g2-flags:` line) at 16, 64 and 128 KB
   nurseries, with G2_HOST=1; each prints `ok` last.
3. test262: the directories where values cross native code while guest code runs, through GocciaTest262Runner
   plainly and with G2_NURSERY_KB=16 and G2_GC_VERIFY=1. A run that passes plainly and fails stressed is a
   finding.

Usage: gc-stress-ci.py --suite-dir=<test262 checkout> [--out-dir=DIR] [--jobs=N] [--bin-dir=DIR]
Writes <out-dir>/report.md and <out-dir>/summary.json. Exits 0 with no finding, 1 with findings, 2 when a part
could not run."""
import argparse, json, os, subprocess, sys

TEST262_CATEGORIES = [
    "built-ins/Promise", "built-ins/Iterator", "language/expressions/await",
    "language/expressions/async-function", "language/expressions/async-arrow-function",
    "language/statements/async-function", "language/eval-code", "language/arguments-object",
    "language/expressions/generators", "language/statements/generators",
]
REGRESS_KB = (16, 64, 128)


class Infrastructure(Exception):
    pass


def exe(path):
    return path + ".exe" if os.name == "nt" else path


def run_tests(g2, jobs, out):
    proc = subprocess.run([sys.executable, "scripts/g2/gcstress-tests.py", g2, str(jobs), "tests"],
                          capture_output=True, text=True)
    open(os.path.join(out, "tests.log"), "w").write(proc.stdout + proc.stderr)
    lines = proc.stdout.splitlines()
    if not any(l.startswith("OK tests") or l.startswith("BAD ") for l in lines):
        raise Infrastructure("gcstress-tests.py printed no result (exit %d): %s" % (proc.returncode, proc.stderr[-500:]))
    return [l for l in lines if l.startswith("BAD ")], [l for l in lines if l.startswith("TIMEOUT ")]


def run_regress(g2):
    bad = []
    for name in sorted(os.listdir("scripts/g2/regress")):
        if not name.endswith(".js"):
            continue
        path = os.path.join("scripts/g2/regress", name)
        first = open(path, encoding="utf-8").readline()
        flags = first.split("g2-flags:", 1)[1].split() if "g2-flags:" in first else []
        for kb in REGRESS_KB:
            env = dict(os.environ, G2_HOST="1", G2_NURSERY_KB=str(kb))
            try:
                proc = subprocess.run([g2, "--gc-verify", *flags, path], capture_output=True, text=True,
                                      env=env, timeout=1800)
            except subprocess.TimeoutExpired:
                bad.append("TIMEOUT regress %s (%d KB)" % (name, kb))
                break
            last = [l for l in proc.stdout.splitlines() if l.strip()][-1:] or [""]
            if last[0].strip() != "ok":
                bad.append("BAD regress %s (%d KB): exit %d, %s" % (name, kb, proc.returncode,
                                                                   (last[0] or proc.stderr.strip()[-200:])[:200]))
                break
    return bad


def run_test262(runner, suite, jobs, out, stressed):
    tag = "stress" if stressed else "plain"
    report = os.path.join(out, "test262-%s.json" % tag)
    if os.path.exists(report):
        os.remove(report)
    env = dict(os.environ, TZ="UTC")
    if stressed:
        env.update(G2_NURSERY_KB="16", G2_GC_VERIFY="1")
    with open(os.path.join(out, "test262-%s.log" % tag), "w") as log:
        proc = subprocess.run([runner, "--suite-dir=" + suite, "--categories=" + ",".join(TEST262_CATEGORIES),
                               "--jobs=%d" % jobs, "--timeout=20m", "--output=" + report],
                              stdout=log, stderr=subprocess.STDOUT, env=env)
    if not os.path.exists(report):
        raise Infrastructure("test262 %s run wrote no report (exit %d)" % (tag, proc.returncode))
    results = json.load(open(report, encoding="utf-8")).get("results") or []
    if not results:
        raise Infrastructure("test262 %s report has no results" % tag)
    return {(r["id"], r.get("mode", "")): r for r in results}


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--suite-dir", required=True)
    ap.add_argument("--out-dir", default="build/gc-stress-report")
    ap.add_argument("--jobs", type=int, default=os.cpu_count() or 4)
    ap.add_argument("--bin-dir", default="build")
    args = ap.parse_args()
    os.makedirs(args.out_dir, exist_ok=True)
    g2 = exe(os.path.join(args.bin_dir, "g2", "g2"))
    runner = exe(os.path.join(args.bin_dir, "GocciaTest262Runner"))
    findings, notes, errors = [], [], []
    try:
        bad, timeouts = run_tests(g2, args.jobs, args.out_dir)
        findings += bad
        notes += timeouts
    except Infrastructure as e:
        errors.append(str(e))
    findings += run_regress(g2)
    try:
        plain = run_test262(runner, args.suite_dir, args.jobs, args.out_dir, False)
        stress = run_test262(runner, args.suite_dir, args.jobs, args.out_dir, True)
        for key, r in sorted(stress.items()):
            base = plain.get(key)
            if base and base["status"] == "PASS" and r["status"] != "PASS":
                findings.append("BAD test262 [%s] %s: %s %s" % (key[1], key[0], r["status"],
                                                                (r.get("message") or "")[:200]))
    except Infrastructure as e:
        errors.append(str(e))

    lines = ["## GC stress on G2", ""]
    if findings:
        lines += ["Tests that pass plainly but fail under GC stress:", ""] + ["- `%s`" % f for f in findings]
    else:
        lines.append("No test fails only under GC stress.")
    if notes:
        lines += ["", "Timed out under stress (not failures):", ""] + ["- `%s`" % n for n in notes]
    if errors:
        lines += ["", "Parts that could not run:", ""] + ["- %s" % e for e in errors]
    lines += ["", "Reproduce a tests/ finding with `G2_NURSERY_KB=16 build/g2/g2 --gc-verify --test <file>`, a test262 "
              "one with `G2_NURSERY_KB=16 G2_GC_VERIFY=1 build/GocciaTest262Runner --suite-dir=<test262> "
              "--filter=<id>` (docs/contributing/tooling.md#gc-stress)."]
    open(os.path.join(args.out_dir, "report.md"), "w").write("\n".join(lines) + "\n")
    json.dump({"findings": findings, "timeouts": notes, "errors": errors},
              open(os.path.join(args.out_dir, "summary.json"), "w"), indent=2)
    print("\n".join(lines))
    sys.exit(2 if errors else 1 if findings else 0)


if __name__ == "__main__":
    main()
