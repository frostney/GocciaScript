#!/usr/bin/env python3
"""A campaign of generated programs on G2's fuzz harness, without AFL.

Each case is random bytes that GocciaFuzzHarness --grammar turns into a program
(source/app/Goccia.Fuzz.Generator.pas) and runs in the interpreter and with the
JIT, comparing what the two print. A case that exits nonzero (a fault, or runs
that differ) is a finding: its bytes, the program they choose and the harness's
report are kept in the findings directory, after the bytes are reduced.

The reducer leans on the generator's rule that a zero byte is always the smallest
choice: it truncates the input and zeroes runs of bytes, keeping each change
after which the harness still reports the same kind of finding.

Prints how the runs ended (from the harness's --verbose lines) and each finding.
Exits 1 when there was a finding.

Usage: grammar-fuzz.py <G2 GocciaFuzzHarness> <cases> [--jobs N] [--seed S]
         [--findings DIR] [--size BYTES]
       grammar-fuzz.py <G2 GocciaFuzzHarness> --reduce <input> [--findings DIR]"""
import argparse, collections, concurrent.futures, os, random, re, subprocess, sys, tempfile

# a run ends within about 2 s (each executor's 1 s deadline): past this the harness hung
TIMEOUT = 20
# the address space a run may map: as the other G2 harness scripts cap it
ULIMIT_KB = 4000000


def run(harness, data, tmp):
    path = os.path.join(tmp, 'case-%d.bin' % os.getpid())
    with open(path, 'wb') as f:
        f.write(data)
    try:
        p = subprocess.run(['bash', '-c', 'ulimit -v %d; exec "$@"' % ULIMIT_KB, 'g2', harness,
                            '--verbose', '--grammar', path], capture_output=True, timeout=TIMEOUT)
        return p.returncode, p.stderr.decode('utf-8', 'replace')
    except subprocess.TimeoutExpired:
        return -1, 'harness timeout after %d s' % TIMEOUT


def kind(rc, report):
    """what kind of finding a run is ('' none), for the reducer to keep"""
    if rc == 0:
        return ''
    if 'the runs differ' in report:
        return 'differ'
    m = re.search(r'unexpected fault in the (\w+) run: (\w+)', report)
    if m:
        return 'fault-%s-%s' % (m.group(1), m.group(2))
    return 'exit-%d' % rc


def outcomes(report):
    return re.findall(r'^\[(bytecode|jit)\] ([a-z-]+)', report, re.M)


def reduce(harness, data, tmp):
    want = kind(*run(harness, data, tmp))
    if not want:
        return data, want
    # truncate while it still fails the same way
    step = len(data) // 2
    while step >= 1:
        while len(data) > step and kind(*run(harness, data[:-step], tmp)) == want:
            data = data[:-step]
        step //= 2
    # zero runs of bytes, the longest first
    size = max(1, len(data) // 4)
    while size >= 1:
        i = 0
        while i < len(data):
            if any(data[i:i + size]):
                trial = data[:i] + bytes(min(size, len(data) - i)) + data[i + size:]
                if kind(*run(harness, trial, tmp)) == want:
                    data = trial
            i += size
        size //= 2
    return data, want


def keep(harness, data, findings, tmp, name):
    os.makedirs(findings, exist_ok=True)
    base = os.path.join(findings, name)
    # the input as found, before a reduction that may take long
    with open(base + '.orig.bin', 'wb') as f:
        f.write(data)
    data, what = reduce(harness, data, tmp)
    with open(base + '.bin', 'wb') as f:
        f.write(data)
    program = subprocess.run([harness, '--grammar', '--print-program', base + '.bin'],
                             capture_output=True).stdout
    with open(base + '.js', 'wb') as f:
        f.write(program)
    rc, report = run(harness, data, tmp)
    with open(base + '.txt', 'w') as f:
        f.write(report)
    return what, base


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('harness')
    ap.add_argument('cases', nargs='?', type=int, default=0)
    ap.add_argument('--jobs', type=int, default=os.cpu_count() or 4)
    ap.add_argument('--seed', type=int, default=None)
    ap.add_argument('--size', type=int, default=4096)
    ap.add_argument('--findings', default='build/g2/grammar-findings')
    ap.add_argument('--reduce', default=None)
    a = ap.parse_args()
    harness = os.path.abspath(a.harness)
    with tempfile.TemporaryDirectory() as tmp:
        if a.reduce:
            data = open(a.reduce, 'rb').read()
            what, base = keep(harness, data, a.findings, tmp, os.path.basename(a.reduce) + '.reduced')
            print('%s: %s' % (what or 'no finding', base))
            return 1 if what else 0
        seed = a.seed if a.seed is not None else random.randrange(1 << 32)
        print('seed %d' % seed, flush=True)
        rng = random.Random(seed)
        inputs = [rng.randbytes(rng.randrange(a.size // 4, a.size + 1)) for _ in range(a.cases)]
        stats = collections.Counter()
        found = []

        def one(i):
            sub = os.path.join(tmp, str(i % (a.jobs * 4)))
            os.makedirs(sub, exist_ok=True)
            return i, run(harness, inputs[i], sub)

        with concurrent.futures.ThreadPoolExecutor(a.jobs) as pool:
            for i, (rc, report) in pool.map(one, range(a.cases)):
                for executor, outcome in outcomes(report):
                    stats[executor + ' ' + outcome] += 1
                if rc != 0:
                    found.append(i)
                    print('finding: case %d: %s' % (i, (report.strip().splitlines() or ['?'])[-1][:300]),
                          flush=True)
        print(' '.join('%s=%d' % kv for kv in sorted(stats.items())))
        for i in found:
            what, base = keep(harness, inputs[i], a.findings, tmp, 'seed%d-case%d' % (seed, i))
            print('kept %s (%s)' % (base, what or 'did not reproduce'))
        print('%d cases, %d findings' % (a.cases, len(found)))
        return 1 if found else 0


if __name__ == '__main__':
    sys.exit(main())
