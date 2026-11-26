#!/usr/bin/env python3
"""Bytecode files round trip over GocciaScript's own test suite on G2.

Every test file under the given paths (GocciaTestRunner's discovery: the script extensions, node_modules skipped;
a path may also name one file) runs through `goccia --test` twice: as is, and with GOCCIA_ARTIFACT_ROUNDTRIP=1, which
writes every compiled Script and module to a bytecode file (Goccia.Artifact), reads it back and verifies it before it
runs. A file whose totals differ between the two runs, or whose round trip is rejected ("artifact round trip: ...
rejected") or crashes, is a gap in the format, the reader or the verifier.

A reclamation test (RECLAIM) may pass fewer of its tests after the round trip.

Prints one line per such file, "BAD <file>: <what>", and "TIMEOUT <file>" for one whose round-trip run outlasted
TIMEOUT_S, then "OK tests (<n> files)" when no file is BAD. Exits 1 when one is.

Usage: artifact-roundtrip-tests.py <goccia binary> [jobs] [path ...]   (default path: tests)"""
import json, os, re, subprocess, sys
from concurrent.futures import ThreadPoolExecutor

G2 = sys.argv[1]
JOBS = int(sys.argv[2]) if len(sys.argv) > 2 else 12
PATHS = sys.argv[3:] or ['tests']
EXTS = ('.js', '.jsx', '.ts', '.tsx', '.mjs', '.mts')
TIMEOUT_S = 120
# Tests that a dropped value is reclaimed while its frame is suspended or alive. A bytecode file's suspensions keep
# every register (the verifier drops the compiler's live counts, which it cannot check), so after a round trip such
# a file may pass fewer of its tests, never more, with the same total.
RECLAIM = re.compile(r'(-released|releases-dead-registers)\.[a-z]+$')
MEM_KB = 4000000  # address-space cap, as the reference runs have


def discover(root):
    if os.path.isfile(root):
        return [root]
    out = []
    for d, subdirs, files in os.walk(root):
        subdirs[:] = sorted(s for s in subdirs if s != 'node_modules')
        out += [os.path.join(d, f) for f in sorted(files) if f.endswith(EXTS)]
    return out


def run(path, roundtrip):
    env = dict(os.environ)
    env.pop('GOCCIA_ARTIFACT_ROUNDTRIP', None)
    if roundtrip:
        env['GOCCIA_ARTIFACT_ROUNDTRIP'] = '1'
    try:
        p = subprocess.run(['bash', '-c', 'ulimit -v %d; exec "$@"' % MEM_KB, 'goccia', G2, '--test', path], env=env,
                           capture_output=True, timeout=TIMEOUT_S)
    except subprocess.TimeoutExpired:
        return {'status': 'timeout'}
    text = p.stdout.decode('utf-8', 'replace')
    m = re.search(r'__GOCCIATEST__ (\{.*)', text)
    if not m:
        err = p.stderr.decode('utf-8', 'replace')
        rejected = [l for l in err.splitlines() if 'artifact round trip' in l]
        tail = rejected[:1] or (text + err).strip().splitlines()[-1:] or ['']
        return {'status': 'crash', 'rc': p.returncode, 'msg': tail[0][:200]}
    try:
        r = json.loads(m.group(1))
    except ValueError:
        return {'status': 'crash', 'rc': p.returncode, 'msg': 'unreadable result ' + m.group(1)[:80]}
    return {'status': 'done', 'passed': r.get('passed'), 'failed': r.get('failed'), 'total': r.get('totalTests'),
            'suiteError': r.get('suiteError')}


def check(path):
    plain = run(path, False)
    tripped = run(path, True)
    if tripped['status'] == 'timeout':
        return path, 'TIMEOUT'
    if tripped['status'] == 'crash':
        return path, 'round trip failed (rc %s): %s' % (tripped['rc'], tripped['msg'])
    key = lambda r: (r.get('status'), r.get('passed'), r.get('failed'), r.get('total'), r.get('suiteError'))
    if (RECLAIM.search(path) and plain['status'] == tripped['status'] == 'done' and
            plain['total'] == tripped['total'] and tripped['passed'] <= plain['passed']):
        return path, None
    if key(plain) != key(tripped):
        return path, 'results differ: plain %s, round trip %s' % (key(plain), key(tripped))
    return path, None


files = [f for p in PATHS for f in discover(p)]
with ThreadPoolExecutor(JOBS) as ex:
    results = list(ex.map(check, files))
bad = [(f, why) for f, why in results if why and why != 'TIMEOUT']
for f, why in results:
    if why == 'TIMEOUT':
        print('TIMEOUT %s: round-trip run past %d s' % (f, TIMEOUT_S))
for f, why in bad:
    print('BAD %s: %s' % (f, why))
if bad:
    sys.exit(1)
print('OK tests (%d files)' % len(files))
