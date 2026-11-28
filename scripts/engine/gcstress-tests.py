#!/usr/bin/env python3
"""GC stress over GocciaScript's own test suite.

Every test file under the given directories (GocciaTestRunner's discovery: the script extensions, node_modules
skipped) runs through `goccia --test` twice: as is, and with a small nursery (GOCCIA_NURSERY_KB, default 16) and
--gc-verify, which collects every few kilobytes and checks the heap after each collection. A file whose totals
differ between the two runs, or whose stressed run crashes or stops at a heap verification failure, is a GC bug (a value read through a pointer an allocation moved, a root the collector missed).

Prints one line per such file, "BAD <file>: <what>", and "TIMEOUT <file>" for one whose stressed run outlasted
TIMEOUT_S (not a failure: verifying the heap at every collection of a deep structure is slow), then
"OK tests (<n> files)" when no file is BAD. Exits 1 when one is.

The run uses a copy of the given binary, made in a temporary directory at the start and removed at the end, so a
rebuild of build/engine/goccia while the run goes on (the build replaces that file) does not fail the remaining files.

Usage: gcstress-tests.py <goccia binary> [jobs] [dir ...]   (default dir: tests)"""
import json, os, re, shutil, subprocess, sys, tempfile
from concurrent.futures import ThreadPoolExecutor

GIVEN_ENGINE = sys.argv[1]
JOBS = int(sys.argv[2]) if len(sys.argv) > 2 else 12
DIRS = sys.argv[3:] or ['tests']
KB = os.environ.get('GOCCIA_NURSERY_KB', '16')
EXTS = ('.js', '.jsx', '.ts', '.tsx', '.mjs', '.mts')
TIMEOUT_S = 600   # a stressed run verifies the heap at every collection
MEM_KB = 4000000  # address-space cap, as the reference runs have


def discover(root):
    out = []
    for d, subdirs, files in os.walk(root):
        subdirs[:] = sorted(s for s in subdirs if s != 'node_modules')
        out += [os.path.join(d, f) for f in sorted(files) if f.endswith(EXTS)]
    return out


def run(path, stress):
    env = dict(os.environ)
    env.pop('GOCCIA_NURSERY_KB', None)
    args = [ENGINE, '--test', path]
    if stress:
        env['GOCCIA_NURSERY_KB'] = KB
        args = [ENGINE, '--gc-verify', '--test', path]
    try:
        p = subprocess.run(['bash', '-c', 'ulimit -v %d; exec "$@"' % MEM_KB, 'goccia'] + args, env=env,
                           capture_output=True, timeout=TIMEOUT_S)
    except subprocess.TimeoutExpired:
        return {'status': 'timeout'}
    text = p.stdout.decode('utf-8', 'replace')
    m = re.search(r'__GOCCIATEST__ (\{.*)', text)
    if not m:
        tail = (text + p.stderr.decode('utf-8', 'replace')).strip().splitlines()[-1:] or ['']
        return {'status': 'crash', 'rc': p.returncode, 'msg': tail[0][:160]}
    try:
        r = json.loads(m.group(1))
    except ValueError:
        return {'status': 'crash', 'rc': p.returncode, 'msg': 'unreadable result ' + m.group(1)[:80]}
    return {'status': 'done', 'passed': r.get('passed'), 'failed': r.get('failed'), 'total': r.get('totalTests'),
            'suiteError': r.get('suiteError')}


def check(path):
    plain = run(path, False)
    stressed = run(path, True)
    if stressed['status'] == 'timeout':
        return path, 'TIMEOUT'
    if stressed['status'] == 'crash':
        return path, 'crashed under stress (rc %s): %s' % (stressed['rc'], stressed['msg'])
    key = lambda r: (r.get('status'), r.get('passed'), r.get('failed'), r.get('total'), r.get('suiteError'))
    if key(plain) != key(stressed):
        return path, 'results differ: plain %s, stressed %s' % (key(plain), key(stressed))
    return path, None


files = [f for d in DIRS for f in discover(d)]
work = tempfile.mkdtemp(prefix='gcstress-tests-')
try:
    ENGINE = os.path.join(work, os.path.basename(GIVEN_ENGINE))
    shutil.copy2(GIVEN_ENGINE, ENGINE)
    with ThreadPoolExecutor(JOBS) as ex:
        results = list(ex.map(check, files))
finally:
    shutil.rmtree(work, ignore_errors=True)
bad = [(f, why) for f, why in results if why and why != 'TIMEOUT']
for f, why in results:
    if why == 'TIMEOUT':
        print('TIMEOUT %s (%s KB): stressed run past %d s' % (f, KB, TIMEOUT_S))
for f, why in bad:
    print('BAD %s: %s' % (f, why))
if not bad:
    print('OK tests (%d files)' % len(files))
sys.exit(1 if bad else 0)
