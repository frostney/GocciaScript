#!/usr/bin/env python3
"""test262 on G2 under GocciaScript's grading contract 4 (docs/test262.md at GocciaScript ef669e85).

- Every top-level category runs; no eligibility filter.
- Runs per file: module -> once (strict); raw -> once (non-strict, file as written, no harness);
  onlyStrict -> once (strict); noStrict -> once (non-strict); otherwise twice (non-strict, then strict).
- Harness files are separate non-strict global Scripts evaluated in one realm before the test:
  $262.js (G2's host object), assert.js, sta.js, doneprintHandle.js for async tests, then the includes.
  A strict run puts '"use strict";\\n' in front of the test file only.
- The host (`g2 --test262`) prints one outcome line, "\\x1eG2T262 <json>", with kind
  ok | parse | throw | harness | compile | unsupported, the escaped value's constructor name read from data
  properties (name), and a message. The runner grades it:
  negative parse/resolution: kind parse and name == negative.type (parse errors precede evaluation);
  negative runtime: kind throw and name == negative.type; async: kind ok and the AsyncTestComplete marker;
  otherwise kind ok. A harness failure, a host crash (signal) or a missing outcome line is WRAPPER_INFRA.
- Report: upstream's JSON shape (summary with byMode and byCategory, results keyed by id and mode).

Usage: run262.py <test262 dir> <g2 binary> <out.json> [jobs] [path prefix ...]"""
import json, multiprocessing, os, re, resource, signal, subprocess, sys, time

T262, G2, OUT = sys.argv[1], sys.argv[2], sys.argv[3]
JOBS = int(sys.argv[4]) if len(sys.argv) > 4 else 12
PREFIXES = sys.argv[5:]
TEST = os.path.join(T262, 'test')
HARNESS = os.path.join(T262, 'harness')
HOST262 = os.path.join(os.path.dirname(os.path.abspath(__file__)), 'harness', '$262.js')
TIMEOUT_S = 20
MEM_BYTES = 3 << 30   # address-space cap; G2 maps its stacks lazily, so this is above upstream's 2 GiB heap limit
MARK = '\x1eG2T262 '


def frontmatter(src):
    m = re.search(r'/\*---(.*?)---\*/', src, re.S)
    y = m.group(1) if m else ''

    def lst(key):
        mm = re.search(r'^\s*' + key + r':\s*\[(.*?)\]', y, re.M | re.S)
        if mm:
            return [x.strip() for x in mm.group(1).split(',') if x.strip()]
        mm = re.search(r'^\s*' + key + r':\s*\n((?:[ \t]+-[^\n]*\n?)+)', y, re.M)
        return [x.strip()[1:].strip() for x in mm.group(1).splitlines() if x.strip().startswith('-')] if mm else []
    neg = None
    mm = re.search(r'^\s*negative:\s*\n((?:[ \t]+\w+:[^\n]*\n?)+)', y, re.M)
    if mm:
        d = dict(re.findall(r'(\w+):\s*(\S+)', mm.group(1)))
        neg = (d.get('phase'), d.get('type'))
    return lst('includes'), set(lst('flags')), neg


def modes(flags):
    if 'module' in flags: return ['strict']
    if 'raw' in flags or 'noStrict' in flags: return ['non-strict']
    if 'onlyStrict' in flags: return ['strict']
    return ['non-strict', 'strict']


def limit():
    resource.setrlimit(resource.RLIMIT_AS, (MEM_BYTES, MEM_BYTES))
    os.setsid()


def run_one(job):
    rel, mode = job
    path = os.path.join(TEST, rel)
    t0 = time.time()

    def res(status, msg):
        return {'id': rel, 'mode': mode, 'status': status, 'durationMs': int((time.time() - t0) * 1000), 'message': msg}
    try:
        src = open(path, encoding='utf-8').read()
        includes, flags, neg = frontmatter(src)
        cmd = [G2, '--test262']
        if mode == 'strict' and 'module' not in flags: cmd.append('--strict')
        if 'module' in flags: cmd.append('--module')
        if 'raw' not in flags:
            hs = [HOST262, os.path.join(HARNESS, 'assert.js'), os.path.join(HARNESS, 'sta.js')]
            if 'async' in flags: hs.append(os.path.join(HARNESS, 'doneprintHandle.js'))
            hs += [os.path.join(HARNESS, i) for i in includes if i not in ('assert.js', 'sta.js', 'doneprintHandle.js')]
            for h in hs: cmd += ['--harness', h]
        cmd.append(path)
        env = dict(os.environ)
        if 'CanBlockIsFalse' in flags: env['G2_CANNOT_BLOCK'] = '1'
        try:
            p = subprocess.run(cmd, capture_output=True, timeout=TIMEOUT_S, preexec_fn=limit, env=env)
        except subprocess.TimeoutExpired as e:
            return res('TIMEOUT', 'TIMEOUT after %ds' % TIMEOUT_S)
        out = p.stdout.decode('utf-8', 'replace')
        err = p.stderr.decode('utf-8', 'replace')
        i = out.rfind(MARK)
        if p.returncode < 0:
            return res('WRAPPER_INFRA', 'host died with signal %d: %s' % (-p.returncode, (out + err)[-200:]))
        if i < 0:
            return res('WRAPPER_INFRA', 'no outcome line, rc %d: %s' % (p.returncode, (out + err)[-200:]))
        o = json.loads(out[i + len(MARK):].split('\n', 1)[0])
        printed = out[:i]
        kind, name, msg = o.get('kind'), o.get('name', ''), o.get('message', '')
        if kind == 'harness':
            return res('WRAPPER_INFRA', 'harness include %s failed: %s' % (o.get('file', '?'), msg))
        desc = (name + ': ' + msg) if name else msg
        if neg:
            phase, typ = neg
            if phase in ('parse', 'resolution', 'early'):
                if kind == 'parse' and name == typ: return res('PASS', '')
                if kind == 'throw' and 'This statement should not be evaluated' in msg:
                    return res('FAIL', 'body was evaluated ($DONOTEVALUATE reached)')
                return res('FAIL', 'expected %s at %s, got %s %s' % (typ, phase, kind, desc))
            if kind == 'throw' and name == typ: return res('PASS', '')
            return res('FAIL', 'expected runtime %s, got %s %s' % (typ, kind, desc))
        if kind != 'ok':
            return res('FAIL', '%s %s' % (kind, desc) if kind not in ('throw',) else desc)
        if 'async' in flags:
            if 'Test262:AsyncTestComplete' in printed: return res('PASS', '')
            m = re.search(r'Test262:AsyncTestFailure:?(.*)', printed)
            return res('FAIL', m.group(1).strip() if m else 'async test did not call $DONE')
        return res('PASS', '')
    except Exception as e:
        return res('WRAPPER_INFRA', 'runner: %r' % e)


def discover():
    files = []
    for root, _, fs in os.walk(TEST):
        for f in fs:
            if f.endswith('.js') and '_FIXTURE' not in f:
                rel = os.path.relpath(os.path.join(root, f), TEST)
                if not PREFIXES or any(rel.startswith(p) for p in PREFIXES):
                    files.append(rel)
    files.sort()
    return files


def summarize(files, results, secs):
    def zero(): return {'run': 0, 'passed': 0, 'failed': 0, 'wrapperInfra': 0, 'timeouts': 0}
    key = {'PASS': 'passed', 'FAIL': 'failed', 'WRAPPER_INFRA': 'wrapperInfra', 'TIMEOUT': 'timeouts'}
    tot, bym, cats = zero(), {'non-strict': zero(), 'strict': zero()}, {}
    for r in results:
        c = cats.setdefault(r['id'].split('/')[0], {'all': zero(), 'non-strict': zero(), 'strict': zero()})
        for b in (tot, bym[r['mode']], c['all'], c[r['mode']]):
            b['run'] += 1; b[key[r['status']]] += 1
    return {'totalDiscovered': len(files), 'totalRun': tot['run'], 'passed': tot['passed'], 'failed': tot['failed'],
            'wrapperInfraFailures': tot['wrapperInfra'], 'timeouts': tot['timeouts'], 'durationSeconds': round(secs, 2),
            'byMode': bym,
            'byCategory': [dict(category=k, **v['all'], byMode={'non-strict': v['non-strict'], 'strict': v['strict']})
                           for k, v in sorted(cats.items())]}


if __name__ == '__main__':
    files = discover()
    jobs = []
    for rel in files:
        src = open(os.path.join(TEST, rel), encoding='utf-8', errors='replace').read()
        for m in modes(frontmatter(src)[1]): jobs.append((rel, m))
    t0 = time.time()
    results = []
    with multiprocessing.Pool(JOBS) as pool:
        for i, r in enumerate(pool.imap_unordered(run_one, jobs, chunksize=8)):
            results.append(r)
            if i % 10000 == 0: print(f'{i}/{len(jobs)} {time.time() - t0:.0f}s', file=sys.stderr, flush=True)
    results.sort(key=lambda r: (r['id'], r['mode'] != 'non-strict'))
    sha = subprocess.run(['git', '-C', T262, 'rev-parse', 'HEAD'], capture_output=True, text=True).stdout.strip()
    s = summarize(files, results, time.time() - t0)
    json.dump({'summary': s, 'results': results,
               'run': {'suiteDir': T262, 'test262Sha': sha, 'engine': os.path.abspath(G2), 'jobs': JOBS,
                       'timeoutMs': TIMEOUT_S * 1000, 'maxAddressSpaceBytes': MEM_BYTES, 'gradingContract': 4}},
              open(OUT, 'w'))
    print(f"{'category':10s} {'passed':>7s} / {'runs':>7s}")
    for c in s['byCategory']:
        print(f"{c['category']:10s} {c['passed']:7d} / {c['run']:7d} {100 * c['passed'] / c['run']:5.1f}%"
              f"  (wrapper-infra {c['wrapperInfra']}, timeouts {c['timeouts']})")
    print(f"{'total':10s} {s['passed']:7d} / {s['totalRun']:7d} {100 * s['passed'] / max(1, s['totalRun']):5.1f}%"
          f"  non-strict {s['byMode']['non-strict']['passed']}/{s['byMode']['non-strict']['run']}"
          f"  strict {s['byMode']['strict']['passed']}/{s['byMode']['strict']['run']}  ({s['durationSeconds']:.0f}s)")
