#!/usr/bin/env python3
"""Checks a G2 run against its list of known failures (scripts/g2/known-failures).

  check-known-failures.py suite RESULTS.json [LIST]     GocciaTestRunner --output JSON
  check-known-failures.py test262 RESULTS.json [LIST]   GocciaTest262Runner --output JSON

suite: fails when a test file not on the list fails, or a listed file fails more tests than the list says.
test262: fails when a (mode, id) run not on the list does not pass, or a run the list names is missing.
Either way a listed failure that now passes is reported, so the list can shrink, but does not fail the check.
Exits 0 when the run is no worse than the list, 1 otherwise."""
import json, os, sys

HERE = os.path.dirname(os.path.abspath(__file__))


def read_list(path):
    rows = []
    for line in open(path, encoding='utf-8'):
        line = line.strip()
        if line and not line.startswith('#'):
            first, rest = line.split(' ', 1)
            rows.append((first, rest))
    return rows


def check_suite(results, known):
    expected = {path: int(count) for count, path in known}
    seen = set()
    bad = 0
    for r in results['results']:
        path = r['fileName']
        n = r['failed'] + r.get('suiteErrors', 0)
        seen.add(path)
        allowed = expected.get(path, 0)
        if n > allowed:
            bad += 1
            print('NEW FAILURE %s: %d failing, %d known' % (path, n, allowed))
            for t in r.get('failedTests', [])[:5]:
                print('    %s' % str(t)[:300])
            if r.get('errorMessage'):
                print('    %s' % r['errorMessage'][:300])
        elif n < allowed:
            print('fixed: %s fails %d of the %d known; lower its count in the list' % (path, n, allowed))
    for path in sorted(set(expected) - seen):
        print('fixed or gone: %s is listed but did not run' % path)
    return bad


def check_test262(results, known):
    expected = {(mode, test) for mode, test in known}
    runs = {(r['mode'], r['id']): r for r in results['results']}
    bad = 0
    for key, r in sorted(runs.items()):
        if r['status'] != 'PASS' and key not in expected:
            bad += 1
            print('NEW FAILURE [%s] %s: %s %s' % (key[0], key[1], r['status'], r.get('message', '')[:300]))
    for key in sorted(expected):
        if key not in runs:
            bad += 1
            print('MISSING [%s] %s: listed, but the run has no such test' % key)
        elif runs[key]['status'] == 'PASS':
            print('fixed: [%s] %s passes; remove it from the list' % key)
    s = results.get('summary', {})
    print('test262: %s passed of %s run' % (s.get('passed'), s.get('totalRun')))
    return bad


def main():
    if len(sys.argv) < 3 or sys.argv[1] not in ('suite', 'test262'):
        print(__doc__)
        return 2
    kind, results_path = sys.argv[1], sys.argv[2]
    list_path = sys.argv[3] if len(sys.argv) > 3 else os.path.join(HERE, 'known-failures', kind + '.txt')
    results = json.load(open(results_path, encoding='utf-8'))
    known = read_list(list_path)
    bad = check_suite(results, known) if kind == 'suite' else check_test262(results, known)
    print('%s: %d new failure(s) against %s' % (kind, bad, os.path.relpath(list_path)))
    return 1 if bad else 0


if __name__ == '__main__':
    sys.exit(main())
