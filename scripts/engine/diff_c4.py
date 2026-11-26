#!/usr/bin/env python3
"""Compare two run262.py (contract 4) reports, keyed by (id, mode) as upstream's PR comparison does.
Usage: diff_c4.py <base.json> <new.json> [max listed]"""
import collections, json, sys
load = lambda p: {(r['id'], r['mode']): r for r in json.load(open(p))['results']}
base, new = load(sys.argv[1]), load(sys.argv[2])
lim = int(sys.argv[3]) if len(sys.argv) > 3 else 40
cats = collections.defaultdict(lambda: [0, 0, 0])
for k in set(base) | set(new):
    c = cats[k[0].split('/')[0]]
    c[0] += base.get(k, {}).get('status') == 'PASS'; c[1] += new.get(k, {}).get('status') == 'PASS'; c[2] += 1
for c, (b, n, t) in sorted(cats.items()):
    print(f'{c:12s} {b:6d} -> {n:6d} / {t:6d}  ({n - b:+d})')
tb, tn = sum(v[0] for v in cats.values()), sum(v[1] for v in cats.values())
print(f'{"total":12s} {tb:6d} -> {tn:6d} / {sum(v[2] for v in cats.values()):6d}  ({tn - tb:+d})')
for st in ('WRAPPER_INFRA', 'TIMEOUT'):
    print(f'{st}: {sum(r["status"] == st for r in base.values())} -> {sum(r["status"] == st for r in new.values())}')
nf = sorted(k for k in new if new[k]['status'] != 'PASS' and base.get(k, {}).get('status') == 'PASS')
np_ = sorted(k for k in new if new[k]['status'] == 'PASS' and base.get(k, {}).get('status') != 'PASS')
print(f'newly failing {len(nf)}')
for k in nf[:lim]: print('  ', k[0], k[1], new[k]['status'], new[k]['message'][:140])
print(f'newly passing {len(np_)}')
for k in np_[:lim]: print('  ', k[0], k[1])
