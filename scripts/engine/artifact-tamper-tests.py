#!/usr/bin/env python3
"""A class field's definition in a tampered bytecode file, in every tier of G2.

opDefField trusts its receiver register to hold an object, and in code read from a file (Goccia.Artifact) every
tier checks that instead of handing the instruction to Execute: Execute, FastLoop, the baseline JIT (inline and
in JH_DefField) and the optimizing tier (its receiver guard, which deoptimizes to Execute). Each case compiles a
small program with GocciaBundler, points the receiver of the class's first field definition at the register that
holds the field's value, and runs the file with GocciaRunner. The value is an object for a while, so the code
gets hot in the tier the case names, and then a number: the run must end in Execute's TypeError ("Invalid
bytecode: ...") at that point, not a crash or a write through a number.

Prints "OK <case>" or "BAD <case>: <what>" per case and exits 1 when one is BAD.

Usage: artifact-tamper-tests.py <G2 GocciaBundler> <G2 GocciaRunner>   (run from the repository root)"""
import os, re, struct, subprocess, sys, tempfile

BUNDLER, RUNNER = sys.argv[1], sys.argv[2]
FLAGS = ['--compat-traditional-for-loop']
EXPECT = "TypeError: Invalid bytecode: an instruction's operand is not the kind it requires at "


def opcode(name):
    src = open(os.path.join('source', 'engine', 'Goccia.Instructions.pas')).read()
    body = re.search(r'TOp\s*=\s*\((.*?)\);', src, re.S).group(1)
    body = re.sub(r'//[^\n]*', '', re.sub(r'\{.*?\}', '', body, flags=re.S))
    return [x.strip() for x in body.split(',') if x.strip()].index(name)


OP_DEFFIELD = opcode('opDefField')

# (case, switch point, program): the field x's value comes from g() or G, an object until the switch point
CASES = []
for tier, n in (('interpreter', 50), ('baseline JIT', 3000), ('hot loop', 60000)):
    CASES.append((tier, n, '''let n = 0;
const g = () => { n++; return n < %d ? { k: n } : 5; };
class C { x = g(); y = 1; }
let last;
try { for (let i = 0; i < %d; i++) last = new C(); console.log("no throw", n); }
catch (e) { console.log(e.constructor.name + ": " + e.message + " at " + n); }
''' % (n, n + 10)))
# the optimizing tier inlines V's constructor and field initializer into plus; G turns into a number on run j
for j in (20, 99):
    CASES.append(('optimizing tier, run %d' % j, j, '''let G = { k: 1 };
class V {
  x = G;
  y = 1;
  constructor(a, b, c) { this.a = a; this.b = b; this.c = c; }
  plus(o) { return new V(this.a + o.a, this.b + o.b, this.c + o.c); }
}
const one = new V(1, 2, 3);
const run = (k) => { let acc = new V(0, 0, 0); for (let i = 0; i < k; i++) acc = acc.plus(one); return acc.a; };
let j = 0;
try { for (j = 0; j < 100; j++) { if (j === %d) G = 5; run(3000); } console.log("no throw", j); }
catch (e) { console.log(e.constructor.name + ": " + e.message + " at " + j); }
''' % j))


def defields(data):
    """the file offsets of DefField instructions (A, B registers and a small IC index): an instruction is a
    little-endian u64, opcode | A << 8 | B << 24 | C << 40"""
    hits = []
    for off in range(len(data) - 8):
        if data[off] != OP_DEFFIELD:
            continue
        (ins,) = struct.unpack_from('<Q', data, off)
        a, b, c = (ins >> 8) & 0xFFFF, (ins >> 24) & 0xFFFF, ins >> 40
        if a < 32 and b < 32 and c < 8 and a != b:
            hits.append((off, b, c))
    return hits


def run_case(tmp, name, n, program):
    js = os.path.join(tmp, 'case.js')
    gbc = os.path.join(tmp, 'case.gbc')
    open(js, 'w').write(program)
    p = subprocess.run([BUNDLER] + FLAGS + [js, '--output=' + gbc], capture_output=True, timeout=120)
    if p.returncode != 0:
        return 'compile failed: ' + p.stdout.decode('utf-8', 'replace')[-200:]
    data = bytearray(open(gbc, 'rb').read())
    hits = defields(data)
    if len(hits) != 2:
        return 'found %d DefField instructions, expected the two fields x and y (has the encoding changed?)' % len(hits)
    off, b, c = hits[0]
    struct.pack_into('<Q', data, off, OP_DEFFIELD | (b << 8) | (b << 24) | (c << 40))
    open(gbc, 'wb').write(data)
    p = subprocess.run(['bash', '-c', 'ulimit -v 4000000; exec "$@"', 'goccia', RUNNER] + FLAGS + [gbc],
                       capture_output=True, timeout=120)
    out = p.stdout.decode('utf-8', 'replace').splitlines()
    lines = [l for l in out if not l.startswith('Running ') and 'Lex:' not in l]
    want = EXPECT + str(n)
    if p.returncode != 0 or lines != [want]:
        return 'rc %d, got %r, expected %r' % (p.returncode, (lines + [p.stderr.decode('utf-8', 'replace')])[:3], want)
    return None


bad = 0
with tempfile.TemporaryDirectory() as tmp:
    for name, n, program in CASES:
        why = run_case(tmp, name, n, program)
        if why:
            bad += 1
            print('BAD %s: %s' % (name, why))
        else:
            print('OK %s' % name)
sys.exit(1 if bad else 0)
