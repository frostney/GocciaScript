#!/usr/bin/env python3
"""Finds a masked test that FPC 3.3.1 compiled away (frostney/fpc-toolchain#23).

FPC 3.3.1's peephole optimizer drops the test in

    Inc(N);
    if (N and 255) = 0 then

when N is in memory, and branches on the flags of the 32-bit add instead: `addl $1,N` then `jne`, so the guarded code
runs only when N wraps to 0. It drops the test for byte and word masks ($FF, $FFFF), for `<> 0` as well as `= 0`,
after an add or a subtract of any amount, and for a local at -O1 and a field or a global at -O2 and above. FPC 3.2.4
compiles all of these correctly.

This reads assembler listings (fpc -al) and reports each add or subtract whose destination is memory, followed by
je/jne/jz/jnz with only labels, directives and source comments between, where the source lines from the arithmetic
to the jump test a variable under a mask: `(X and M) = 0` or `(X and M) <> 0`. A correct `Dec(N); if N = 0` compiles
to the same two instructions, and its source has no mask, so it is not reported.

`./build.pas flagscan` compiles every engine program for the build's target with listings and runs this on them.

Usage: flagscan.py <listing directory> ...   prints one line per finding and exits 1 when there is one
       flagscan.py --self-test"""
import os, re, sys

ARITH = re.compile(r'^\t(add|sub|inc|dec)[bwlq]?\t(.*)$')
JUMP = re.compile(r'^\t(je|jne|jz|jnz)\t')
SOURCE = re.compile(r'^# \[(\d+)\] (.*)$')
REGISTER = re.compile(r'^\s*%[a-z0-9]+\s*$')
MASKED_TEST = re.compile(r'\band\s+[^()]*?\)\s*(=|<>)\s*0\b', re.IGNORECASE)


def scan(name, lines):
    """the findings in one listing, as (name, source line, instruction, jump, source text)"""
    found = []
    pending = None   # the arithmetic on memory just read: its instruction and the source lines since
    source = (None, '')
    for raw in lines:
        line = raw.rstrip('\n')
        m = SOURCE.match(line)
        if m:
            source = (m.group(1), m.group(2).strip())
            if pending:
                pending['text'].append(source[1])
            continue
        stripped = line.strip()
        # labels and directives (.Ll line labels, .stabn, .p2align) do not change the flags
        if not stripped or stripped.startswith('#') or stripped.startswith('.') or not line.startswith('\t'):
            continue
        a = ARITH.match(line)
        if a:
            destination = a.group(2).split(',')[-1]
            pending = None if REGISTER.match(destination) else {'ins': stripped, 'line': source[0], 'text': [source[1]]}
            continue
        if pending and JUMP.match(line) and any(MASKED_TEST.search(t) for t in pending['text']):
            found.append((name, pending['line'], pending['ins'], stripped, ' / '.join(pending['text'])))
        pending = None
    return found


def scan_directories(directories):
    found, listings = [], 0
    for directory in directories:
        for entry in sorted(os.listdir(directory)):
            if entry.endswith('.s'):
                listings += 1
                with open(os.path.join(directory, entry), encoding='utf-8', errors='replace') as f:
                    found += scan(entry, f)
    return found, listings


SELF_TEST = [
    # i386-win32 at -O1, Goccia.VM's StringToBigIntV before the fix: stabs line records between the add and the jump
    ('i386 -O1 masked poll', True, '''# [3493] Inc(I);
\taddl\t$1,-20(%ebp)
\t.stabn 68,0,3496,.Ll2188 - GOCCIA.VM_$$_STRINGTOBIGINTV
.Ll2188:
# [3496] if ((I and 255) = 0) and (Interrupt <> 0) then Exit;
\tjne\t.Lj5948
'''),
    # x86_64 at -O1, the same source, with DWARF line labels
    ('x86_64 -O1 masked poll', True, '''.Ll2538:
# [3493] Inc(I);
\taddl\t$1,-36(%rbp)
.Ll2539:
# [3496] if ((I and 255) = 0) and (Interrupt <> 0) then Exit;
\tjne\t.Lj3074
'''),
    # x86_64 at -O4: a field, on one source line, `<> 0`, a $FFFF mask
    ('x86_64 -O4 field', True, '''# [23] begin Inc(Count); if (Count and $FFFF) <> 0 then Exit; end;
\taddl\t$1,16(%rdi)
\tjne\t.Lj117
'''),
    # the test kept: not reported
    ('test kept', False, '''# [3493] Inc(I);
\taddl\t$1,%r13d
# [3496] if ((I and 255) = 0) and (Interrupt <> 0) then Exit;
\ttestb\t%r13b,%r13b
\tjne\t.Lj3005
'''),
    # a countdown on memory: the add's flags are the test's, correctly
    ('countdown', False, '''# [3501] Dec(PollLeft);
\tsubl\t$1,-24(%ebp)
# [3502] if PollLeft = 0 then
\tjne\t.Lj6001
'''),
    # an instruction between the add and the jump: the jump reads that instruction's flags
    ('compare between', False, '''# [10] Inc(I);
\taddl\t$1,-8(%ebp)
# [11] if (I and 255) = 0 then
\tmovl\t-8(%ebp),%eax
\tandl\t$255,%eax
\ttestl\t%eax,%eax
\tjne\t.Lj6
'''),
]


def self_test():
    failures = 0
    for label, expected, text in SELF_TEST:
        got = bool(scan(label, text.splitlines(True)))
        if got != expected:
            failures += 1
            print('self-test: %s: %s' % (label, 'not reported' if expected else 'reported'))
    if not failures:
        print('flagscan: self-test passed')
    return 1 if failures else 0


def main(argv):
    if argv == ['--self-test']:
        return self_test()
    if not argv:
        print(__doc__.rsplit('Usage: ', 1)[1], file=sys.stderr)
        return 2
    found, listings = scan_directories(argv)
    if listings == 0:
        print('flagscan: no listings (*.s) in %s' % ', '.join(argv), file=sys.stderr)
        return 2
    for name, line, ins, jump, text in found:
        print('%s: line %s: `%s` then `%s` with no test between, under "%s"' % (name, line, ins, jump, text))
    print('flagscan: %d listings, %d dropped masked tests (frostney/fpc-toolchain#23)' % (listings, len(found)))
    return 1 if found else 0


if __name__ == '__main__':
    sys.exit(main(sys.argv[1:]))
