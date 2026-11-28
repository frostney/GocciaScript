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
je/jne/jz/jnz with only labels, directives and source comments between, when the condition the jump tests has an
`and`. The listing names the jump's source line, not which condition on it the jump tests, so this reads every if,
while or until condition that starts on that line, from the source file the listing names, leaving out comments and
strings: the mask's spelling, its parentheses and line breaks do not matter, and the pair is reported when any of them
has an `and`. A correct `Dec(N); if N = 0` compiles to the same two instructions and has no `and` in its condition, so
it is not reported; a logical `and` in such a condition, or a masked test on the same line, is (rewrite it, or test N
in a statement of its own line). A jump whose condition cannot be read, such as an RTL generic's, one behind a comment
left open or one on a line where no condition starts, is reported too, as it cannot be checked.

`./build.pas flagscan` compiles every engine program for the build's target with listings and runs this on them.

Usage: flagscan.py <listing directory> ...   prints one line per finding and exits 1 when there is one
       flagscan.py --self-test"""
import os, re, sys

ARITH = re.compile(r'^\t(add|sub|inc|dec)[bwlq]?\t(.*)$')
JUMP = re.compile(r'^\t(je|jne|jz|jnz)\t')
SOURCE = re.compile(r'^# \[(\d+)\] (.*)$')
SOURCE_FILE = re.compile(r'^# \[([^\]]*[A-Za-z][^\]]*)\]$')
REGISTER = re.compile(r'^\s*%[a-z0-9]+\s*$')
# Pascal's tokens, as far as a condition needs them: comments and strings whole, so a word in one is not a token
TOKEN = re.compile(r"""\{[^}]*\}|\(\*.*?\*\)|\(\*|//[^\n]*|'(?:[^']|'')*'|[A-Za-z_][A-Za-z0-9_]*|\$[0-9A-Fa-f]+|\d+|<>|<=|>=|:=|\S""", re.DOTALL)
CONDITION_END = {'if': ('then',), 'while': ('do',), 'until': (';', 'end', 'else')}
# how far past its line a condition is looked for
CONDITION_LINES = 200
ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
SOURCE_DIRS = ('source', os.path.join('build', 'engine', 'gen'))


def source_index(root=ROOT):
    """every Pascal source file under the source directories, by file name (the listings name files without a path)"""
    index = {}
    for top in SOURCE_DIRS:
        for d, _, files in os.walk(os.path.join(root, top)):
            for f in files:
                if f.endswith(('.pas', '.pp', '.inc', '.dpr')):
                    index.setdefault(f, os.path.join(d, f))
    return index


def conditions(read, file, line):
    """the words of every if, while or until condition that starts on the line (lower case, comments and strings left
    out), however far each runs; None when the file is not one of ours (an RTL generic's code), when no condition starts
    on the line, or when one cannot be read whole. The listing names the jump's line, not which condition on it the
    jump tests, so all of them count"""
    lines = read(file)
    if lines is None or not 1 <= line <= len(lines):
        return None
    first_line_end = len(lines[line - 1])
    found, keyword, words = [], None, []
    for m in TOKEN.finditer('\n'.join(lines[line - 1:line - 1 + CONDITION_LINES])):
        t = m.group(0)
        if t.startswith(('{', '(*', '//', "'")):
            # a comment or a string is no word of a condition; one left open is not read further
            if t in ('{', '(*'):
                return None
            continue
        t = t.lower()
        if keyword is None:
            if t in CONDITION_END:
                if m.start() > first_line_end:
                    break
                keyword, words = t, []
        elif t in CONDITION_END[keyword]:
            found.append(words)
            keyword = None
        elif t in CONDITION_END:
            # a condition inside a condition is no Pascal: not read
            return None
        else:
            words.append(t)
    if keyword is not None or not found:
        return None
    return found


def scan(name, lines, read):
    """the findings in one listing, as (name, file, line, instruction, jump, why). read(file) returns a source file's
    lines, or None"""
    found = []
    pending = None   # the arithmetic on memory just read, and where its jump's condition is
    file, line = None, None
    for raw in lines:
        text = raw.rstrip('\n')
        m = SOURCE_FILE.match(text)
        if m:
            file = m.group(1)
            continue
        m = SOURCE.match(text)
        if m:
            line = int(m.group(1))
            if pending:
                pending['at'] = (file, line)
            continue
        stripped = text.strip()
        # labels and directives (.Ll line labels, .stabn, .p2align) do not change the flags
        if not stripped or stripped.startswith('#') or stripped.startswith('.') or not text.startswith('\t'):
            continue
        a = ARITH.match(text)
        if a:
            destination = a.group(2).split(',')[-1]
            pending = None if REGISTER.match(destination) else {'ins': stripped, 'at': (file, line)}
            continue
        if pending and JUMP.match(text):
            at_file, at_line = pending['at']
            tested = conditions(read, at_file, at_line) if at_file and at_line else None
            if tested is None:
                found.append((name, at_file, at_line, pending['ins'], stripped,
                              'its condition could not be read (no source in this repository, or no whole condition '
                              'starting on its line)'))
            elif any('and' in words for words in tested):
                found.append((name, at_file, at_line, pending['ins'], stripped,
                              ' | '.join(' '.join(words) for words in tested)))
        pending = None
    return found


def file_reader(index):
    cache = {}

    def read(file):
        if file not in cache:
            path = index.get(file)
            cache[file] = None
            if path:
                with open(path, encoding='utf-8', errors='replace') as f:
                    cache[file] = f.read().split('\n')
        return cache[file]
    return read


def scan_directories(directories):
    read = file_reader(source_index())
    found, listings = [], 0
    for directory in directories:
        for entry in sorted(os.listdir(directory)):
            if entry.endswith('.s'):
                listings += 1
                with open(os.path.join(directory, entry), encoding='utf-8', errors='replace') as f:
                    found += scan(entry, f, read)
    return found, listings


# (label, reported, listing, source of T.pas) for the self-test
POLL = """procedure P;
begin
  Inc(I);
  if ((I and 255) = 0) and (Interrupt <> 0) then Exit;
  Inc(J);
  if J and 255 = 0 then Exit;
  Inc(K);
  if (K and (255)) = 0 then Exit;
  Inc(L);
  if (L
      and 255) = 0 then
    Exit;
  Dec(PollLeft);
  if PollLeft = 0 then { and so on: a comment }
    Exit;
  Inc(Count); if (Count and $FFFF) <> 0 then Exit;
  Inc(M);
  if (M // poll now and then; or not
      and 255) = 0 then Poll;
  Inc(N);
  if (N { a; b
      } and 255) = 0 then Poll;
  Dec(PollLeft);
  if PollLeft = 0 then Flags := Flags and 255;
  Dec(PollLeft);
  if PollLeft = 0 then WriteLn('and');
  if Skip then Exit; Inc(R); if (R and 255) = 0 then Poll;
  if Ready then begin Inc(S); if (S and 255) = 0 then Poll; end;
  Inc(T);
  if (T
      and 255) = 0 then Poll;
  Inc(Q);
  if (Q { never closed
end;"""
SELF_TEST = [
    # i386-win32 at -O1, StringToBigIntV before the fix: stabs line records between the add and the jump
    ('i386 -O1 masked poll', True, """# [T.pas]
# [3] Inc(I);
\taddl\t$1,-20(%ebp)
\t.stabn 68,0,4,.Ll2188 - P
.Ll2188:
# [4] if ((I and 255) = 0) and (Interrupt <> 0) then Exit;
\tjne\t.Lj5948
"""),
    # x86_64 at -O1, with DWARF line labels; the mask with no parentheses
    ('x86_64 -O1 bare mask', True, """# [T.pas]
.Ll2538:
# [5] Inc(J);
\taddl\t$1,-36(%rbp)
.Ll2539:
# [6] if J and 255 = 0 then Exit;
\tjne\t.Lj3074
"""),
    ('a parenthesised mask', True, """# [T.pas]
# [7] Inc(K);
\taddl\t$1,-8(%ebp)
# [8] if (K and (255)) = 0 then Exit;
\tjne\t.Lj1
"""),
    ('a condition over two lines', True, """# [T.pas]
# [9] Inc(L);
\taddl\t$1,-12(%ebp)
# [10] if (L
\tjne\t.Lj2
"""),
    # x86_64 at -O4: a field, both statements on one source line, `<> 0`, a $FFFF mask
    ('x86_64 -O4 field', True, """# [T.pas]
# [16] Inc(Count); if (Count and $FFFF) <> 0 then Exit;
\taddl\t$1,16(%rdi)
\tjne\t.Lj117
"""),
    # code whose source is not ours (an RTL generic): reported, as it cannot be checked
    ('unknown source', True, """# [generics.collections.pas]
# [40] Inc(FCount);
\taddl\t$1,8(%eax)
\tjne\t.Lj9
"""),
    # a line comment inside the condition, which ends at its line
    ('a line comment in the condition', True, """# [T.pas]
# [17] Inc(M);
\taddl\t$1,-8(%ebp)
# [18] if (M // poll now and then; or not
\tjne\t.Lj3
"""),
    # a block comment with a semicolon inside the condition
    ('a block comment in the condition', True, """# [T.pas]
# [20] Inc(N);
\taddl\t$1,-8(%ebp)
# [21] if (N { a; b
\tjne\t.Lj4
"""),
    # a comment never closed: the condition cannot be read, which is reported
    ('an unclosed comment', True, """# [T.pas]
# [32] Inc(Q);
\taddl\t$1,-8(%ebp)
# [33] if (Q { never closed
\tjne\t.Lj5
"""),
    # several conditions on the jump's line: the listing does not say which the jump tests, so each counts
    ('a guard before it on its line', True, """# [T.pas]
# [27] if Skip then Exit; Inc(R); if (R and 255) = 0 then Poll;
\taddl\t$1,-8(%ebp)
\tjne\t.Lj8
"""),
    ('a containing if on its line', True, """# [T.pas]
# [28] if Ready then begin Inc(S); if (S and 255) = 0 then Poll; end;
\taddl\t$1,-8(%ebp)
\tjne\t.Lj9
"""),
    # the jump on a line where no condition starts (a condition's continuation): not readable, so reported
    ('a continuation line', True, """# [T.pas]
# [29] Inc(T);
\taddl\t$1,-8(%ebp)
# [31] and 255) = 0 then Poll;
\tjne\t.Lj10
"""),
    # an `and` after the condition, in its statement: not the condition's
    ('and in the guarded statement', False, """# [T.pas]
# [23] Dec(PollLeft);
\tsubl\t$1,-24(%ebp)
# [24] if PollLeft = 0 then Flags := Flags and 255;
\tjne\t.Lj6
"""),
    ('and in a string', False, """# [T.pas]
# [25] Dec(PollLeft);
\tsubl\t$1,-24(%ebp)
# [26] if PollLeft = 0 then WriteLn('and');
\tjne\t.Lj7
"""),
    # the test kept: not reported
    ('test kept', False, """# [T.pas]
# [3] Inc(I);
\taddl\t$1,%r13d
# [4] if ((I and 255) = 0) and (Interrupt <> 0) then Exit;
\ttestb\t%r13b,%r13b
\tjne\t.Lj3005
"""),
    # a countdown on memory: the subtract's flags are the test's, correctly; the comment's "and" does not count
    ('countdown', False, """# [T.pas]
# [13] Dec(PollLeft);
\tsubl\t$1,-24(%ebp)
# [14] if PollLeft = 0 then { and so on: a comment }
\tjne\t.Lj6001
"""),
    # an instruction between the add and the jump: the jump reads that instruction's flags
    ('compare between', False, """# [T.pas]
# [3] Inc(I);
\taddl\t$1,-8(%ebp)
# [4] if ((I and 255) = 0) and (Interrupt <> 0) then Exit;
\tmovl\t-8(%ebp),%eax
\tandl\t$255,%eax
\ttestl\t%eax,%eax
\tjne\t.Lj6
"""),
]


def self_test():
    source = POLL.split('\n')
    read = lambda file: source if file == 'T.pas' else None
    failures = 0
    for label, expected, text in SELF_TEST:
        got = bool(scan(label, text.splitlines(True), read))
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
    for name, file, line, ins, jump, why in found:
        print('%s:%s (%s): `%s` then `%s` with no test between, under: %s' % (file, line, name, ins, jump, why))
    print('flagscan: %d listings, %d jumps on an add\'s or subtract\'s own flags under a masked test '
          '(frostney/fpc-toolchain#23)' % (listings, len(found)))
    return 1 if found else 0


if __name__ == '__main__':
    sys.exit(main(sys.argv[1:]))
