# Lists relocations that reference a TLS symbol with a non-TLS relocation type:
# the FPC 3.3.1 native-TLS code generator emits R_X86_64_32S for `lea sym+off`
# of a threadvar when off <> 0, which is an absolute address, not a TLS offset.
# Also lists a TLS offset relocation (TPOFF) on an instruction whose memory operand
# has neither the %fs segment nor a base register: the offset is then read as an
# absolute address (fpc-toolchain 3.3.1+4: a second threadvar read folded into
# `add tpoff,%reg` after `mov %fs:tpoff,%reg`).
import subprocess, sys, re, glob, collections
TLS_OK = {'R_X86_64_TPOFF32', 'R_X86_64_TPOFF64', 'R_X86_64_GOTTPOFF', 'R_X86_64_TLSGD', 'R_X86_64_TLSLD',
          'R_X86_64_DTPOFF32', 'R_X86_64_DTPOFF64', 'R_X86_64_DTPMOD64', 'R_X86_64_GOTPC32_TLSDESC', 'R_X86_64_TLSDESC_CALL'}
bad = collections.Counter(); where = collections.defaultdict(set)
for o in sys.argv[1:]:
    syms = subprocess.run(['readelf', '-sW', o], capture_output=True, text=True).stdout
    tls = set(l.split()[-1] for l in syms.split('\n') if ' TLS ' in l)
    if not tls: continue
    dis = subprocess.run(['objdump', '-dr', '--no-show-raw-insn', o], capture_output=True, text=True).stdout
    fn = '?'
    insn = ''
    for l in dis.split('\n'):
        m = re.match(r'^[0-9a-f]+ <(.*)>:', l)
        if m: fn = m.group(1); continue
        m = re.search(r'(R_X86_64_\w+)\s+(\S+?)(?:[+-]0x[0-9a-f]+)?$', l)
        if m and m.group(2) in tls:
            if m.group(1) not in TLS_OK or (m.group(1) in ('R_X86_64_TPOFF32', 'R_X86_64_TPOFF64') and
                                             '%fs:' not in insn and not re.search(r'\(%\w+', insn)):
                bad[o.split('/')[-1]] += 1; where[o.split('/')[-1]].add(fn)
            continue
        if re.match(r'^\s+[0-9a-f]+:\t', l): insn = l
for k, v in bad.most_common(): print(v, k, sorted(where[k])[:8])
print('total', sum(bad.values()))
