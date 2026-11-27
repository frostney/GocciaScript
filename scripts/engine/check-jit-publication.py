#!/usr/bin/env python3
"""Check the allocation and publication of real JIT code in a Linux syscall trace.

check-jit-publication.py TRACE... [--jitmap JITMAP]: each TRACE is one thread's trace (strace -ff writes one per
thread; a thread publishes and unmaps its own code). JITMAP is the run's GOCCIA_JITMAP=1 output. With it, the
optimizing tier must have compiled at least one block, and each of its blocks must lie in a mapping a trace shows
published (requested write-only, sealed read-execute) and then unmapped."""

from pathlib import Path
import re
import sys

mmap = re.compile(r"^mmap\([^,]+, (\d+), ([^,]+), ([^,]+), [^)]*\)\s+= (0x[\da-f]+)$")
mprotect = re.compile(r"^mprotect\((0x[\da-f]+), (\d+), ([^,)]+)\)\s+= 0$")
munmap = re.compile(r"^munmap\((0x[\da-f]+), (\d+)\)\s+= 0$")
arguments = sys.argv[1:]
jitmap = None
if "--jitmap" in arguments:
    at = arguments.index("--jitmap")
    jitmap = arguments[at + 1]
    del arguments[at:at + 2]
if not arguments:
    sys.exit("usage: check-jit-publication.py TRACE... [--jitmap JITMAP]")
published = 0
# [address, size, unmapped] of each published mapping
lifetimes = []
lines = []
for trace in arguments:
    # a thread's trace on its own, so a mapping's allocation, sealing and unmapping are in one sequence
    lines.append(None)
    lines.extend(Path(trace).read_text().splitlines())
allocations = {}

for line in lines:
    if line is None:
        allocations = {}
        continue
    allocated = mmap.match(line)
    if allocated is not None:
        size, protection, flags, address = allocated.groups()
        if "MAP_ANONYMOUS" in flags:
            if {"PROT_WRITE", "PROT_EXEC"} <= set(protection.split("|")):
                sys.exit(f"anonymous mapping requested both write and execute: {line}")
            allocations[address, size] = (protection, flags)
        continue

    changed = mprotect.match(line)
    if changed is not None:
        address, size, protection = changed.groups()
        if "PROT_EXEC" not in protection.split("|") or (address, size) not in allocations:
            continue
        initial, flags = allocations[address, size]
        if initial != "PROT_WRITE" or "MAP_PRIVATE" not in flags or protection != "PROT_READ|PROT_EXEC":
            sys.exit(f"expected private write-only mapping sealed read-execute: {line} (initial {initial}, {flags})")
        published += 1
        lifetimes.append([int(address, 16), int(size), False])
        continue

    released = munmap.match(line)
    if released is not None:
        allocations.pop(released.groups(), None)
        start, length = int(released.group(1), 16), int(released.group(2))
        for lifetime in lifetimes:
            if not lifetime[2] and start <= lifetime[0] and lifetime[0] + lifetime[1] <= start + length:
                lifetime[2] = True

if published == 0:
    sys.exit("no anonymous writable JIT mapping was sealed read-execute")
print(f"JIT publication: {published} private write-only mappings sealed read-execute")

if jitmap is not None:
    optimized = [int(match.group(1), 16) for match in re.finditer(r"^jitmap ([\dA-F]+) \d+ opt ", Path(jitmap).read_text(), re.M)]
    if not optimized:
        sys.exit("the optimizing tier compiled no block")
    for block in optimized:
        # every publication that held the block's address, an earlier one too when the address was reused
        holders = [lifetime for lifetime in lifetimes if lifetime[0] <= block < lifetime[0] + lifetime[1]]
        if not holders:
            sys.exit(f"optimized block {block:#x} is in no published mapping")
        if not all(lifetime[2] for lifetime in holders):
            sys.exit(f"optimized block {block:#x} was never unmapped")
    print(f"JIT publication: {len(optimized)} optimized blocks published and unmapped")
