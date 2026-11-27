#!/usr/bin/env python3
"""Check the allocation and publication of real JIT code in Linux syscall traces.

check-jit-publication.py TRACE... [--jitmap JITMAP]: each TRACE is one thread's trace (strace -ff writes one per
thread; a thread publishes and unmaps its own code). JITMAP is the run's GOCCIA_JITMAP=1 output. With it, the
optimizing tier must have compiled at least one block, each block must lie wholly in a mapping a trace shows
published (requested write-only, sealed read-execute), and every publication that held a block must have been
unmapped by its own thread afterwards."""

import argparse
from pathlib import Path
import re
import sys

mmap = re.compile(r"^mmap\([^,]+, (\d+), ([^,]+), ([^,]+), [^)]*\)\s+= (0x[\da-f]+)$")
mprotect = re.compile(r"^mprotect\((0x[\da-f]+), (\d+), ([^,)]+)\)\s+= 0$")
munmap = re.compile(r"^munmap\((0x[\da-f]+), (\d+)\)\s+= 0$")
optimized_block = re.compile(r"^jitmap ([\dA-F]+) (\d+) opt ", re.M)

parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
parser.add_argument("traces", nargs="+", metavar="TRACE", help="one thread's strace output")
parser.add_argument("--jitmap", help="the run's GOCCIA_JITMAP=1 output: check its optimized blocks")
options = parser.parse_args()


class Publication:
    """A mapping one thread sealed read-execute, and whether that thread unmapped it afterwards."""

    def __init__(self, start, size):
        self.start = start
        self.size = size
        self.unmapped = False

    def holds(self, address, length):
        return self.start <= address and address + length <= self.start + self.size


publications = []
for trace in options.traces:
    # this thread's mappings: a mapping's allocation, sealing and unmapping are in its own thread's sequence
    allocations = {}
    live = []
    for line in Path(trace).read_text().splitlines():
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
            publication = Publication(int(address, 16), int(size))
            publications.append(publication)
            live.append(publication)
            continue

        released = munmap.match(line)
        if released is not None:
            allocations.pop(released.groups(), None)
            start, length = int(released.group(1), 16), int(released.group(2))
            # only this thread's publications still mapped: a later mapping at the same address is a new one
            for publication in [p for p in live if start <= p.start and p.start + p.size <= start + length]:
                publication.unmapped = True
                live.remove(publication)

if not publications:
    sys.exit("no anonymous writable JIT mapping was sealed read-execute")
print(f"JIT publication: {len(publications)} private write-only mappings sealed read-execute")

if options.jitmap is not None:
    blocks = [(int(address, 16), int(length))
              for address, length in optimized_block.findall(Path(options.jitmap).read_text())]
    if not blocks:
        sys.exit("the optimizing tier compiled no block")
    for address, length in blocks:
        # every publication that held the block, an earlier one too when its address was reused
        holders = [p for p in publications if p.holds(address, length)]
        if not holders:
            sys.exit(f"optimized block {address:#x} ({length} bytes) is in no published mapping")
        if not all(p.unmapped for p in holders):
            sys.exit(f"optimized block {address:#x} ({length} bytes) was never unmapped")
    print(f"JIT publication: {len(blocks)} optimized blocks published and unmapped")
