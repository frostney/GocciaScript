#!/usr/bin/env python3
"""Check the allocation and publication of real JIT code in a Linux syscall trace."""

from pathlib import Path
import re
import sys

mmap = re.compile(r"^mmap\([^,]+, (\d+), ([^,]+), ([^,]+), [^)]*\)\s+= (0x[\da-f]+)$")
mprotect = re.compile(r"^mprotect\((0x[\da-f]+), (\d+), ([^,)]+)\)\s+= 0$")
munmap = re.compile(r"^munmap\((0x[\da-f]+), (\d+)\)\s+= 0$")
allocations = {}
published = 0

for line in Path(sys.argv[1]).read_text().splitlines():
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
        continue

    released = munmap.match(line)
    if released is not None:
        allocations.pop(released.groups(), None)

if published == 0:
    sys.exit("no anonymous writable JIT mapping was sealed read-execute")
print(f"JIT publication: {published} private write-only mappings sealed read-execute")
