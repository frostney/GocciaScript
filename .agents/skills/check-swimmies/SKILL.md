---
name: check-swimmies
description: Check changed or staged files against project documentation rules. Use when you want to verify which docs to read before modifying files in the current working tree.
---

# Check Swimmies

Run the swimmies checker against files that have been changed or staged in the current working tree. Swimmies maps source files to their relevant documentation so you know what to read before making changes.

## Steps

1. Find files to check by running `git diff --name-only HEAD` to get modified files and `git ls-files --others --exclude-standard` to get untracked files.
2. Run `bun check-swimmies.ts <files>` with the combined file list.
3. If any warnings are printed, read the referenced documentation before proceeding.
4. If no warnings are printed, report that all swimmies pass.
