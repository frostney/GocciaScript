# An ADR's number is reserved on the remote before its file is written

**Date:** 2026-11-28
**Area:** `docs`, `process`
**Related:** [ADR index: Numbering](README.md#numbering)

## Context

ADRs are numbered in sequence. A new ADR took the highest number in its own checkout plus one, so branches worked on
at the same time took the same number, and each found out only when another merged first. #2255 renamed its ADR
three times (0196, 0198, 0199, 0201) as other open pull requests took and merged those numbers. Each rename
meant merging `main` again, editing every citation of the old number, and another full CI run. Each merge of `main`
also conflicted in the index, `docs/adr/README.md`, where every branch appends its entry after the same last line.

## Decision

A number is taken by creating the ref `refs/adr/<NNNN>` on `origin` before the file is written.
`npx tsx scripts/adr.ts new "<title>"` creates it, pointing at a commit with no files whose subject names the ADR's
file, then writes the file from the template and adds its index entry. The remote creates a ref once: when two
branches create the same one at the same time, it accepts one push and refuses the other with "reference already
exists", and the refused branch takes the next number. A number therefore belongs to one file from the moment it is
written, and nothing is renamed later.

`scripts/adr.ts check`, run in the docs job of both CI workflows, fails when two ADRs share a number, when an ADR
after 0202 has no reservation or its reservation names another file, and when the index does not list every ADR once.
ADRs up to 0202 predate reservations and hold none. `move` renames an ADR its branch reserved and moves the
reservation with it.

`.gitattributes` merges the index with Git's `union` driver, so two branches' appended entries both stay.

Considered and not taken:

- **The pull request's number as the ADR's number**, as Rust's RFCs do. It is unique too, but it exists only once the
  pull request is open, after the ADR and the code that cites it are written, so every ADR would be renamed once,
  and a pull request with several ADRs would need a second scheme.
- **A number assigned at merge.** Renaming at merge costs a commit and a CI run on every ADR, and assigning it after
  merge needs a bot that pushes to `main`.
- **Checking open pull requests for numbers in use.** It narrows the race, but two branches checking at the same
  time still take the same number.

## Consequences

Taking a number needs push access to `origin` and the network. A contributor without push access writes the ADR
and a maintainer reserves its number. A number whose pull request never merges stays reserved and leaves a gap in
the sequence, as 0121 already does. Reservation refs sit outside `refs/heads` and `refs/tags`: a clone does not fetch
them, GitHub does not list them, and creating one starts no workflow.
