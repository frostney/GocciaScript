# 0119 - A sandbox run is materialized by the host, on the host's command line

**Date:** 2026-09-21
**Area:** `sandbox`

## Context

[ADR 0118](0118-original-file-source-ranges.md) got a lint rule to the point
where it can compute a correct edit against the file it read. Nothing could
apply one, and [ADR 0117](0117-javascript-visible-ast-module.md) deferred the
question with a sentence: *a sandbox rule that reports is a different risk
from one that edits.*

That is true, and it is the whole of the decision. A rule that reports is code
that read some text. A rule that edits is code that changes what a project
builds, what a reviewer sees, and what runs in production. The second is worth
having and is worth being deliberate about, because the sandbox exists for
running code nobody has read — [the VISION](../../VISION.md) is explicit that
the primary goal is a runtime for AI agents with no ambient filesystem access.

Most of the answer was already decided. [ADR 0068](0068-goccia-sandbox-runner.md)
settled the shape when it built the runner:

> Diffs are consumer-facing artifacts rather than write-back behavior. Tools
> that want to materialize sandbox changes must consume the diff and choose
> how to apply it.

What was missing was the tool, and the diff was not enough to build one on.
The JSON diff reports a kind, a path, and two sizes — no content. The
`unified` format is a readable summary, not a patch: one `-` line holding the
whole old file and one `+` line holding the whole new one, which no `patch` or
`git apply` will take. So the sentence described a loop nobody could close.

## Decision

**`--write-back` on `GocciaSandboxRunner`.** After a run that succeeded, the
files the run changed are written to the host paths they were seeded from.

The guest is unchanged and gains nothing. It writes into its own virtual
filesystem, as it always could, and that filesystem reaches nothing. This is
the host, after the guest has stopped running, deciding to keep what came out
— which is exactly the consumer ADR 0068 said would have to exist.

### Why not the other two shapes

- **A write capability in the sandbox, behind a flag.** This is the shape the
  question is usually asked in, and it is the wrong one. It moves host
  authority inside the guest, which is the one thing the sandbox exists to
  prevent, and it buys nothing: the guest already has somewhere to write. A
  flag that says "this program may touch host files" also has to survive
  `runScript`, nested sandboxes, and the shell's `goccia` builtin, each of
  which would need to decide whether the capability is inherited. There is no
  good answer to that question and no need to have one.
- **A rule returns edits and the host applies them.** This invents a second
  vocabulary — a range, a replacement, a path, an ordering, a conflict rule —
  that only a linter would ever speak, and it makes every fixer serialize
  what it already knows how to write. The sandbox filesystem is a better
  version of the same idea: it is the returned edit, it is already ordered
  and conflict-free because the guest resolved it, and it works for a
  codemod, a formatter, or a generator without any of them agreeing on what
  an edit is.

### The property this buys

**A rule that reports and a rule that fixes are the same program.** The
sandbox gives a guest no argument vector — no `process.argv`, by design, since
that is ambient input — so a rule cannot be asked to fix. It simply writes its
result into its own filesystem. Whether that becomes a change to a real file
is a word on the *host's* command line, and there is nothing the guest can do
to make that word appear.

So the risk difference between reporting and editing lands entirely where
authority is granted, and is visible there. `examples/_experimental/blank-lines.mjs`
demonstrates it: the same invocation with and without `--write-back` differs
in the host tree and in nothing else.

### What it will and will not do

- Only a path a `--seed` supplied is eligible, and confinement is the seed
  mapping itself, so there is no second path vocabulary to get wrong. The
  longest matching seed wins, and the resolved host path is checked to be
  under its seed root regardless.
- **Deletions are never applied.** This makes a file say something different;
  it does not make one stop existing. A fixer does not need it, and it is the
  one outcome that re-running cannot undo.
- A file created under a seeded directory is written. A codemod that splits a
  file is the same kind of edit as one that rewrites it.
- A host target that is a symlink is skipped, matching the refusal seeding
  already applies in the other direction.
- A path with no seeded origin is reported and skipped, not invented on the
  host.
- **A run that failed writes nothing.** A fixer that threw halfway has written
  some of its files and not the rest, and a half-applied fix is worse than
  none.
- Every target is resolved before any is written, and each file is written to
  a temporary in its own directory and renamed, so a single file is never
  left half-written.

Seeds remain import baselines and not mounts. Nothing is live, nothing is
observable to the guest, and the write happens once, after the guest can no
longer act on it.

## Consequences

- The loop closes. A rule reads a tree over a seeded filesystem, computes
  edits against offsets that index the real file, writes the corrected text
  into its own filesystem, and the host keeps the result if it asked to. No
  toolchain in front of it and no patch format in between.
- `--write-back` is the runner's second host-write, after `--diff-output`. It
  is the same authority — a path the host named on its own command line — with
  a different payload.
- There is no tree-level transaction. Each file is replaced atomically; a
  failure partway through the set leaves the earlier files written, and the
  summary says how many landed. A stronger guarantee would mean staging the
  whole tree, which is a cost every run would pay for a case a re-run already
  handles.
- File modes and timestamps are not carried across. The sandbox filesystem's
  metadata is its own; write-back is about content.

## Related

- [ADR 0068](0068-goccia-sandbox-runner.md) — the runner, seeds as baselines,
  and diffs as artifacts
- [ADR 0118](0118-original-file-source-ranges.md) — why a rule can compute an
  edit at all
- [ADR 0103](0103-layered-untrusted-execution-boundaries.md) — what the
  sandbox boundary is and is not
