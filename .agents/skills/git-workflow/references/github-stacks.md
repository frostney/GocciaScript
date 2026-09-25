# Native GitHub stacks

Use GitHub's official `gh stack` extension and native pull-request stack
topology. Branch names, labels, PR base branches, and delivery automation are
routing evidence, not proof of stack membership.

## Establish the stack

1. Require a clean worktree. Resolve and fetch the remote default branch, then
   record its exact remote head.
2. Verify that `gh stack` is installed and authenticated. Inspect local state
   with `gh stack view --json`; for existing PRs, also read GitHub's native
   `PullRequest.stack` and `stackEntry` data.
3. Before `gh stack init`, account for a stale local trunk: the extension uses
   the local default branch even when a detached checkout points at the fetched
   remote tip. Immediately verify the initialized bottom layer against the
   recorded remote-default head. If it is stale, perform the guarded sync below
   before editing; stop if the trunk or topology cannot be reconciled safely.
4. Use `gh stack init` for the first layer and `gh stack add` for later layers.
   Preserve bottom-to-top dependency order. When layers are independent, place
   the layer that shrinks the gate first; every layer above it pays the gate on
   each push. An empty new layer can omit `head` in `gh stack view --json`;
   resolve its actual local Git ref, and cross-check any head the extension
   does report. Do not manually imitate a native stack by changing PR bases alone.

## Guarded rewrite exception

`gh stack sync`, `gh stack rebase`, and `gh stack push` may cascade-rebase and
push with force-with-lease. They are allowed only when all of these are true:

- the worktree is clean before the operation;
- `gh stack view --json` confirms the intended local branches and order;
- GitHub native topology, when PRs exist, agrees with the intended stack;
- the current remote head of every affected branch is recorded first;
- no unrelated branch or worktree is in scope; and
- the cascade's CI load is accepted: a sync re-fires CI for every rebased layer
  at once, which on a seven-layer stack meant about 25 concurrent jobs and
  flaked two surfaces. Prefer a new top layer for fixes; after a sync, rerun
  flaked surfaces one at a time.

Run the narrowest official command that satisfies the need. Stop on a rebase
conflict, unexpected divergence, changed topology, lease rejection, partial
push, or remote head that cannot be explained. Use `gh stack rebase --abort`
when the extension offers restoration; never resolve ambiguity by invoking raw
rebase or force-push commands.

Ordinary branches never inherit this exception. Update them by merging the
freshly fetched remote default and pushing normally.

## Submit, validate, and merge

- Bind GitHub API and CLI calls to the verified host and repository, including
  child helpers. `gh api --hostname HOST` selects the API host; inherited
  `GH_HOST` and `GH_REPO` can otherwise alter command defaults. A Git remote or
  push guard alone does not bind API requests. Set target overrides only for
  the scoped subprocess, preserving the user's saved login configuration.
  See [GitHub CLI environment settings](https://cli.github.com/manual/gh_help_environment).
- Preserve approved heads at the actual push boundary. A prior remote read or
  the presence of `--force-with-lease` alone is insufficient: `gh stack submit`
  in [gh-stack 0.1.0](https://github.com/github/gh-stack/blob/v0.1.0/cmd/submit.go)
  refreshes tracking refs before constructing its leases. For frozen layers,
  require both the queued source and advertised remote head to match the
  approved commit; require remote absence for a new branch. An enforced
  [pre-push check](https://git-scm.com/docs/githooks#_pre_push) can reject a
  mismatch before updates are sent while retaining existing hooks. A later
  remote update must still be rejected by Git's receive-side comparison.
  Use the bundled helper described below to enforce this constraint; it keeps
  existing hooks and does not change persistent Git configuration.
- For a new top layer above frozen approved PRs, use protected `gh stack push`,
  then the separate creation and append procedure below. Record successful push
  completion and re-read the exact remote heads before creation. The native push
  refreshes tracking leases and is non-atomic; the guard must enforce the
  admitted source and expected remote IDs. It performs no PR creation or base
  updates. A partial or uncertain push requires stopped ownership and fresh
  reconciliation before advancing.
- Check each frozen PR's full metadata as well as its head: its base branch and
  base commit must match the approved previous layer (or remote default for the
  bottom PR), and auto-merge must be disabled while the required overlay is
  incomplete. Missing metadata is not evidence of a safe state. If two reads
  disagree during inspection, discard that observation and reassess before a
  write. Stable reads do not make subsequent GitHub mutations atomic.
- `gh stack submit` remains the combined command for a broader authorized native
  stack update. Its Git guard does not constrain later PR metadata writes; do not
  use that combined path to preserve a frozen dependency during top-layer publication.
  Reconcile every
  PR's title, body, base, draft state, linked requirements, and observed
  validation after submission. Put a closing keyword only on the layer that
  completes the issue.
- Treat every layer as a real PR. Validate its exact head and review its own
  claim; do not let evidence from one layer stand in for another.
- `/address-feedback` is the explicit review exception for a whole native
  stack. It may freeze an exact-head reviewed layer and cover a validated live
  finding with a later top fix layer. A covered lower layer is not independently
  merge-ready: the required overlay and every layer beneath it form one
  indivisible ready stack.
- Before merging, re-read native topology and exact remote heads. Use
  `gh stack merge --squash` for a selected atomic prefix only after every PR in
  that prefix independently satisfies its current-head checks, review, and
  readiness gates. When `/address-feedback` supplied whole-stack readiness, select
  the complete reported stack only after its final membership audit still
  matches; never merge a prefix beneath its top overlays. Never select
  rebase-merge merely because stack maintenance used rebases.
- After merge, use the official stack cleanup/sync path, remove only clean local
  branches owned by the run, and verify the integrated remote default.

## Protected publication helper

Use [scripts/stack_push_guard.py](../scripts/stack_push_guard.py) from this
installed skill. It requires Python 3.11+, Git, a POSIX shell, and the tested
official gh-stack 0.1.0 command. It has no third-party Python or eval-harness
dependency. Existing authorization and native-topology checks still apply.

After validating the local commits and reading the exact remote heads, write
an admission JSON file outside the worktree's tracked/untracked files. It has
`directory` (the physical absolute checkout path), `remote`, `url` (the observed
push URL), and `refs`. Each ref entry has a fully qualified `refs/heads/...` name,
the approved local commit as `source`, and the previously approved remote
commit as `expected`. Use all-zero object IDs of the repository's object format
only for branches verified absent remotely. Frozen members have equal source
and expected IDs. Include every branch the native command may push; never
refresh an approved expectation merely to make a rejection pass.

Run the authorized frozen-top push through the helper, using its actual installed
path and the chosen remote name:

```sh
python3 /path/to/git-workflow/scripts/stack_push_guard.py run \
  --admission /path/to/approved-stack.json -- \
  gh stack push --remote origin
```

For an observed partial publication, first reconcile native topology, branch
heads, candidate PRs, prior operation receipts and stopped ownership. Use the
separate creation and append procedure below for recovery. The wrapper retains
support for native link commands, but its Git hook does not prevent their PR-base
changes. This helper guards Git updates; it
does not deduplicate GitHub requests or authorize retrying an uncertain command.

Each invocation freezes the helper, records its Python runtime and admission,
and keeps hook receipts under the checkout's Git directory in `kgr-push-guards`.
The final command reports that directory. Retain it for reconciliation. A
successful exit establishes command execution only; re-read GitHub and apply
the full PR/stack readiness contract. A rejected push leaves the prepared work
available and requires fresh scope/evidence assessment.

The helper also accepts `gh stack submit --auto --remote REMOTE` for its combined
workflow and legacy diagnostics. Neither command's successful exit establishes
PR creation or readiness. Keep each subsequent API phase's intent and executor
record separately; saved completion alone does not authorize a second live caller.

Programmatic callers can use `prepare --admission FILE` (or `-` for stdin) to
obtain a guard directory and only the new Git configuration fields. Merge those
fields into the child process environment at creation. Do not change a running
worker's environment and assume its children receive the update, and do not
disable or replace the supplied hook when launching the native command.

## Recover a partial publication

If the approved branch was pushed but has no PR, claim a separate creation once.
Create a draft PR with the exact approved head branch, base branch, title and body
through `gh api --method POST repos/OWNER/REPO/pulls --input /path/to/pr.json`.
Then read the full PR and native stack again before claiming append. Do not use
combined branch linking to choose a different base from a concurrently changed
stack. Preserve creation evidence; a lost response requires reconciliation after
its executor stops, not a second creation. An older uncertain link attempt with
no visible PR does not establish that creation would be safe.

When recovery has an existing PR at the approved head and base, use GitHub's
[native stack append endpoint](https://docs.github.com/en/rest/pulls/stacks#add-pull-requests-to-a-pull-request-stack).
In gh-stack 0.1.0, `gh stack link` can retarget that PR's base before appending;
a concurrent new top could therefore change the approved dependency. The direct
endpoint requires the candidate's base ref to match the current native top's
head ref and does not first change the candidate's base.

First verify the repository, native stack identity and order, exact branch heads,
candidate PR number/head/base branch/base commit, and intended draft/open/unmerged
state with auto-merge disabled. Retain the original operation intent and verify
that its previous executor has stopped. Claim the remaining append once. Write
only the admitted PR number in a JSON body, for example `{"pull_requests":[123]}`,
then use the observed repository and stack number:

```sh
gh api --method POST repos/OWNER/REPO/stacks/STACK_NUMBER/add \
  -H 'Accept: application/vnd.github+json' \
  -H 'X-GitHub-Api-Version: 2026-03-10' --input /path/to/append.json
```

On a conflict, validation error, or lost response, retain the intent and reconcile
GitHub without repeating the request or retargeting the PR. Verify membership,
heads and PR policy again after acceptance. The server's base-chain constraint
protects against an intervening append that leaves this PR's approved base
unchanged; it is not a conditional check of every commit, draft setting or review
event. This API operation uses native stack membership and performs no Git push.
