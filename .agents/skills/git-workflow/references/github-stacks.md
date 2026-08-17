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
   Preserve bottom-to-top dependency order. Do not manually imitate a native
   stack by changing PR bases alone.

## Guarded rewrite exception

`gh stack sync`, `gh stack rebase`, and `gh stack push` may cascade-rebase and
push with force-with-lease. They are allowed only when all of these are true:

- the worktree is clean before the operation;
- `gh stack view --json` confirms the intended local branches and order;
- GitHub native topology, when PRs exist, agrees with the intended stack;
- the current remote head of every affected branch is recorded first; and
- no unrelated branch or worktree is in scope.

Run the narrowest official command that satisfies the need. Stop on a rebase
conflict, unexpected divergence, changed topology, lease rejection, partial
push, or remote head that cannot be explained. Use `gh stack rebase --abort`
when the extension offers restoration; never resolve ambiguity by invoking raw
rebase or force-push commands.

Ordinary branches never inherit this exception. Update them by merging the
freshly fetched remote default and pushing normally.

## Submit, validate, and merge

- Use `gh stack submit` to create or update the native stack. Reconcile every
  PR's title, body, base, draft state, linked acceptance criteria, and observed
  validation after submission. Put a closing keyword only on the layer that
  completes the issue.
- Treat every layer as a real PR. Validate its exact head and review its own
  claim; do not let evidence from one layer stand in for another.
- Before merging, re-read native topology and exact remote heads. Use
  `gh stack merge --squash` for the selected atomic prefix only after every PR
  in that prefix independently satisfies its current-head checks, review, and
  readiness gates. Never select rebase-merge merely because stack maintenance
  used rebases.
- After merge, use the official stack cleanup/sync path, remove only clean local
  branches owned by the run, and verify the integrated remote default.
