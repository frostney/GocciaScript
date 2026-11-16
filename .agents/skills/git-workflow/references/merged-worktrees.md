# Merged worktrees

Remove every merged worktree that is safe to remove, whichever run created it,
and keep everything else. List the repository's worktrees with
`git worktree list --porcelain` and decide each one separately.

## Merged

- A detached worktree is merged when the remote default branch contains its
  HEAD.
- A worktree on a branch is merged when a pull request from that branch was
  merged and its head contains the worktree's HEAD, so the worktree has no
  commit the pull request lacks. A squash merge leaves the branch's own commits
  out of the default branch, so find the pull request by branch or head commit;
  default-branch containment alone does not make a branch merged.
- A worktree on the remote default branch, or on any branch with an open pull
  request, is never merged.

## Safe to remove

A merged worktree is safe to remove only when all of these hold:

- `git status --porcelain` is empty: no tracked changes and no untracked files.
- Every ignored path (`git status --porcelain --ignored`) is dependency or
  build output the project regenerates. `git worktree remove` deletes ignored
  files without asking, so an ignored `.env` file, local database, credential
  or `.agent/` note keeps the worktree.
- It is not locked, and it is neither the main worktree nor the one this
  session is in.
- No running process uses it as its working directory, runs an executable from
  it, or holds a file in it open.
- No file in it changed in the last day, unless this run just merged it:
  another session may have created it for new work and be between commands.

## Remove, prune and report

- Remove each safe worktree with `git worktree remove` without `--force`, so git
  refuses one that changed since you checked. Then delete its local branch, if
  it has one, with `git branch -D`: git treats a squash-merged branch as
  unmerged, and the pull request check above proved it merged.
- An entry whose directory is gone keeps its recorded HEAD in the repository's
  `worktrees/<name>/HEAD`. `git worktree prune` drops every such entry, so
  first lock each one that is not merged by the rules above, for example a
  worktree on an unmounted drive, then prune.
- Keep every other worktree and report each with its reason: an open or
  unmerged pull request, commits no merged pull request contains, changes,
  ignored files that are not build output, a lock, a running process or recent
  changes.
- A checkout that must stay, such as a build tree that scripts run from, is
  locked with `git worktree lock --reason`.
