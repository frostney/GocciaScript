---
name: git-workflow
description: >-
  Applies the user's git defaults: branch from the remote default, merge rather
  than rebase for ordinary branches, use native GitHub stacks when selected,
  never amend, and squash-merge pull requests. Use when branching, syncing,
  committing, pushing, or merging in the user's repos.
license: Unlicense OR MIT
compatibility: >-
  Requires git; pull-request operations also require the GitHub CLI (gh) and
  network access. Publishing to GitHub also requires Python 3.11+; protected
  native stack publication also requires a POSIX shell.
---

# Git workflow

Keep every change on a fresh, focused branch whose history stays reviewable and
recoverable: merge instead of rewriting, never lose local work, and land each
pull request as one squash commit with a meaningful subject. A git request
authorizes only the repository and GitHub state required for that operation.

An explicit user instruction about one of these defaults replaces only that
default for the active workstream. Preserve it across turns until changed; a
new repository or operation needs its own scope.

## Start or resume work

- Resolve the base from the remote default; never hardcode `main`.
- Make remote-default synchronization an automatic preflight for new work; do
  not ask permission to perform a safe clean update.
- When new work starts from the local default branch, inspect status before
  fetching or editing. If it is clean, fetch and fast-forward it to the fetched
  remote-default tip automatically. Stop on local-only commits, divergence, or
  a non-fast-forward update.
- If that local default worktree is dirty, stop before fetching, updating, or
  editing and ask the user to choose: discard the state, or preserve it and
  create a focused branch/worktree from the latest remote default. Do not make
  either choice, stash, commit, or discard anything without the answer.
- Before the first edit in any other newly selected or reused branch or
  worktree, require a clean worktree and fetch the remote default branch. Stop
  and report dirty files; never stash, commit, or discard them automatically.
- Automatically create every new focused local branch and worktree at the exact
  freshly fetched remote-default tip. Do not configure a focused branch to
  track the remote default; set its upstream only when pushing that focused
  branch.
- When entering an existing focused branch or worktree, fetch and merge its
  remote base before editing.

## Update, commit, and push

- Merge the remote base to update a branch. Never rebase. The remote base is
  the branch the open PR targets when that is the remote default or a named
  base as [create-pr](../create-pr/SKILL.md) step 1 defines it, such as a
  release branch for a backport. Never merge the remote default into a branch
  whose PR targets a named base. Without an open PR, the remote base is the
  remote default.
- A layer of a native stack is updated through the stack reference. When an
  open PR targets any other non-default branch, it has no remote base: stop
  before merging or pushing and report that base instead of choosing one. The
  user can then name it or have the PR restacked.
- During an authorized PR update, resolve conflicts whose intended behavior is
  established by the selected change and current base. Preserve both sides'
  required behavior, regenerate generated files with the project tool, and
  validate the result. Pause when resolution requires a material unresolved
  choice. During a new-work preflight, report conflicts before implementation.
- Never amend commits. Add a new commit for every correction.
- Never force-push. Stop if a plain push is rejected by divergent history.
- Stage only relevant files and exclude secrets or unrelated local work.
- Run the [publication guard](#publication-guard) before each commit, push
  and pull request write to a GitHub repository.
- Use concise Conventional Commit subjects in imperative mood. Each commit title
  must state its observable impact, not only the mechanism changed.
- Let hooks run unless the user explicitly asks otherwise.

## Publication guard

Run [scripts/publication_guard.py](scripts/publication_guard.py) from this
installed skill with Python 3.11 or newer, using its actual installed path, for
example `python3 /path/to/git-workflow/scripts/publication_guard.py`. It needs
`gh` signed in to GitHub. When the repository receiving the content is public,
it rewrites private GitHub references and local machine paths out of it.

- Before each commit, write the message to a file, run `staged --message-file
  FILE`, and commit with `git commit -F FILE`.
- Before each push, including `gh stack push`, `gh stack submit` and
  `gh stack link`, run `outgoing --base BASE`. BASE is `REMOTE/DEFAULT`, or for
  a stack layer the branch beneath it. Add `--remote NAME` when the push goes
  to a remote other than the branch's push remote. Commit what it stages
  through the commit step above, then push.
- Before each pull request title or body write, write the title and body to
  files and run `pr --repo OWNER/REPO --title-file FILE --body-file FILE`, where
  `OWNER/REPO` is the repository the pull request is opened in. Then create or
  edit the pull request from those files. This covers `gh pr create`,
  `gh pr edit`, `gh api .../pulls --input FILE` (build its JSON from the
  checked files), and the titles and bodies that `gh stack submit` and
  `gh stack link` create, which you reconcile with `gh pr edit` afterwards.

Act on its result:

- Exit 0 with `is not public; nothing checked` or `nothing to rewrite`:
  continue.
- Exit 0 with `rewrote ...` lines: the staged files and the files you passed
  now hold the rewritten text, and unstaged edits to those files get the same
  rewrite. Continue, and report each `rewrote` and `placeholder` line.
- `warning: commit ... pushing publishes that commit unchanged`: history is
  never rewritten. Report the named commits and files to the user with the
  push.
- `warning: PATH was not checked`: the guard could not read that file as text.
  Check it yourself for private repository names and local paths before
  publishing, and report it.
- `warning:` about `.github/publication-guard.json` or a keep pattern: report
  it; the guard used its default keep list for what it could not read.
- Exit 2: Git or GitHub could not answer a question the guard depends on, or
  an argument was wrong, and nothing was changed. Fix the cause the error names
  and rerun. Never publish without a run that exits 0.
- Any other exit: the guard failed. Stop, do not publish, and report its
  output.

## Merge

- Squash-merge pull requests and delete the source branch afterward.
- Because the merge is a squash, the pull request **title** becomes the commit
  subject on the base branch: the branch's own commit subjects do not survive.
  Give the title a Conventional Commit subject that states the observable impact
  of the change as a whole. Pick its type from the net effect rather than the
  most frequent commit under it. Where a project generates its changelog or
  version bump from commit history, a non-conforming title merges cleanly and is
  then silently absent from it.
- After a squash merge, sync the local base and remove the merged local branch.

## Native GitHub stacks

Use the official `gh stack` workflow when the user selects a stack, when work
has a real dependency chain, or when a confirmed large issue is deliberately
split into cumulative, independently reviewable layers. Do not stack unrelated
work. Each layer must have one clear claim, a bounded diff, its own validation,
and an explicit subset of the requirements.

Read [references/github-stacks.md](references/github-stacks.md) before creating,
syncing, pushing, submitting, reviewing, or merging a stack. That reference is
the only exception to the merge-only and never-force-push defaults above:
rebases and force-with-lease are permitted only when performed by verified
`gh stack` commands against a clean, confirmed native stack. Raw `git rebase`,
`git push --force`, and manual `git push --force-with-lease` remain forbidden.
