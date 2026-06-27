---
name: create-release
description: >-
  Cuts a release so the tag always contains its own changelog: computes the next
  version from the conventional commits, writes the changelog and bumps the
  version on a release branch, opens a release pull request, and only after it is
  squash-merged tags the merge commit and publishes the GitHub release — never a
  changelog pull request that lands after the tag and isn't in the release. Uses
  a version passed in by the user, or otherwise recommends one from the changes
  since the last release and asks the user to confirm before proceeding. Defaults
  to git-cliff for changelog generation but works with any changelog tool or a
  hand-maintained changelog, and bumps the version with the project's own tooling
  regardless of language. Use when the user runs /create-release or asks to cut,
  tag, or publish a release, bump the version, or generate release notes.
license: Unlicense OR MIT
compatibility: >-
  Requires git and the GitHub CLI (gh) authenticated to the target repository,
  plus network access. A changelog generator (git-cliff by default; see
  project-structure) is recommended but optional — the flow also supports a
  hand-maintained changelog.
---

# Create release

## Instructions

Explicit permission to compute the next version, generate the changelog, open a release pull request, and — once it is squash-merged — tag the release and publish it.

The whole point of this flow is the **ordering**, and it holds regardless of which tool produces the changelog. A changelog generator (git-cliff by default) can compute the next version and render its section from the *unreleased* commits before any tag exists; even without one, you decide the version and write the section yourself. Either way the changelog is committed first and the tag lands on a commit that already contains it:

```text
correct  →  compute next version (no tag yet)  →  changelog + version bump committed  →  release PR merges  →  tag the merge commit
broken   →  tag v1.2.0  →  generate the changelog after the fact  →  commit/PR lands AFTER the tag  →  not in the release
```

### Rules

- **Changelog before tag.** The changelog and version bump are committed *before* the tag exists; the tag points at a commit that already contains them. Never tag first and add the changelog afterward — that produces a changelog commit or pull request that lands after the release and isn't included in it. This is the one invariant the whole skill exists to protect; everything below serves it.
- **Release through a pull request.** Land the release commit via a squash-merged release PR, per `git-workflow`. Never commit the release directly to the base branch.
- **Tag the post-merge commit.** Squash-merge rewrites SHAs, so tag the commit that exists on the base branch *after* the merge — not the release-branch commit, and not before the merge.
- **Don't tag until the release PR is merged.** After opening the release PR, wait for it to be squash-merged and confirm the merge before creating the tag. Never tag a release whose PR is still open or could still change.
- **Tagging is allowed; force-push and amend are not.** Creating a tag on the base branch and pushing it with a plain `git push` is fine — `git-workflow` forbids force-push and direct *commits* to the base branch, not tags. No `--force`, no `--force-with-lease`, no `git commit --amend`.
- **Changelog generation is pluggable; git-cliff is the default.** Use git-cliff when it is available and configured; otherwise use the project's configured changelog/release tool, or maintain the section by hand following the repo's changelog convention. See `project-structure` for the tool choice and when to deviate. The ordering invariant above is independent of the tool.
- **Version is supplied or confirmed, never silently chosen.** If the user passed a version (a `/create-release` argument or in the request), use it. Otherwise recommend one from the conventional commits since the last tag — the changelog tool's computed bump, the project's release tool, or the conventional-commit rules by hand (a breaking change → major, `feat` → minor, otherwise → patch) — present it alongside the commits since the last release, and ask the user to confirm or pick another before proceeding.
- **Bump the version with the project's own tooling, in every place it's declared** — never hand-edit when a bumper exists, so lockfiles and generated files stay consistent. The mechanism is language-specific (defer to the stack skill). Some ecosystems derive the version from the git tag and have no manifest to bump.
- **The generated changelog is not hand-edited.** When a generator is used and the wording is wrong, fix the offending conventional commit and regenerate rather than editing the output. Configure the generator to skip the `chore(release)` commit (e.g. git-cliff's `commit_parsers`) so the release commit itself never appears in a later changelog.
- **Verify release tooling live.** Confirm the installed version of the changelog generator and any release tool before running; their flags evolve. Live tool docs override this skill on conflict.
- **Don't skip hooks or verification** unless the user explicitly asks.

Defer to `project-structure` for changelog tooling (git-cliff by default) and conventions, to `git-workflow` for branch naming and merge/push rules, and reuse `/create-pr` to open the release PR (and `/update-pr` if it needs follow-up commits).

### Steps

1. **Preflight.**
   - Confirm `git` and `gh` are installed and authenticated.
   - Detect the changelog mechanism: git-cliff with a `cliff.toml` (default), the project's configured release/changelog tool, or a hand-maintained changelog. Confirm the relevant tool is available and print its version.
   - Resolve the base branch from the remote default (do not hardcode `main`):

     ```bash
     BASE_BRANCH=$(gh repo view --json defaultBranchRef --jq '.defaultBranchRef.name')
     ```

   - Ensure a clean working tree and an up-to-date base:

     ```bash
     git fetch --tags origin
     git switch "$BASE_BRANCH" && git pull origin "$BASE_BRANCH"
     ```

2. **Confirm there is something to release.** If there are no releasable commits since the last tag, stop and say so. With git-cliff: `git cliff --unreleased` is empty. Otherwise: `git log <last-tag>..HEAD` has nothing release-worthy.

3. **Determine the version.**
   - **If the user supplied a version** (a `/create-release` argument or in the request), use it. Validate that it is well-formed and ahead of the last tag.
   - **Otherwise, recommend and confirm.** Compute a recommended version from the conventional commits since the last tag, then present it together with those commits and ask the user to confirm or choose another. Proceed only once the user has decided — do not auto-pick.
     - With git-cliff (default): `VERSION=$(git cliff --bumped-version)` for the recommendation; list the commits with `git cliff --unreleased` (or `git log <last-tag>..HEAD`).
     - Otherwise: derive the recommendation from the project's release tool or the conventional-commit bump rules by hand.

4. **Create the release branch** off the fresh base, per `git-workflow` naming:

   ```bash
   git switch -c "release/$VERSION"
   ```

5. **Generate the changelog** for that version into the project's changelog file. The intent is to render the section for the unreleased commits under the new version.
   - With git-cliff (default): `git cliff --tag "$VERSION" -o CHANGELOG.md` regenerates the whole file idempotently; for a large existing file, prepend only the new section with `git cliff --tag "$VERSION" --unreleased --prepend CHANGELOG.md`.
   - Otherwise: produce the same section with the project's changelog tool, or write it by hand following the repo's changelog convention (e.g. Keep a Changelog) from the commits since the last tag.

6. **Bump the version wherever it is declared**, using the project's own tooling so derived files stay consistent — e.g. `cargo set-version "$VERSION"` (Rust), `npm version "$VERSION" --no-git-tag-version` (Node/TypeScript), the `pyproject.toml` bumper (Python), the gem's `version.rb` (Ruby), or the manifest field the project's language uses. If the project derives its version from the git tag (e.g. Go modules, setuptools-scm), there is no manifest to bump — the tag *is* the version, and the release commit carries only the changelog.

7. **Commit the release** with the type the generator skips:

   ```bash
   git commit -m "chore(release): $VERSION"
   ```

   Let the pre-commit hooks run (markdownlint on the changelog, etc.); do not skip them.

8. **Open the release PR** via `/create-pr`. Title `chore(release): $VERSION`; body = the new changelog section (with git-cliff: `git cliff --unreleased --tag "$VERSION" --strip all`; otherwise the section you just wrote). It opens as a draft — mark it ready once the diff looks right.

9. **Wait for the release PR to be merged, then verify it.** Do not create the tag while the PR is still open — wait for the merge and confirm it before continuing. The PR must be **squash-merged** (per `git-workflow`), with the squash message `chore(release): $VERSION`, placing the changelog and version bump on the base branch as one commit; delete the release branch after the merge.
   - Confirm the merge by polling `gh pr view <pr> --json state,mergedAt,mergeCommit` until `state` is `MERGED`, or by pausing until the user confirms they have merged it.
   - The merge is normally left to review/CI and performed by the user; squash-merge it yourself here only if the user has authorized the agent to merge.

10. **Sync and tag the merge commit.** This is the only place a tag is created, and it happens *after* the changelog is on the base branch:

    ```bash
    git switch "$BASE_BRANCH"
    git pull origin "$BASE_BRANCH"
    git tag -a "$VERSION" -m "$VERSION"   # annotated tag on the squash-merge commit
    ```

    If other commits landed on the base branch after the release PR merged, tag the squash-merge commit by its SHA rather than `HEAD`. Sanity-check that the tagged commit contains the changelog: `git show "$VERSION":CHANGELOG.md | head`.

11. **Push the tag** (plain push, never force):

    ```bash
    git push origin "$VERSION"
    ```

12. **Publish the GitHub release** from the tag, with notes from the changelog. With git-cliff: `gh release create "$VERSION" --title "$VERSION" --notes "$(git cliff --latest)"`. Otherwise pass the extracted section via `--notes-file`. Add `--prerelease` for an `-rc`/`-beta` version or `--draft` to stage it first. Attach build artifacts when the project produces them (defer to the deployment docs).

13. **Report:** the version, the tag and the commit SHA it points at, the release PR URL, the GitHub release URL, and confirmation that the tagged commit contains the changelog.

### Notes

- **Prerelease / draft.** Compute or pass a prerelease version (e.g. `v1.2.0-rc.1`) and pair it with `gh release create --prerelease`/`--draft`.
- **Tag-derived versions.** When the version comes from the tag (Go modules, setuptools-scm, etc.), skip the manifest bump in step 6; the release commit carries only the changelog and the ordering invariant still holds.
- **Monorepo / multiple manifests.** Bump every manifest that declares the version in step 6, and scope the changelog tool (tag pattern, include paths) to the package being released.
- **Signed tags.** Use `git tag -s` instead of `-a` when the project requires signed release tags.
- **Optional CI publish.** A tag-triggered workflow that only publishes the GitHub release from the pushed tag (reading the latest changelog section) is fine. It must *not* generate or commit the changelog — that already happened before the tag in steps 5–9, which is what keeps the release self-contained.
