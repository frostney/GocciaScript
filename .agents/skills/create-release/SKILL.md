---
name: create-release
description: >-
  Prepares a changelog-first release and, only when explicitly requested,
  publishes it through the repository's single established release path. Use
  when the user asks to prepare, cut, tag, or publish a release, bump the version,
  or generate release notes.
license: Unlicense OR MIT
compatibility: >-
  Requires git and the GitHub CLI (gh) authenticated to the target repository,
  plus network access. A changelog generator (git-cliff by default; see
  project-structure) is recommended but optional — the flow also supports a
  hand-maintained changelog.
---

# Create release

## Instructions

Prepare a release whose tag contains its own changelog, then publish it through
exactly one established release path when the user has authorized publication.

```text
compute version → commit changelog/version → merge release PR → select one publisher → tag/publish
```

### Authorization stages

- **Prepare** computes or confirms the version, updates the changelog and version
  declarations, validates the result, and opens the release PR. A request to
  prepare a release, bump a version, or generate release notes authorizes only
  this stage. Stop after opening the PR.
- **Publish** begins only after the release PR is merged. It may create or push a
  tag, trigger or monitor a release workflow, and publish the GitHub release.
  `/create-release` or an explicit request to cut, tag, or publish the release
  authorizes this stage as well as Prepare.
- When the requested boundary is ambiguous, perform Prepare only and state that
  tagging and publication remain pending.

### Rules

- **Changelog before tag.** The tag points at a commit that already contains the
  changelog and any version bump.
- **Release through a pull request.** Land the release commit through a
  squash-merged release PR; never commit it directly to the base branch.
- **Version supplied or confirmed.** Use an explicit version or recommend one
  from the unreleased conventional commits and wait for confirmation.
- **Project tooling wins.** Use the repository's configured changelog and version
  tools, verify their current flags, and let live documentation override this
  skill. Generated changelogs are regenerated rather than hand-edited.
- **Validate before handoff.** Run the repository's declared release-relevant
  checks before committing and opening the release PR. Never claim an unrun gate.
- **Publication ownership gate.** Immediately before any action that can publish
  or trigger publication, inspect the merged base branch's workflows and release
  documentation and identify who owns tag creation, GitHub release creation, and
  other publishing side effects. Choose one path; never duplicate a workflow.
- **No history rewriting.** Never amend, force-push, or force-update a release
  tag. Do not skip hooks unless the user explicitly asks.

Defer to `project-structure` for changelog tooling (git-cliff by default) and conventions, to `git-workflow` for branch naming and merge/push rules, and reuse `/create-pr` to open the release PR (and `/update-pr` if it needs follow-up commits).

### Steps

1. **Resolve the authorization stage and preflight.**
   - State whether this run is **Prepare** or **Prepare + Publish** from the user's
     request. Do not silently cross from Prepare into Publish.
   - Confirm `git` and `gh` are installed and authenticated.
   - Detect the changelog mechanism: git-cliff with a `cliff.toml` (default), the project's configured release/changelog tool, or a hand-maintained changelog. Confirm the relevant tool is available and print its version.
   - Resolve the base branch from the remote default (do not hardcode `main`):

     ```bash
     BASE_BRANCH=$(gh repo view --json defaultBranchRef --jq '.defaultBranchRef.name')
     ```

   - Ensure a clean working tree and an up-to-date base. List remote tags before
     fetching any needed release tag explicitly; do not force-update unrelated
     moving tags such as `nightly`:

     ```bash
     git fetch origin "$BASE_BRANCH"
     git switch "$BASE_BRANCH" && git pull --ff-only origin "$BASE_BRANCH"
     git ls-remote --tags origin
     ```

   - Inspect `.github/workflows/`, release documentation, and package-publishing
     configuration. Record the apparent release trigger and owners as a lead;
     the publishing gate re-checks them from the merged base before acting.

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

7. **Validate the prepared release.** Run the repository's declared checks that
   cover the changelog, version declarations, generated files, build, and tests.
   If a required check cannot run, stop or surface the limitation; do not open a
   release PR described as ready.

8. **Commit the release** with the type the generator skips:

   ```bash
   git commit -m "chore(release): $VERSION"
   ```

   Let the pre-commit hooks run (markdownlint on the changelog, etc.); do not skip them.

9. **Open the release PR** via `/create-pr`. Title `chore(release): $VERSION`; body = the new changelog section (with git-cliff: `git cliff --unreleased --tag "$VERSION" --strip all`; otherwise the section you just wrote). Include the validation performed. It opens as a draft — mark it ready once the diff looks right. If this run is Prepare only, report the PR and stop here.

10. **Wait for the release PR to be merged, then verify it.** This step belongs to Publish. Do not create or push a tag while the PR is open. The PR must be squash-merged, placing the changelog and version bump on the base branch as one commit.

    - Confirm the merge by polling `gh pr view <pr> --json state,mergedAt,mergeCommit` until `state` is `MERGED`, or by pausing until the user confirms they have merged it.
    - The merge is normally left to review/CI and performed by the user; squash-merge it yourself here only if the user has authorized the agent to merge.

11. **Run the publication ownership gate.** Refresh the merged base, then read the
    actual workflow YAML and release documentation again. Search workflow
    triggers and steps, not filenames alone, including tag pushes,
    `workflow_dispatch`, release events, `gh release`, release actions, registry
    publishing, and signing or artifact jobs. Check recent workflow runs or
    releases when the intended owner is still unclear.

    Before acting, state one of these evidence-backed routes:

    - **Workflow owns tag and release.** Use only its documented trigger or wait
      for the merge-triggered run. Do not create a tag or call `gh release create`.
    - **Agent owns tag; workflow owns release.** Tag and push the verified merge
      commit once, then monitor the workflow. Do not call `gh release create`.
    - **Workflow owns tag; agent owns release.** Trigger or wait for the workflow,
      verify its tag, then create the GitHub release once from that tag.
    - **Agent owns tag and release.** Only when no workflow owns those actions,
      tag and push the verified merge commit, then create the GitHub release.

    If ownership is ambiguous, multiple paths could publish, or the workflow and
    documentation disagree, stop and ask. Do not test the ambiguity by publishing.

12. **Execute exactly the selected route.** When the agent owns the tag, tag the
    squash-merge commit rather than assuming it is `HEAD`, then push it once:

    ```bash
    git switch "$BASE_BRANCH"
    git pull --ff-only origin "$BASE_BRANCH"
    git tag -a "$VERSION" -m "$VERSION" <merge-commit-sha>
    git push origin "$VERSION"
    ```

    When the agent also owns GitHub release publication, create it once from the
    tag using the changelog notes. When a workflow owns publication, monitor its
    run to completion instead; never add a manual fallback while it is pending or
    failed without first diagnosing the failure and getting approval to change
    publishers.

13. **Verify and report.** Confirm that the published tag points at the intended
    merge commit and contains the changelog, and that the single selected
    publisher completed. Report the version, tag SHA, release PR URL, GitHub
    release URL, workflow run URL when applicable, validation, and artifacts.

### Notes

- **Prerelease / draft.** Compute or pass a prerelease version (e.g. `v1.2.0-rc.1`) and use the selected publisher's prerelease or draft mechanism. Use `gh release create --prerelease`/`--draft` only on the agent-owned path.
- **Tag-derived versions.** When the version comes from the tag (Go modules, setuptools-scm, etc.), skip the manifest bump in step 6; the release commit carries only the changelog and the ordering invariant still holds.
- **Monorepo / multiple manifests.** Bump every manifest that declares the version in step 6, and scope the changelog tool (tag pattern, include paths) to the package being released.
- **Signed tags.** Use `git tag -s` instead of `-a` when the project requires signed release tags.
- **Automated publishing.** A tag-triggered workflow that publishes the GitHub
  release is the publisher. Push the tag once and monitor it; do not also run
  `gh release create`.
