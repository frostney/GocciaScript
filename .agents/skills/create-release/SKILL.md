---
name: create-release
description: >-
  Prepares a changelog-first release and, only when explicitly requested,
  publishes it through the repository's single established release path. Use
  when the user asks to prepare, cut, tag, or publish a release, bump the version,
  or generate release notes.
license: Unlicense OR MIT
compatibility: >-
  Requires git, Python 3.11 or newer, the GitHub CLI (gh) authenticated to the
  target repository, the internal `delivery-wait` skill, and network access.
  Supports the project's changelog tooling or a hand-maintained changelog.
---

# Create release

Prepare a release whose tag contains its changelog, then publish through exactly
one evidence-backed path when publication is authorized.

## Authorization

- **Prepare**: determine the version, update changelog/version declarations,
  validate, and open the release PR. Requests to prepare, bump, or generate
  notes authorize only this stage.
- **Publish**: after the PR merges, create or trigger the tag/release through the
  repository's established publisher. Requests to cut, tag, publish, or run
  `/create-release` authorize this stage too.
- When ambiguous, perform Prepare only.

## Invariants

- The changelog and version bump land before the tag, through a squash-merged PR.
- Use the repository's configured tools and current documentation. Regenerate
  generated changelogs rather than hand-editing them.
- Use an explicit version or recommend one from unreleased conventional commits
  and wait for the user's decision.
- Run the declared release-relevant gate and report only observed results.
- Never amend, force-push, force-update a tag, skip hooks, or publish through
  more than one path.

## Prepare

1. Resolve the authorization stage, remote default branch, clean working tree,
   changelog/version tooling, last release, remote tags, workflows, and release
   documentation.
2. Stop if there are no releasable commits.
3. Validate a supplied version, or recommend and confirm the next version from
   unreleased commits.
4. Create a release branch from the fresh remote base.
5. Generate the changelog section and update every authoritative version
   declaration using project tooling. Do not invent a manifest bump when the
   project derives its version from tags.
6. Run the release-relevant project gate, commit `chore(release): <version>`,
   and open a draft release PR through `/create-pr`. Include the changelog
   section and observed validation. Stop here for Prepare-only requests.

## Publish

1. Use `delivery-wait` to await and verify the squash merge for the exact PR
   head; never tag the open PR branch.
2. Refresh the merged base, then re-read the actual workflow YAML and release
   documentation. Identify separate owners for tag creation, GitHub release
   creation, artifact signing, and registry publishing.
3. Select exactly one route:
   - workflow owns tag and release: trigger or monitor it only;
   - agent owns tag, workflow owns release: push the verified tag once, then
     monitor;
   - workflow owns tag, agent owns release: verify its tag, then create one
     GitHub release;
   - agent owns both: only when no workflow owns either action, push the verified
     tag once and create one release.
4. Stop when ownership is ambiguous or documentation and workflow disagree.
5. Execute only the selected route and verify the final tag target, release,
   workflow result, artifacts, and registry state that the route owns. Use the
   helper's `wait workflow-terminal`, `wait tag-target`, and
   `wait release-assets` operations for those GitHub transitions; do not
   monitor unchanged state through model turns.

Lead with the release outcome, PR/release URL, selected publisher, and current
evidence. Never describe an unobserved state as complete.
