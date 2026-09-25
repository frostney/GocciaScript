---
name: create-pr
description: >-
  Validates and repairs an in-scope change, publishes its draft pull request,
  reconciles metadata and CI, and marks it ready for review. Use
  when the user runs /create-pr.
license: Unlicense OR MIT
compatibility: >-
  Requires git, Python 3.11 or newer, the GitHub CLI (gh) 2.99 or newer
  authenticated to the target repository with push access, the internal
  `delivery-wait` skill, and network access.
---

# Create PR

The request authorizes the repository's declared gates, relevant commits, PR
metadata updates, one ordinary draft pull request or the confirmed native stack
layers owned by the change, required review and behavior testing, in-scope fixes,
and transitions to ready for review. This includes the project's existing PR
preview path when required to test the change. Merge and integration delivery
remain with an authorized parent such as `/deliver`; a standalone publication
request does not add those endpoints or unrelated changes.

Prepare reviewer-facing media from the completed change, following
[references/walkthroughs.md](references/walkthroughs.md) when there is useful
behavior to demonstrate. Missing recording, narration, subtitles or upload
capabilities do not block publication or readiness by themselves. Report each
gap and its remedy to the user; required behavior evidence and project gates
still apply.

When the branch belongs to a native GitHub stack, read
[../git-workflow/references/github-stacks.md](../git-workflow/references/github-stacks.md).
The request then authorizes submission and metadata reconciliation for the
confirmed stack layers owned by the current change, not unrelated branches.

1. Inspect the working tree, staged diff, recent commits, remote default branch,
   stack topology when applicable, and any existing remote head. Preserve
   unrelated local work.
2. Stop if there are no relevant changes or commits ahead of the remote base.
   Continue without an empty commit when the work is already committed.
3. If currently on the base branch, create a focused branch named from the issue
   or change.
4. Establish the PR claim and publication requirements before publishing
   anything. Read the explicit request, linked issue or confirmed mini-spec,
   applicable project instructions, product docs, ADRs or durable decisions,
   the nearest `DEFINITION_OF_READY.md`, and the completion evidence supplied by
   the implementation workflow. If no source states the claim, reconstruct the
   narrowest claim supported by the commits and diff and label it as inferred.
5. Apply `/code-review fix-all` and `/test-against-spec fix` to the current
   requirements and change. Reuse valid matching results; run missing or stale
   review and real-interface checks. If the claim was inferred from the diff,
   establish explicit expected behavior before specification testing. Resolve a
   material ambiguity without inventing requirements from the implementation.
6. Repair verified in-scope requirement, fidelity, test and compatibility gaps
   through `/implement`'s development loop; use the same documented checks
   directly if a companion skill is unavailable. Every verified requirement gap
   must be resolved regardless of severity. After edits, obtain renewed review
   and specification results for the affected scope before publication or
   readiness. Targeted finding revalidation can reuse unchanged review coverage;
   fixing a finding does not itself renew the review result. Run the missing or
   invalidated project gate after fixes converge. Preserve evidence for unchanged
   content, environment and coverage. A failure routes to repair, not a ritual stop.
   When required behavior can only be tested on a PR preview, open the draft
   needed to obtain it under this publication request, then test that exact
   revision and resume the loop. Keep it draft while evidence is missing;
   recording a walkthrough does not replace behavior testing.
7. Stage only relevant files, excluding secrets and unrelated local work.
   Commit uncommitted work with a concise Conventional Commit subject. Never
   amend and never skip hooks. Preserve already-published history and add a new
   commit for any correction.
8. Title each pull request with a Conventional Commit subject covering the whole
   change, since the squash merge makes that title the commit subject on the
   base branch. Follow
   [../agent-writing/references/pr-descriptions.md](../agent-writing/references/pr-descriptions.md)
   for the body, preserving explicit project-template requirements. Before writing it, search
   open and closed issues and recent sibling sessions or adjacent branches when
   available for related findings and duplicates. Put each closing keyword on
   its own line as `Closes #N`, and only on the layer that completes that issue.
   Use a supported upload path for local screenshots and videos; inspect the
   installed GitHub CLI's attachment support. With `--attach`, use
   `'<image>#<alt text>'` for images and a plain file path for videos. Verify
   the uploaded asset in the actual PR. A partial upload failure can still
   create the PR: reconcile the returned URL and successful attachments before
   retrying. Never commit evidence media to the repository.
9. After the publication checks pass, or for the preview-only draft needed in
   step 6, push an ordinary branch normally and set its upstream when needed,
   then open one draft PR against the remote default. For a verified native
   stack, follow the owning native-stack reference: a new top layer above frozen
   approved PRs uses protected push, separate PR creation and native append;
   broader authorized submissions use `gh stack submit`. Require current evidence
   for every published layer. When only required preview evidence is missing,
   publish the necessary drafts through that same permitted path after the
   available checks pass, obtain their exact-revision previews and resume testing
   before readiness. Only guarded official stack
   operations may rebase or push with force-with-lease. Preserve bottom-to-top
   topology and keep each layer draft.
10. Run the PR-specific phase. Compare the actual PR diff, body, links, metadata,
    committed tests and documentation, supplied completion evidence, and facts
    that exist only after publication. Correct metadata-only gaps without a
    commit.
11. If the PR-specific phase or CI exposes an in-scope repository, behavior,
    fidelity, test, documentation or compatibility gap, keep the PR draft and
    repair it through the development loop. Revalidate affected requirements,
    then use `/update-pr` for the same PR and verify its new head. Continue until
    all verified requirement gaps are resolved. Ask only for a material choice,
    new authority or external blocker after safe alternatives are exhausted.
12. Invoke `delivery-wait`'s foreground `wait checks-terminal` operation with
    the repository, PR number, exact head, required `--check` contexts, absolute
    deadline and `--json`. Passively await meaningful transitions; inspect failed
    logs and return established in-scope causes to step 11. Do not substitute
    repeated model turns for a passive wait. Report unavailable host capability.
13. Keep the PR draft while required evidence or CI is pending or failing.
    Test an exact-revision preview when publication was needed to obtain one.
    If expected PR checks have no run, inspect mergeability and resolve an
    established conflict through `/update-pr` before considering a retrigger.
    Continue pending transitions through the deterministic wait; an unavailable
    external dependency remains pending or blocked, never a passed check.
14. Once the PR is missing nothing required by the publication and PR-specific
    phases and all applicable CI is observed green for its exact head, mark it
    ready for review. Return every affected URL, native stack order when
    applicable, final states, metadata changes, supplied completion evidence,
    and observed readiness and CI evidence. Include incomplete walkthrough
    requirements and actionable remedies, even when the PR is ready.
