# Fix mode

Applies only when the caller passed the exact `fix` qualifier. Fixing stays
inside the specified behavior; the boundaries in the skill still apply.

1. Reproduce each failure yourself through the external interface before
   inspecting implementation source, and record the setup, action, input, and
   observed result. A recorded observation of the failure, even on the exact
   revision, does not replace this run; it becomes the black-box check that
   step 3 reruns.
2. Apply the smallest in-scope fix and run only the focused developer checks
   needed for that fix.
3. Refresh the available test environment and rerun the same black-box
   behavior. Each edit invalidates the affected results, so rerun every
   requirement the edit could change.
4. Repeat while available access and environments allow safe progress.
5. Stop fixing for a material product, architecture, security, compatibility,
   or scope decision, and report the decision with the evidence gathered.
6. When a fix can be exercised only through a preview and no preview contains
   the changed revision, report the fix as applied but unverified. The caller
   owns any authorized push or deployment and must invoke this skill again on
   the resulting exact-revision preview.

Report the changed files and, for each fix, the failure it addressed and its
status: `applied and verified` with the rerun evidence, or `applied but
unverified` with the reason.
