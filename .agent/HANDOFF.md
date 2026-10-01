# Handoff

Updated: 2026-10-01, after the 0.14.0 release and its retrospective.

The previous handoff, the September string-accumulator audit (merged as #1235),
is in git history at `13751a63:.agent/HANDOFF.md`.

## Where things stand

Nothing from this workstream is open. Everything below is merged, released or
filed as an issue.

- **0.14.0 is released:** [release](https://github.com/frostney/GocciaScript/releases/tag/0.14.0).
  - The website's `/api/versions` defaults to 0.14.0.
  - The Homebrew tap is updated (frostney/homebrew-tap#20).
  - The headline is the unified capability model, [ADR 0122](../docs/adr/0122-unified-capability-model.md).
  - The changelog annotations for renamed and removed flags come from `cliff.toml` postprocessors.
- **CI changes after the release:**
  - #1291 (fixing #1289) cancels superseded `main` runs by commit ancestry in the `supersede` job and moves nightly publishing to `nightly.yml`.
  - #1296 fixed the nightly's macOS archive names. The first nightly under the new workflow published from `55a16cb1` with all six archives.
  - The `test` job has a 30-minute timeout.
- **Docs (#1292):**
  - Contributor and agent docs link the known-good-route rules instead of restating them.
  - `docs/contributing/workflow.md` § Full CI is the platform gate.
  - `milestone-rush` is vendored.
- **Skills (#1297):** the vendored known-good-route skills match upstream `main` at `b5e6981`. That brings in:
  - the CodeRabbit adapter's allowance budget and finished-review recognition;
  - `delivery-wait`'s `checks-terminal --all-workflows`;
  - the `create-pr` native-stack trigger and delegation rule.

## Next steps

Open issues from this workstream, in suggested order:

1. **[#1293](https://github.com/frostney/GocciaScript/issues/1293):** a nested `runScript` runs its caller's promise callbacks and drops them if the child fails. It's a spec violation (ECMA-262 §9.5) and the most serious engine bug filed.
2. **[#1295](https://github.com/frostney/GocciaScript/issues/1295):** an unhandled promise rejection is silently ignored and the run exits 0. It interacts with #1293.
3. **[#1294](https://github.com/frostney/GocciaScript/issues/1294):** a `--timeout` while a fetch is pending surfaces as a catchable HTTP error and exits 0, which contradicts `docs/errors.md`.
4. **[#1290](https://github.com/frostney/GocciaScript/issues/1290):** make verify-on-load enforceable for every module loader. Text assets are untested today.
5. **Upstream in known-good-route:** fix skill and helper defects there, then sync.
   - [#97](https://github.com/frostney/known-good-route/issues/97): `milestone-rush` `validate` accepts superseded work without a cancellation or blocker.
   - [#100](https://github.com/frostney/known-good-route/issues/100): `update-pr` and `git-workflow` should keep a backport's named base.
6. **Watch known-good-route's open PRs #103–#108,** opened 2026-09-30 outside this workstream. #103 changes the CodeRabbit adapter again. Once they merge, run a GocciaScript skills sync, as in #1297.

Known but not filed (the maintainer declined on 2026-09-26): fetch workers are never joined at process exit. The crash this caused no longer reproduces since #1259; it's hardening only.

## Decisions to keep

- **Readiness:** PR readiness goes through `/address-feedback` and its helpers, never a count of unresolved threads. CodeRabbit puts outside-diff and nitpick findings in review bodies.
- **Vendored skills:** never hand-edit a skill listed in `skills-lock.json`. Fix it upstream in known-good-route and sync. Repo-native skills are edited only when the maintainer asks.
- **Dependent PRs:** a PR that builds on another open PR goes up as a native GitHub stack (`gh stack`), never with its base set by hand to another PR's branch. Squash merges break the hand-based form (known-good-route #96 conflicted after #94 merged).
- **PR bases:** PRs target the default branch unless the maintainer or project instructions name a long-lived base, such as a release branch for a backport.
- **Full CI:** run `gh workflow run ci.yml --ref <branch>` before marking ready for stacked branches and platform-sensitive changes. PR CI is Linux x64 only.
- **CodeRabbit allowance:** the allowance is currently 1 review an hour. Check `coderabbit_adapter.py status --head PR=SHA` before marking several PRs ready, and space them by its `retryAt`.

## Working preferences

- No AI attribution anywhere: no `Co-Authored-By`, no "Generated with", no "Created on behalf of" notes. This overrides skill text that asks for them.
- PR titles state the impact as a Conventional Commit subject. Never "round-N review fixes".
- Merge, never rebase. Never amend or force-push. PRs are squash-merged, and the maintainer merges.
- Assistants run the test suite with `-P`, never `--trust`.
- Delegated work publishes through `/create-pr` and the other loop skills, not hand-written `gh pr create` or `gh pr merge` commands.

## Gotchas

- **Waiting on CI:** use `delivery_wait.py wait checks-terminal --all-workflows`. A hand-built `--check` list misses jobs that start later. Wait on a dispatched full CI run with `wait workflow-terminal --run-id <id> --head <sha>`, because its checks don't show on the PR.
- **CodeRabbit adapter:** `--head` takes `PR=SHA`. CodeRabbit skips draft PRs, and an unknown allowance shows as `degraded`.
- **Packaging:** zips every target matching `*win*`, so "darwin" is zipped too. macOS release and nightly archives are `.zip`; Homebrew and the website installers depend on that.
- **Stale builds:** after a merge or branch switch, rebuild with `./build.pas --clean <target>` before diagnosing an FPC error.

## Suggested skills

`deliver` (or `implement` → `create-pr` → `address-feedback`) for the issues above. `git-workflow` for branching and stacks. `delivery-wait` for CI. `gocciascript-issue-validation` for #1293–#1295. `create-issue` for new findings. `run-retro` after the next milestone.
