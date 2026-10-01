# Handoff

Updated: 2026-10-01, after the 0.14.0 release, its retrospective, and the #1293 fix.

The previous handoff, the September string-accumulator audit (merged as #1235),
is in the GocciaScript repo history at `13751a63:.agent/HANDOFF.md`.

## Where things stand

- **#1293 has a fix in review:** [#1299](https://github.com/frostney/GocciaScript/pull/1299) on `fix/1293-nested-runscript-microtasks`. Check its current state with `gh pr view 1299`; the maintainer merges.
  - An `Execute` nested in a different engine's run gets its own scope of the thread's microtask queue. A promise reaction belongs to the scope that registered it. [ADR 0123](../docs/adr/0123-per-execution-microtask-scopes.md) records the design and the options rejected.
  - Two simpler designs were tried and rejected, each with a test now guarding it. Scoping every `Execute` dropped a `--globals` module's pending jobs in interpreted mode. Routing jobs by the scope a promise was created in ran a nested engine's callback after that engine was freed.
  - Known limits, stated in the PR: only `Execute` isolates, and a caller's `then` *getter* on a fetch response can still run during a nested drain.
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
- **Skills (#1297):** the vendored known-good-route skills are at `b5e6981`. Upstream `main` has since moved to `69f7102`, two commits ahead:
  - #101 keeps unlisted skill directories as project-authored in `update-project-skills`.
  - #102 makes every `code-review` and `address-feedback` finding state its impact, gain and cost of not doing it.

## Next steps

Open issues from this workstream, in suggested order:

1. **[#1299](https://github.com/frostney/GocciaScript/pull/1299):** see it through review to ready-to-merge.
2. **[#1295](https://github.com/frostney/GocciaScript/issues/1295):** an unhandled promise rejection is silently ignored and the run exits 0. It interacts with #1293, so recheck its reproduction once #1299 merges.
3. **[#1300](https://github.com/frostney/GocciaScript/issues/1300):** in bytecode mode, a test file that hits the file `--timeout` with microtasks queued crashes the next file with an access violation. Found during the #1299 review; it reproduces on 0.14.0.
4. **[#1294](https://github.com/frostney/GocciaScript/issues/1294):** a `--timeout` while a fetch is pending surfaces as a catchable HTTP error and exits 0, which contradicts `docs/errors.md`.
5. **[#1290](https://github.com/frostney/GocciaScript/issues/1290):** make verify-on-load enforceable for every module loader. Text assets are untested today.
6. **Upstream in known-good-route:** fix skill and helper defects there, then sync.
   - [#97](https://github.com/frostney/known-good-route/issues/97): `milestone-rush` `validate` accepts superseded work without a cancellation or blocker.
   - [#100](https://github.com/frostney/known-good-route/issues/100): `update-pr` and `git-workflow` should keep a backport's named base.
7. **Skills sync:** known-good-route #103 and #105–#108 are still open; #104 was closed unmerged. #103 changes the CodeRabbit adapter again. Once they merge, run a GocciaScript skills sync, as in #1297. It will also bring in #101 and #102.

Known but not filed (the maintainer declined on 2026-09-26): fetch workers are never joined at process exit. The crash this caused no longer reproduces since #1259; it's hardening only.

Observed but not filed: an `async` function's `await null` continuation runs later than in Bun relative to top-level awaits. After `(async () => { await null; log("a"); })(); await null; await null; log("b");` GocciaScript logs `b` then `a` in both modes and Bun logs `a` then `b`. It predates #1299.

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
- **Stale builds:** after a merge or branch switch, rebuild with `./build.pas --clean <target>` before diagnosing an FPC error. The same applies after moving a unit between `interface` and `implementation` uses, and after switching between `--prod` and development builds.
- **Linux workstation (`~/.t3` worktrees):** FPC 3.2.2 comes from Homebrew (`brew install fpc`). `npx` and `lefthook` are not installed, so:
  - commit hooks do not run; run `./format.pas --check`, `bunx markdownlint-cli2 <files>` and the `scripts/check-*.ts` checks from `lefthook.yml` with `bun` by hand;
  - the `tc39` MCP server cannot start; read clauses from `tc39.es/ecma262` instead.
- **Local test262:** run `GocciaTest262Runner` from the repository root with `--suite-dir` pointing at a checkout of the SHA in `scripts/test262-suite-sha.txt`. From any other directory every test reports a missing harness include. Compare per-test status against a base build; timeouts vary with machine load.
- **Pascal tests:** CI runs `build/*.Test`, which is 94 programs. `build/Goccia.*.Test` covers only 69 of them.
- **One Pascal test:** `fpc @config.cfg -FUbuild/compiled/tests/source/units/<Name>.Test -O- -gw -godwarfsets -gl -Ct -Cr -Sa source/units/<Name>.Test.pas` rebuilds a single test program without the 20-minute `./build.pas tests`.
- **CLI tests and Windows:** a `runScript` child's `stdout` uses the platform line ending. Normalise it after parsing it out of the guest's JSON; `normalizeLineEndings` on the runner's own stdout does not reach it.
- **GocciaScript in test scripts:** `while` loops, `function` expressions and `var` are rejected by default. Use array iteration, arrow functions and method shorthand.

## Suggested skills

`deliver` (or `implement` → `create-pr` → `address-feedback`) for the issues above. `git-workflow` for branching and stacks. `delivery-wait` for CI. `gocciascript-issue-validation` for #1294 and #1295. `create-issue` for new findings. `run-retro` after the next milestone.
