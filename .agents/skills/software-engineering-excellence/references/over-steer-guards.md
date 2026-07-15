# Over-steer guards

Every principle in the skill is a virtue, and every virtue becomes a failure when you push it to the exclusion of the others. This file names the failure mode for each, the tell that you are in it, and how to rebalance. The tiebreaker for all of them is the same: **maintainability — the ease of the next change.**

A general rule before the specifics: if you find yourself working *harder* on a principle than the change in front of you warrants, that effort is probably crossing from virtue into over-steer. The cost of the work is part of the equation, not separate from it.

## 1. Grounding → analysis paralysis

**Over-steer:** reading endlessly, re-deriving context you already have, refusing to act until you have seen everything.
**Tell:** you have read the same area three times; you are gathering more context but your confidence is not rising; you have written nothing and the picture stopped changing a while ago.
**Rebalance:** ground until your picture is *correct*, not *complete*. The bar is "I can act without guessing," not "I have seen all of it." Start, and let contact with the real problem surface the next thing worth learning.

## 2. Reuse → forced abstraction

**Over-steer:** bending an existing function to serve a case it was never meant for, or hoisting a shared abstraction over two things that only superficially resemble each other.
**Tell:** the "reused" function now has a boolean parameter that switches its behavior; the abstraction needs a comment to explain which path applies when; callers must understand its internals to use it safely.
**Rebalance:** reuse where the *meaning* is shared, not merely the shape. Two things that look alike today but answer to different reasons will diverge tomorrow — duplication is cheaper than the wrong abstraction. Prefer extracting the genuinely-common core and leaving the specifics separate.

## 3. Completeness → gold-plating

**Over-steer:** building for requirements no one has, handling inputs that cannot occur, generalizing a concrete need into a framework.
**Tell:** you are adding configuration nobody asked for; you are handling a case you cannot actually produce; the phrase "in case we ever need to" is doing the justifying.
**Rebalance:** "production-ready" is sized to the *real* requirement and its real paths — every path that can actually happen, and nothing past that. Completeness means the in-scope problem is wholly solved, not that every imaginable future is pre-built. (This is the mirror of principle 5: build the right thing, then build it completely.)

## 4. Validation → ritual or paralysis

**Over-steer:** running every possible check on every change regardless of risk, or never shipping because there is always one more thing to verify.
**Tell:** you are running the full exhaustive suite to validate a comment fix; verification time dwarfs the change; you keep finding reasons a low-risk change "isn't proven yet."
**Rebalance:** validate *to the risk*. The non-negotiable is that you never *claim* what you have not run — but what you choose to run scales with what could break. High-risk change: verify deeply, every mode. Trivial change: verify proportionally. Coverage of what matters, not ceremony for its own sake.

## 5. Right-value → under-building

**Over-steer:** using "does this carry its weight?" as a license to skip the unglamorous-but-load-bearing work — the error path, the test for the case that genuinely fails, the boring validation.
**Tell:** "YAGNI" is being invoked to drop something that *is* needed; the thing being removed has a real caller or prevents a real failure; you are trimming coverage of what matters, not ceremony.
**Rebalance:** the value question cuts ceremony, never coverage of what matters. "Who wants this surface?" has a real answer here — and when it does, build it fully (principle 3). Right-value and completeness are partners: pick the right things, then do them completely.

## 6. Holding the line → rigidity or silence

**Over-steer (rigidity):** clinging to a decided approach after real evidence shows it is wrong, because "the decision was made." **Over-steer (silence):** surfacing so often that you stop and ask at every trivial fork, or going quiet and doing nothing when you could have recommended-and-proceeded.
**Tell:** you are defending a choice the evidence no longer supports; or you are asking permission for something the request already authorized; or a throwaway spike has stalled waiting for input it did not need.
**Rebalance:** hold decisions stable *against churn*, not *against evidence* — when the ground genuinely shifts, change course out loud and re-validate. Calibrate surfacing to stakes: clear path and met bar → proceed; genuine fork or unmet bar → surface. The failure is the *silent* call in either direction, not the act of deciding.

### Correction → compensation spiral

**Over-steer:** responding to criticism by increasing activity: broadening scope, adding infrastructure, changing a decided tool, or fixing adjacent issues to prove seriousness.
**Tell:** the next change is larger than the rejected one; you are adding a new framework or harness after the user objected to direction; the work feels like a lurch rather than a narrower repair.
**Rebalance:** pause acceleration. Restate the broken contract, name the smallest corrective change, check existing decisions, and continue only within that boundary. After correction, increase precision, not force.

## Structure → over-building (bikeshedding instead of shipping)

**Over-steer:** treating "invest in the right structure" as license to build *more* structure than the problem needs — perfecting the architecture, stacking layers, abstractions, and configuration around a slice that does not yet work end-to-end, or bikeshedding names and boundaries while nothing actually ships. The same failure wears a second mask once you adopt the walking-skeleton habit: standing up deploy pipelines, environments, and infrastructure ceremony *before* there is a real path worth running through them.
**Tell:** you have re-architected more than you have made work; there is no correct end-to-end path running yet but you are polishing the scaffolding; the debate is about the shape of the structure rather than about getting a real input to produce a real output. Or: you have an elaborately deployed shell with rich CI but no real behavior through it — and you are quietly treating "it deployed" or "the skeleton ran" as *done*.
**Rebalance:** get a correct end-to-end path working first — the thinnest real slice that runs — then invest in the structure around it. Keep the skeleton thin and calibrated to stakes: prove the path, don't pre-build infrastructure, and remember the skeleton is the *first increment, not the finish*. Structure (and pipeline) earns its keep by making the *next* change cheaper on code that already works; built ahead of a working path it is speculation. Resisting the quick fix means fixing at the *right layer*, not building the *biggest* structure — the smallest change at the right layer still wins.

## The meta-guard

When two principles genuinely conflict and the guards above do not settle it, ask the one question that always applies: **which choice leaves the next change easier?** That is the governor. It is also why none of these principles is absolute — they all serve maintainability, and when one of them stops serving it, that one yields.
