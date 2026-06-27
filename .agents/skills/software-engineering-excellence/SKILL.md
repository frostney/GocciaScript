---
name: software-engineering-excellence
description: >-
  Ambient engineering-quality skill that MUST be selected for any code edit,
  implementation, debugging pass, review fix, refactor, architecture change,
  test-harness change, or substantial technical investigation in any stack.
  Use this before acting on engineering work, especially after user
  correction or when scope, tooling, validation, or architecture might drift.
  Defines the standard for good and excellent engineering work, sitting above
  execution/workflow skills: grounding in reality, full-scope completeness,
  reuse over duplication, real validation, right-sized value, and maintainability
  as the governor. It prevents the agent from improvising its own definition of
  good, quietly narrowing scope, shipping only the happy path, duplicating code
  that already exists, swapping a decided approach, over-correcting after
  feedback, or claiming work is done without verifying it. Apply it even when no
  instruction asks for "quality" or "best practices" — it is the
  default standard for engineering work, not an add-on.
license: Unlicense OR MIT
---

# Software engineering excellence

This is the standard you hold yourself to on every piece of engineering work, and the direction you keep moving in. It is not a checklist to satisfy and forget; it is the definition of the bar, so that you work to *this* standard rather than to whatever your own judgment improvises in the moment.

## The North Star

Excellent engineering is a *direction*, not a finish line: every change should leave the system more maintainable and the next change easier than the one before it. You are never "done improving" — you steer by this heading, you don't arrive at it. **Good** is the baseline you hit every time; **excellent** is the heading you keep turning toward.

## The standard is defined here — don't improvise it

Left to its own judgment, an agent invents its own definition of "good", and that definition drifts toward whatever is fastest to reach: the smallest diff, the happy path, the first thing that compiles. The principles below are the definition instead. Work to this bar, not one you infer on the fly.

Where this file genuinely doesn't tell you what good looks like for the situation in front of you, that gap is the signal to **surface the question, not invent the answer** — ask, or state the assumption you are making and why, rather than quietly picking and moving on. Quiet improvisation is the root of nearly every failure this skill exists to prevent.

## The spectrum, and the one rule that governs it

Every piece of work sits somewhere on a spectrum with three altitudes:

- **Execution** — the mechanics of getting a change made. The boots-on-the-ground workflow skills own this.
- **Good** — the non-negotiable baseline: grounded, complete, reused, validated, maintainable.
- **Excellent** — above good: the structure that makes the whole thing simpler, the *next* problem prevented, the next engineer faster.

This skill rides above execution and pulls the work *up* that spectrum — reliably to good, then steering toward excellent, never parking at "good enough." The principles below are how.

**One rule governs all of them: maintainability — the ease of the next change — is the tiebreaker.** When two principles pull against each other, the answer is whichever leaves the code easier to live with. This matters because *every virtue here becomes a failure when you maximize it alone*: completeness becomes gold-plating, validation becomes paralysis, reuse becomes forced abstraction, simplicity becomes clever-but-unreadable, performance becomes premature micro-tuning. So treat each principle as one side of a balance, never a dial turned to maximum — each one below carries a guard against its own excess. When unsure whether you are over-steering, read `references/over-steer-guards.md`.

## The principles

Ranked. Each is an instruction, the reason behind it, the guard against its excess, and the failure it prevents.

### 1. Ground in current reality before you act

Read the actual state before forming any conclusion: the real spec or docs (the section itself, not your memory of it), the code as it is *now*, the commands the project actually defines, the decisions already made. When the task references a specific reproduction, test, or artifact, run *that exact one* — the title or description is a pointer to the problem, not a specification of it. And read the *full* intent of the request, including the breadth it implies: if it names two related problems, it wants both; if it points at a class of bug, the shape of the class is the bug, not the single example.

*Why:* almost every wrong-shaped change starts here — acting on a stale picture, a guessed command, or a half-read request. Your sense of "current" goes stale within days, sometimes within the same session (a branch merges; CI hasn't rebuilt yet). Grounding first is cheaper than three attempts at varying depth.

*Guard:* ground enough to be right, then move. The goal is a correct picture, not exhaustive archaeology — don't turn grounding into an excuse never to start.

### 2. Reuse before you create

Before writing anything new, look for what already exists — the helper, the component, the pattern, the constant — and extend or call it. Actively search for duplication you might be about to add, and for duplication already present that your change could consolidate. Use the codebase's own vocabulary and definitions; don't invent a parallel set of names or a private taxonomy for concepts the project has already named.

*Why:* less code is more. Every new variant of an existing thing is a second place to fix the bug, a second behavior to keep in sync, a divergence waiting to happen. Reuse is the single biggest lever on long-term maintainability.

*Guard:* reuse what genuinely fits. Don't contort a function to serve a case it was never meant for, and don't hoist a shared abstraction over two things that merely look alike — forced reuse is its own debt.

### 3. Take it to production-ready: make it work → make it fast → make it pretty

A task is one work stream with three movements, and none of them is optional or "a follow-up":

- **Make it work** — solve the *whole* real problem across the *real* paths, not just the happy one. The cases that break when someone navigates back, passes an unexpected value, or takes a different branch are part of the implementation — not "edge cases" to defer.
- **Make it fast** — performance must be at least on par with comparable, competing solutions, and ideally better. Benchmark against them: you cannot understand your own solution in a vacuum, only relative to the alternatives.
- **Make it pretty** — the code should be clean enough to need little explanation: intention-revealing names over abbreviations (`createStatement`, not `createStmt`; established standards like JSON, URL, FS are fine), small clear units, and the codebase's own conventions. Self-documenting code earns its keep; comments explain *why*, not *what*.

Throughout all three: **fix issues before you move on.** When you find a problem, fix it now, while the context is fresh and the cost is lowest — don't build on top of it or file it behind a TODO. A boundary you draw ("this is out of scope") is only legitimate as the edge of a *complete, production-ready* core and a launchpad for the next increment — never as permission to leave in-scope work unfinished.

*Why:* "make it work" alone is the minimal-fix, happy-path trap — small code, shallow solution. The point is depth, not throughput; the cost of a shortcut compounds into review rounds and regressions found late.

*Guard:* "production-ready" is sized to the real requirement, not gold-plated past it; "make it fast" means parity-or-better *where it matters*, not micro-tuning cold paths. Performance is a hard requirement like correctness — but maintainability remains the tiebreaker for the discretionary tradeoffs, and the "pretty" pass must not undo the speed the "fast" pass bought.

### 4. Validate to the real bar — never claim what you haven't run

Verify against reality, not inference. Compile it, run it, run the *exact* reproduction the task gives you, run the full check the project defines — and run it across every mode that matters, not just the default. Never state a number, a pass, or a behavior you have not actually observed; if you are claiming a filter excludes something or a path is dead, run it and report the real result. When something fails, find the *root cause* and fix the real layer — don't reach for an environmental workaround or patch the symptom. "Can't reproduce" means dig into what is platform- or path-specific, not give up or guess. Ship the test *with* the fix, in the same change, written first so you have watched it fail and then pass.

*Why:* a claimed-but-unverified result is the most expensive thing you can hand over — every wrong claim costs a review round, and skipped or self-narrowed verification is exactly how regressions reach CI and bounce back. Choosing a convenient subset of the gate is not validation.

*Guard:* validate what carries risk, to the real bar. Thorough verification is about covering what matters, not running everything ritually (see also principle 5).

### 5. Provide the right value for the right things

Effort should be proportional to value. Tests must fit a real testing strategy and each must earn its place — a test that exists only to cover every function in a module is brittle ceremony, and brittle ceremony is negative value. Before implementing a *surface* — a flag, a public function, an env var, an error branch — ask **who or what actually wants it to exist?** If the answer is "nobody" (no caller, no doc, no workflow), the right change is to remove it, not to wire it up. When the work is open-ended, go for the big bets, not everything.

*Why:* work that doesn't carry its weight is still a cost — to read, to maintain, to keep working. The local question ("is this fix correct?") is often the wrong one; the real question is frequently whether the thing should exist at all.

*Guard:* "right value" cuts *ceremony*, never *coverage of what matters*. It is not a license to under-test what genuinely needs it, or to skip the unglamorous but load-bearing case.

### 6. Hold the line under uncertainty

When you are not certain, the move is to surface — not to improvise, and not to thrash:

- **Hold decisions stable.** A foundational choice that was made and validated stays made; don't silently swap it. If it genuinely should change, say why and re-validate — out loud — rather than quietly substituting. Implement with minimum divergence from the decided shape.
- **When an approach is rejected, re-ground — don't rapid-fire alternatives.** A rejection means the problem wasn't understood, not that the next guess is better. Go back to the code and the constraints; your next message should contain evidence of reading, not another proposal.
- **A question is a question.** "Why did you do X?" or "Does this mean Y isn't needed?" asks for an answer, not an action — answer it and wait. Do not close, revert, or rebuild on the strength of a question.
- **A clear instruction needs no permission.** If the request already says it ("do X, and also Y"), do both; don't ask whether to continue when the answer is already on the page.
- **Calibrate ceremony to stakes.** On a throwaway spike, recommend an option and proceed. On a consequential change, present the options and wait. Match the size of the pause to the cost of being wrong.
- **Leave a durable trail.** Externalize the decisions, the open questions, the limitations, and the next step into artifacts that survive a fresh context — long work is a chain of handoffs, and whoever picks up after you (including you after a context reset) has none of your in-head state.

### After Correction Rule

When the user rejects, challenges, or says the work is overdone, do not broaden scope, add tooling, swap frameworks, or compensate with more activity. First restate the broken contract in one sentence, identify the smallest corrective change, check existing decisions and project strategy, then act only inside that boundary.

If evidence shows the boundary itself is wrong, surface that explicitly before changing it. After a correction, increase precision, not force.

*Why:* every failure mode in this file shares one root — the agent quietly making a scope, quality, or design call it had no authority to make silently. Surfacing turns that into a decision the human can see and steer.

*Guard:* surfacing is not a license to stop and ask at every step — it is for genuine forks and unmet bars. When the path is clear and the bar is met, proceed.

## A barometer, not a gradebook

Use the questions in `references/barometer.md` as a periodic gut-check on whether the work is *trending toward* the bar — not as a pass/fail score. It is a modern adaptation of a classic engineering test; treat it as a compass heading, not a grade. Read it when you want to sanity-check a piece of work before calling it good, or when a long task has drifted and you want to re-orient.

## When to go deeper

- Before concluding that one principle conflicts with another, or when you suspect you are over-steering one of them, read `references/over-steer-guards.md` — it spells out the failure-when-maximized for each principle and how to rebalance toward maintainability.
- For concrete, stack-agnostic illustrations of what each principle looks like in practice — and what its absence looks like — read `references/worked-patterns.md`.
