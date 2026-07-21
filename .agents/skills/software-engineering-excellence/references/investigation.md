# Investigation discipline

Read this when you are running a substantial investigation — diagnosing a defect, evaluating a design, comparing this project against other implementations, or forming a recommendation from evidence. One rule sits under all of it: **evidence first, recommendation second — and evidence is what you read in the source and watched when you ran it, not the story told about it.**

## Evidence is what you read and ran, not what you were told

Documentation, READMEs, comments, prior notes, and issue text are *leads*. They tell you where to look; they are not the thing itself. Follow every lead down to the source and the running behavior before you rely on it.

Challenge the local evidence with the same suspicion you would apply to a stranger's. The repo's own docs, comments, tests, and existing implementation choices may be stale, incomplete, or simply wrong — written for a version that has since changed, or aspirational and never true. Verify each against the current source and the spec before treating it as authoritative. A comment that says what the function *should* do is a hypothesis; the code is the fact.

## Compare other projects by their source, not their positioning

When you look at how another project, library, or engine solves a problem, its public positioning — marketing, README claims, blog headlines, changelog summaries — tells you what it wants you to believe, not what it does. Go to the source paths and read the mechanisms:

- **parser gates and feature flags** — what is accepted, and under what condition;
- **build options and compile-time switches** — what actually ships versus what is possible;
- **runtime hooks** — where behavior can be swapped or intercepted;
- **object-model / data-model changes** — the shape the design truly commits to;
- **tests** — the ground truth for intended behavior, including the edges;
- **default configuration** — what a normal user actually gets.

What is gated off by default is not the same as what is "supported." What a flag *enables* is not the same as what ships on. The test suite and the defaults are the behavior; the docs are the claim. When the two disagree, the source wins and the disagreement is itself a finding.

## Separate evidence from recommendation

Do the recording pass before the recommending pass, and keep them visibly separate. First record — plainly, without editorializing:

- what the **spec** says (the section itself, cited);
- what **other implementations** actually do (from their source, per above);
- what **this codebase** currently does (the real code path, not its comments);
- what **mechanisms** a change would actually require.

Only then recommend a policy or an architecture — and make the recommendation trace back to that record. A recommendation that arrives before the evidence is a preference dressed as a finding, and it will quietly bend the evidence-gathering to fit it.

## Say when a recommendation touches project vision

A change can be entirely feasible and still be wrong *for this project* — counter to its goals, its security model, its portability goals, or its compatibility philosophy. Technically possible is not the same as aligned. When a recommendation touches any of those, mark it explicitly as a vision-level decision for the human to make, rather than folding it in as a technical detail.

## After a correction, rebuild from evidence

A correction is not a cue to flip the conclusion or soften the tone — reversing a guess is still a guess, and matching the correction's apparent mood is not the same as being right. Return to the source, name the specific assumption that turned out to be wrong, and rebuild the answer from verified facts. Your next message should contain evidence of fresh reading, not a new proposal aimed in the opposite direction.

## The thread through all of it

Investigation goes wrong the moment the story replaces the source: the README stands in for the code, the positioning stands in for the implementation, the recommendation arrives before the evidence, or the correction flips the answer without re-reading. Consult reality first — the source, the run, the spec — record it plainly, and only then say what should be done.

## Intermittent failures and the debugging bar

An intermittent defect is fixed only when the mechanism is pinned by observed
evidence — a reproduction, a discriminating instrument, or the raw failure
artifact. Guards, retries, timeout bumps, and quarantines are instrumentation
or mitigation, never the fix; they may ship, but only alongside a tracked
record of the still-unpinned cause.

Debug from the complete raw artifact — the full log, the actual run — never
from a summary, a paraphrase, or a truncated slice; each hides exactly the
detail that discriminates between hypotheses. And a negative stress result
rules a mechanism out only when the stressor provably aligns the suspect
window; an unaligned non-reproduction is absence of evidence, not evidence of
absence.

After three disproven hypotheses or failed fix attempts on the same defect,
stop and bring in an independent second opinion, handing over an evidence
brief: the symptom, the raw artifacts, and each hypothesis with how it was
killed. Three quick kills in an hour is healthy progress; the trigger exists
for the third dead end, when the next guess would otherwise come from an
emptying well.
