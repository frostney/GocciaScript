# The engineering barometer

A periodic gut-check, adapted for agentic coding from a classic engineering self-test. These are **direction questions, not a score**: a "no" is not a failing grade, it is a heading to correct. Run through them when you are about to call a piece of work good, or when a long task feels like it has drifted and you want to re-orient.

Read each as "for the work I am doing right now…"

## Grounding

- **1.** Did I read the actual current state, including the real spec/docs, current code, and recorded decisions, rather than work from memory or assumption?
- **2.** When the task named a specific repro, test, or artifact, did I run *that exact one* before forming a theory?
- **3.** Did I treat docs, comments, and issue text as *leads to verify against the source* rather than as proof, and record what the evidence says before I recommended anything?

## Scope and intent

- **4.** Did I solve the *whole* problem the request implies, across its full breadth and real paths, not just the first or happiest slice?
- **5.** Did I read the full intent, including what was asked implicitly, and do what was clearly instructed without asking redundant permission?

## Reuse

- **6.** Did I look for what already exists and reuse or consolidate it, instead of adding a new variant of something the codebase already has?
- **7.** Am I using the project's own names and conventions, rather than a parallel vocabulary of my own?

## Quality

- **8.** Is the result production-ready, correct on every real path, and clean enough to need little explanation?
- **9.** Did I fix problems that invalidate the authorized outcome and record unrelated findings without expanding scope?
- **10.** Did I fix at the *right layer* and leave the structure sounder rather than plaster over a symptom and let the bar drift down one tolerated compromise at a time?
- **11.** For new or sizable work, did I start from a thin end-to-end slice that actually runs, deployed or live where a target exists, and grow it in runnable increments rather than integrating isolated layers late?

## Performance and feedback

- **12.** Did I consider product runtime and the time to reliable feedback while choosing the design, rather than leave speed until the end?
- **13.** Where the change affects a hot path, command, hook, local environment, CI, concurrency, external work, resource use, or a performance claim, did I measure a representative baseline and changed result?
- **14.** Are the frequent local checks fast and high-signal, with heavier work placed where it protects correctness without needlessly delaying every iteration?
- **15.** If measured speed required more complexity, is that complexity contained, covered, documented, and worth its maintenance cost at representative scale?

## Validation

- **16.** Have I run every mode that matters so that nothing in my report is asserted but unverified?
- **17.** When something failed, did I fix the root cause rather than a symptom or environmental workaround, and does the test ship in the same change as the fix?

## Judgment

- **18.** Where I was uncertain, did I surface the question or state my assumption instead of quietly improvising, and where I was certain and authorized, did I proceed?
- **19.** Did I leave a durable trail of decisions, open questions, limitations, and next steps that someone without my internal context could pick up?

Apply these questions to the authorized outcome. Correct material gaps within
that scope, then stop when its completion conditions hold. Record adjacent
improvements as follow-up candidates rather than new completion requirements.
Assessment-only work remains non-mutating.
