# The engineering barometer

A periodic gut-check, adapted for agentic coding from a classic engineering self-test. These are **direction questions, not a score**: a "no" is not a failing grade, it is a heading to correct. Run through them when you are about to call a piece of work good, or when a long task feels like it has drifted and you want to re-orient.

Read each as "for the work I am doing right now…"

## Grounding

- **1.** Did I read the actual current state — the real spec/docs, the code as it is now, the decisions already made — rather than work from memory or assumption?
- **2.** When the task named a specific repro, test, or artifact, did I run *that exact one* before forming a theory?
- **3.** Did I treat docs, comments, and issue text as *leads to verify against the source* rather than as proof — and record what the evidence says before I recommended anything?

## Scope and intent

- **4.** Did I solve the *whole* problem the request implies — its full breadth and its real paths — not just the first or happiest slice?
- **5.** Did I read the full intent, including what was asked implicitly, and do what was clearly instructed without asking redundant permission?

## Reuse

- **6.** Did I look for what already exists and reuse or consolidate it, instead of adding a new variant of something the codebase already has?
- **7.** Am I using the project's own names and conventions, rather than a parallel vocabulary of my own?

## Quality and performance

- **8.** Is the result production-ready — correct on every real path, at least on par with comparable solutions in performance, and clean enough to need little explanation?
- **9.** Did I fix the problems I found as I went, rather than leaving them behind a TODO to build on top of?
- **10.** Did I fix at the *right layer* and leave the structure sounder — rather than plaster over a symptom and let the bar drift down one tolerated compromise at a time?
- **11.** For new or sizable work, did I start from a thin end-to-end slice that actually runs — deployed or run live where there is a target — and grow it in runnable increments, rather than building layers in isolation and integrating late?

## Validation

- **12.** Have I actually run everything I am claiming — every mode that matters — so that nothing in my report is asserted but unverified?
- **13.** When something failed, did I fix the root cause rather than a symptom or an environmental workaround — and does the test ship in the same change as the fix?

## Judgment

- **14.** Where I was uncertain, did I surface the question (or state my assumption) instead of quietly improvising an answer — and where I was certain and authorized, did I just proceed?
- **15.** Did I leave a durable trail — decisions, open questions, limitations, next step — that someone with none of my in-head context could pick up?

If several of these are "no," the work is not at the bar yet — and the fix is usually one of the six principles, not a new idea. If they are "yes," the question becomes the North Star one: *is there a structure here that would make the whole thing simpler and the next change easier?* That is the step from good toward excellent.
