# Review worker lanes

When the user supplies `subagents`, the coordinating agent still owns the
comparison boundary, claim, finding scope, active and skipped review axes,
validation, verdict, and report.

1. Publish a bounded review-axis-to-lane map before delegation. For a fresh
   review, give each active review axis exactly one lane across the complete
   finding scope: de-duplication, claim and specification, engineering quality,
   and discoverability when active. Keep axes separate so one cannot mask
   another. Queue excess lanes when platform capacity is temporarily full.
2. For targeted revalidation alone, map selected findings to bounded finding
   lanes instead. Group only tightly coupled findings. Exact-file and
   prior-findings inputs retain their normal intersection rules.
3. Give each worker its lane ID, assigned review axis or finding IDs, exact
   scope, claim or source finding, comparison boundary or baseline, relevant
   project instructions, and known evidence. A worker may inspect and run the
   safe probes allowed by this skill, but it must not edit, create persistent or
   external side effects, delegate further, assign final finding IDs or
   severities, or issue a verdict.
4. Require each worker to return its lane ID, assigned review axis or findings,
   bounded scope, inspected supporting context, exact probes and observed
   results, every evidence-supported candidate with evidence, impact, smallest
   remedy, and any uncertainty or limitation, verified claims, limitations, and
   `complete` or `incomplete` status. Workers do not apply a severity or
   reporting threshold; the coordinator owns candidate filtering.
5. Validate every candidate against the current checkout, apply the
   de-duplication model below, reconcile conflicts across lanes, then assign
   final IDs, severities, categories, and verdict. Do not repeat a completed
   lane wholesale.
6. If sub-agents are unsupported, unavailable after any applicable bounded
   retry, or leave a lane incomplete, complete that lane directly. Report the
   affected lane and reason as a single-agent fallback. Temporary capacity
   exhaustion queues work rather than triggering immediate fallback.

Before starting a worker, identify its actual model from host metadata. Deliver
the applicable role, scope, authority, completion condition, and required skill
contents or reachable reference paths; naming a skill is not proof of delivery.
Record the resources the worker actually loaded and any missing capability in
its result. An isolated worker must not assume parent conversation or loaded
skills are inherited. Keep model-specific settings at the host boundary and
apply them only to the actual worker when supported by evidence.
