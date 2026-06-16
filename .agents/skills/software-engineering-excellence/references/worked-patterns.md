# Worked patterns

Concrete, stack-agnostic illustrations of each principle — what it looks like when it is present, and the shape of its absence. None of these is tied to a particular language, framework, or project; they are the recurring patterns that separate work at the bar from work that merely compiles.

## 1. Ground in current reality

**Present:** A task points at a specific failing test and includes the command to reproduce it. The engineer runs *that command* first, watches the actual failure, and only then forms a theory — discovering the real fault is in a different code path than the title suggested.
**Absent:** The engineer reconstructs the scenario from the title, hits a *different* failure, and treats it as confirmation — fixing a problem the task was not about while the real one remains. Or: the engineer reports a metric from a previous run as current, not noticing a change has since landed and the picture has moved.

## 2. Reuse before you create

**Present:** Before adding a tree-walking helper, the engineer searches and finds the same walk implemented thirteen times across the test suite, confirms each copy is behaviorally identical, and consolidates them into one shared helper — deleting twelve copies and the divergence risk with them.
**Absent:** A new near-copy of an existing function is added because it was faster than finding the original — creating a second place every future bug must be fixed, and a second behavior that will silently drift from the first.

## 3. Production-ready (work → fast → pretty)

**Present:** A new low-level library is built to the full protocol, then benchmarked head-to-head against the established reference implementations in other languages (not in a vacuum), then validated for byte-level interoperability against those references — and only called done once it matches them on correctness *and* is competitive on speed.
**Absent:** "It passes the basic case" is declared done, with the uncommon-but-real paths left unhandled and performance never measured against anything — so "works" means "works on the one input I tried," and the slow path surfaces in production.

## 4. Validate to the real bar

**Present:** Performance has regressed over several weeks. Rather than guess at a cause, the engineer rebuilds at successive historical points, bisects to the exact change that introduced the cost, and root-causes it to a specific per-operation overhead — then verifies the fix across *both* execution modes the system supports, not just the default.
**Absent:** A platform-specific CI failure that cannot be reproduced locally is "fixed" by lowering an optimization level or skipping the test — an environmental workaround that buries the real defect instead of tracing the platform-specific code path that actually differs.

## 5. Right value for the right things

**Present:** A review flags that a command-line flag does nothing. Instead of wiring it up, the engineer asks who actually wants it — finds no caller, no doc, and no workflow that uses it — and removes it. The right diff was deletion, not implementation.
**Absent:** The flag is dutifully made to work "as advertised," adding a surface, a code path, and a maintenance burden for a capability nobody needs — answering "is the fix correct?" when the real question was "should this exist at all?"

## 6. Hold the line under uncertainty

**Present:** A proposed fix is rejected. The engineer's next move is not another proposal but a return to the code — reading the constraints properly, mapping the real option space — and only then bringing back a grounded recommendation. Separately, when asked "does this change mean the other one isn't needed?", the engineer *answers the question* and waits, rather than taking it as an instruction to start reverting things.
**Absent:** A rejection triggers a burst of four alternatives in four minutes, each one only dodging the previous objection without understanding the problem, each worse than the last. Or: a clarifying question is misread as a command, and the engineer closes work and rebuilds on the strength of something the human only *asked about*.

## The thread through all of them

In every "absent" case above, the engineer made a silent call it was not entitled to make: that the title was the spec, that a near-copy was fine, that "works once" was done, that a workaround was a fix, that a surface should exist, that a question was a command. In every "present" case, reality was consulted first and uncertainty was surfaced rather than improvised. That is the whole skill in one line: **work to the defined bar, and when the bar is not defined for your situation, surface the question instead of inventing the answer.**
