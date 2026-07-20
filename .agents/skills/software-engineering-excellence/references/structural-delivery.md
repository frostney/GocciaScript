# Structural delivery

Read this for substantial architecture, greenfield work, or a defect whose
symptom may sit above the real fault.

## Resist the quick fix

The smallest correct change is not necessarily the change with the fewest lines.
A patch that hides a symptom while leaving the underlying boundary wrong makes
the next change harder. Fix at the layer that owns the behavior, while keeping
the solution inside the requested scope.

Depth and breadth are different. Correcting a load-bearing boundary is depth;
adding unrelated features, frameworks, or future-proofing is breadth. Prefer the
smallest change that fully solves the real problem at the right layer.

## Prove the whole path early

For new or genuinely multi-layer systems, start with the thinnest real slice that
runs from entry to result. Include every boundary the finished system actually
needs, but only one minimal path through it. Deploy it when deployment is part of
the real product; otherwise invoke the real artifact rather than a mock.

Then deepen the system in runnable increments. Each increment should be correct
at its current scope, and the end-to-end path should keep working as depth is
added.

## Calibrate to stakes

A walking skeleton is not mandatory ceremony for a small fix, isolated library
change, or throwaway spike. It is useful when early integration evidence can
invalidate architectural assumptions. The objective is fast contact with
reality, not infrastructure for its own sake.
