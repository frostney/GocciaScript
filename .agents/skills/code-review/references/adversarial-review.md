# Adversarial review

Read this only when a fresh review touches authentication, authorization,
payments, secrets, destructive or data-loss behavior, or tenant isolation. This
is bounded engineering-quality coverage, not a separate review axis.

Build an attack-oriented map before judging candidates:

- enumerate every changed mutating or sensitive surface and the gate that must
  protect it;
- trace attacker-controlled inputs through parsing, normalization,
  authorization, persistence, side effects, and user-visible errors;
- test missing, reordered, duplicated, replayed, and partially completed
  operations where the interface makes them possible;
- look for fail-open inversions, checks performed after effects, confused-deputy
  paths, cross-tenant identifiers, secret disclosure, and response differences
  that create an oracle; and
- exercise the most consequential safe bypass or failure path through the real
  interface, or mark the conclusion static-only when that cannot be done safely.

Return evidence-supported candidates to the coordinator without assigning
severity or filtering by likely reportability. A generic security preference is
not a finding; tie every candidate to the changed path, observed evidence,
concrete impact, and smallest remedy.
