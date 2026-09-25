# Engineering smell prompts

Read this only when structural evidence suggests a design problem but its impact
or smallest remedy is not yet clear. These are prompts for investigation, never
hard rules or automatic findings. Repository standards and observed behavior
override them.

Probe whether the changed path exhibits:

- names that conceal domain meaning;
- duplicated or competing representations;
- behavior living with the wrong data or crossing too many object boundaries;
- values that repeatedly travel together without a named domain concept;
- primitive values carrying validation or state-machine meaning;
- repeated branching over the same kind or state;
- one change forcing edits across unrelated owners;
- one owner changing for unrelated reasons;
- abstractions, inheritance, indirection, or extension points without a current
  caller;
- long delegation chains or wrappers that only forward behavior; or
- an abstraction whose advertised contract is rejected by its implementations.

Continue only when the prompt leads to concrete evidence in the bounded change.
Report the observed failure or maintainability cost and the smallest remedy,
not the smell name by itself.
