# FreePascal code style

- Classes use `T`, interfaces `I`, exceptions `E`, and private fields `F`.
- Parameters of two or more letters use `A`; single-letter conventional names
  remain unchanged.
- Use PascalCase, full words, no numeric suffixes; retain standard acronyms.
- Model behavioral domain concepts with classes and explicit responsibilities.
  Use records or procedures when there is no behavior to attach.
- Use `const` parameters unless mutated; use `var` or `out` deliberately.
- Mark genuinely small non-recursive routines `inline` where useful.
- Replace magic literals with named constants; expose them in `interface` only
  when both sections need them.
- Put one unit per `uses` line, alphabetized within system, third-party,
  namespaced-project, and relative-path groups.
- Keep public API minimal; move heavy or cycle-causing dependencies to
  `implementation uses`.
- Give reused generic specializations one owner-defined alias to avoid distinct
  cross-unit VMT types.
- Use `published` plus `TypInfo` only for members that need runtime
  introspection.
