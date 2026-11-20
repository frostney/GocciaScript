# FreePascal code style

- Classes use `T`, interfaces `I`, exceptions `E`, and private fields `F`.
- Parameters use the `A` prefix: `AEntity`, `ABuilder`.
- Only loop counters may be single letters (`for I := ...`, `for X := ...`).
  Every other variable, parameter and field takes a full-word name: `Builder`,
  not `B`; `Entity`, not `E`.
- Use PascalCase, full words, and no numeric suffixes. Do not abbreviate:
  `TVector3`, `TMatrix4`, `Position`, `Index` and `Context`, not `TVec3`,
  `TMat4`, `Pos`, `Idx` or `Ctx`. Preserve standard initialisms in their
  conventional uppercase form everywhere, including inside identifiers and unit
  names: `HTTPClient`, `IHTTPTransport`, `TGCRoot` and `Nightfare.ECS.World`,
  not `HttpClient`, `IHttpTransport`, `TGcRoot` or `Nightfare.Ecs.World`.
  Follow an external API or explicit project convention when compatibility
  requires another spelling.
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
