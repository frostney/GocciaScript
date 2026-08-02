# 0103 - Layer untrusted execution boundaries

## Context

The CLI runtime, website playground, bytecode loader, network transport,
sandbox runner, and native built-ins each enforced part of the resource and
capability policy. Untrusted source could still cross gaps between those
layers: ambient filesystem module loading remained attached to public
execution, URL validation and connection parsing differed, nested execution
could replace outer budgets, deserialized bytecode reached unchecked VM
operands, and native recursion or backing storage bypassed execution limits.

## Decision

Treat every transition from untrusted data into a host capability or
long-running native operation as an explicit boundary:

- Runtime attachment takes an explicit host-filesystem-loading capability.
  Public website execution requires the probed `--no-host-filesystem` feature
  on both loader and test-runner binaries and hides older unsafe engines.
- Fetch allowlisting and transport use one canonical authority parser.
  The allowlist is rechecked after every redirect, request syntax rejects
  controls, socket work observes the remaining execution deadline, and
  response headers and bodies are capped.
- Bytecode input is size- and depth-bounded, structurally verified before
  execution, and still uses checked runtime accessors.
- Nested sandbox execution pushes and pops deadline/instruction scopes,
  inherits remaining filesystem quotas, and has a runner-level depth limit.
- Native parsers, serializers, cloning, string growth, typed-array work, and
  binary backing stores share depth, polling, and memory-accounting controls.

Ordinary trusted CLI and embedding workflows retain filesystem module loading
unless they opt out. The dedicated sandbox runner remains bounded by default
and exposes explicit quota overrides for trusted hosts.

## Consequences

Older vendored engines without the filesystem-disable capability are no longer
available in the public playground. Malformed `.gbc` artifacts fail at load
time. Deep or allocation-heavy native operations may now throw `RangeError`,
syntax errors, timeout errors, or memory-limit errors instead of exhausting the
host process. These limits are implementation safety bounds, not new
ECMAScript semantics.
