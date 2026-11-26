# Host Environment

*Inject script-visible time, time-zone, and random providers without changing infrastructure clocks.*

## Executive Summary

- A host environment supplies the JavaScript-visible wall time, monotonic time, default time zone, and randomness.
- `GocciaRunner --host-environment=<module>` takes the providers as callable named JavaScript exports; Pascal embedders set the same module path, or the deterministic profile, on `TGocciaEngine`.
- Child engines share the clock and receive derived random stream identifiers, so realms remain reproducible without replaying the parent stream.
- A **clock override** layers a mocked epoch and/or monotonic time over the configured providers, which is how fake timers reach `Date`, `Temporal.Now`, and `performance` at once.
- Timeouts, profiling, benchmarks, and other infrastructure continue to use the real clocks in `TimingUtils`.

## JavaScript Provider Modules

Pass a module with four callable named exports:

```javascript
export const epochNanoseconds = () => 1700000000000000000n;
export const monotonicNanoseconds = () => 0n;
export const timeZoneIdentifier = () => "UTC";
export const random = (streamId) => streamId === 0n ? 0.25 : 0.75;
```

Then run a script with the provider:

```bash
./build/GocciaRunner app.js \
  --host-environment=./examples/custom-host-environment.js
```

The epoch and monotonic callbacks must return either an `Int64`-range `BigInt` or a safe integer `Number`. The time-zone callback must return a supported identifier. The random callback must return a finite `Number` in the interval `[0, 1)`.

`streamId` is an opaque `BigInt`. The initial engine receives `0n`; child engines receive derived identifiers. Providers that maintain random state should key it by this value, as shown in [the complete example](../examples/custom-host-environment.js).

The provider module is evaluated after file loading is available but before the loader runtime profile extensions attach. Its exported callbacks remain rooted for the engine lifetime. A provider callback must not recursively read the source it supplies—for example, `random` must not call `Math.random()`, and a clock callback must not call `Date.now()`.

`--host-environment` and `--deterministic` are mutually exclusive. The equivalent configuration-file key is `"host-environment"`; a module a config names resolves from that config file and loads as a guest module under the script's capabilities rather than as a host module (see [Permissions](permissions.md#config-files)).

## Pascal Embedders

Set the providers on the engine before its first run or `PrepareHost`:

```pascal
Engine := TGocciaEngine.Create;
Engine.HostEnvironment := 'provider.js';   { evaluated before the entry, as --host-environment }
Result := Engine.Run('app.js', Source);
```

`HostEnvironment` names the provider module above; its time zone is read once, before the first run's entry. `Deterministic := True` selects the [deterministic profile](#deterministic-profile) instead; the two are exclusive. `RandomRoot` gives a deterministic engine realm 0's `Math.random` stream, an `IGocciaHostRandom` such as another engine's `ForkRandom`, which is how a sandbox's nested run keeps a stream of its own. The engine takes no clock object: wall and monotonic time come from the system clocks, the provider module, or the profile.

## Deterministic Profile

The deterministic profile, which `--deterministic`, `"deterministic": true` and `TGocciaEngine.Deterministic` select, supplies epoch and monotonic time `0`, `UTC`, and portable seeded SplitMix64 randomness. `TGocciaHostEnvironment.DeterministicClock` returns the profile's clock, for host state that has to keep time with it.

The sandbox virtual filesystem is such state. In `GocciaRunner`'s sandbox mode, `--deterministic` also stamps the filesystem with the profile's clock: every access, modification, change and birth time is the epoch, for the copied inputs, the files the run writes, and the files of isolated nested runs. `fs.statSync(path).mtimeMs` reads `0` as `Date.now()` does, and two runs print the same times and write the same diff. A JSON diff still lists every content change, but its `metadataChanges` is empty, because no timestamp moves. Nothing is ever newer than the module the loader cached, so a module rewritten during the run is not reloaded by a later import, and a failed import is not retried after its file is rewritten. An embedder keeps its own sandbox in step by passing a clock to the context ([Embedding — Sandbox filesystem](embedding.md#sandbox-filesystem)). [ADR 0151](adr/0151-deterministic-sandbox-runs-stamp-files-with-the-deterministic-clock.md) records the decision.

## Mocked Clocks

The test runner's fake timers (`prelude/timers.js`) layer a mocked clock over the configured providers rather than replacing them: a mocked epoch time, a mocked monotonic time, or both. This is the layer the [virtual timer queue](adr/0113-deterministic-virtual-timer-queue.md) installs, so `Date`, `Temporal.Now`, and `performance` all report the same simulated instant.

The two halves are independent. Freezing the date alone — `vi.setSystemTime()` outside `vi.useFakeTimers()` — leaves monotonic time real, so `performance.now()` keeps measuring elapsed wall time. Under fake timers both are mocked, and `performance.now()` reports elapsed **virtual** time from the moment the clock was installed, which a `setSystemTime` jump does not move.

## Default Compatibility

Without an explicit provider, the system host environment preserves each API's established default behavior. Temporal and Date observe the system time zone; `Intl.DateTimeFormat` retains GocciaScript's historical `UTC` default. An explicitly configured host time zone overrides both surfaces.
