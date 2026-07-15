# Host Environment

*Inject script-visible time, time-zone, and random providers without changing infrastructure clocks.*

## Executive Summary

- Every engine owns a `TGocciaHostEnvironment` that supplies JavaScript-visible wall time, monotonic time, default time zone, and randomness.
- Pascal embedders inject implementations of `IGocciaHostClock` and `IGocciaHostRandom` before attaching runtime extensions or executing source.
- `GocciaScriptLoader --host-environment=<module>` accepts the same providers as callable named JavaScript exports.
- Child engines share the clock and receive derived random stream identifiers, so realms remain reproducible without replaying the parent stream.
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
./build/GocciaScriptLoader app.js \
  --host-environment=./examples/custom-host-environment.js
```

The epoch and monotonic callbacks must return either an `Int64`-range `BigInt` or a safe integer `Number`. The time-zone callback must return a supported identifier. The random callback must return a finite `Number` in the interval `[0, 1)`.

`streamId` is an opaque `BigInt`. The initial engine receives `0n`; child engines receive derived identifiers. Providers that maintain random state should key it by this value, as shown in [the complete example](../examples/custom-host-environment.js).

The provider module is evaluated after file loading is available but before the loader runtime profile extensions attach. Its exported callbacks remain rooted for the engine lifetime. A provider callback must not recursively read the source it supplies—for example, `random` must not call `Math.random()`, and a clock callback must not call `Date.now()`.

`--host-environment` and `--deterministic` are mutually exclusive. The equivalent configuration-file key is `"host-environment"`.

## Pascal Embedders

Implement the two narrow provider interfaces and configure the engine before attaching runtime extensions or executing JavaScript:

```pascal
type
  TApplicationClock = class(TInterfacedObject, IGocciaHostClock)
  public
    function EpochNanoseconds: Int64;
    function MonotonicNanoseconds: Int64;
    function TimeZoneIdentifier: string;
  end;

  TApplicationRandom = class(TInterfacedObject, IGocciaHostRandom)
  private
    FState: QWord;
  public
    constructor Create(const ASeed: QWord);
    function NextDouble: Double;
    function Fork(const AStreamId: QWord): IGocciaHostRandom;
  end;

Engine := TGocciaEngine.Create('app.js', Source, Executor);
Engine.HostEnvironment.Configure(
  TApplicationClock.Create,
  TApplicationRandom.Create(Seed));
AttachRuntime(Engine);
Engine.Execute;
```

`Fork` must return a distinct random stream derived from both the provider's current stream identity and `AStreamId`. The engine shares the clock with child engines because wall and monotonic time belong to the host, while random state is isolated per child.

For a fixed built-in profile, call `Engine.HostEnvironment.UseDeterministicProfile` instead of implementing providers. It supplies epoch and monotonic time `0`, `UTC`, and portable seeded SplitMix64 randomness.

## Default Compatibility

Without an explicit provider, the system host environment preserves each API's established default behavior. Temporal and Date observe the system time zone; `Intl.DateTimeFormat` retains GocciaScript's historical `UTC` default. An explicitly configured host time zone overrides both surfaces.
