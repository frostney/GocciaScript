{ Use-after-free regression gate for the TGocciaSharedPrototype units (#892
  follow-up).

  ~30 value units (Map, Set, WeakMap/WeakSet/WeakRef, FinalizationRegistry,
  Promise, ArrayBuffer, all Intl*, all Temporal*, ...) build their prototype via
  TGocciaSharedPrototype.Create(Self) — where Self is a per-realm host instance,
  unpinned when the realm is torn down — while caching their member definitions
  in a process-wide threadvar guarded by `if Length(FPrototypeMembers) = 0`.
  The cached member array's `procedure of object` callbacks are therefore bound
  to the FIRST realm's host on a thread, and are reused (bound to that same host)
  by every later realm on the thread. After the first realm is destroyed and the
  GC collects, that host is freed.

  This test drives exactly that path: it runs many independent engines (each its
  own realm) on one thread, forcing a full GC collection between them so the
  first realm's host is actually reclaimed, then exercises the shared-prototype
  types in the later realms. Each script self-verifies its results and throws on
  any discrepancy, so a use-after-free that corrupted dispatch surfaces either as
  a thrown error (caught below) or a hard crash (fails the suite). Passing
  confirms the bound callbacks never dereference the freed host (they re-derive
  their receiver from the JS `this`), which is why the cross-realm cache is safe
  despite the realm-owned host. See docs/adr/0084 (and docs/adr/0083 for the
  related #892 migration). }

program Goccia.SharedPrototypeRealmReuse.Test;

{$I Goccia.inc}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  SysUtils,

  Goccia.Engine,
  Goccia.GarbageCollector,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives,
  TestingPascalLibrary,

  Goccia.TestSetup;

const
  // Exercises representative units from each hazard group: collections + weak
  // refs + promise, Intl, and Temporal + ArrayBuffer. Every operation checks its
  // own result and throws on mismatch, so reusing a cached member bound to a
  // freed host would surface here (wrong result -> throw, or a crash).
  SELF_VERIFYING_SCRIPT =
    'const m = new Map(); m.set("a", 1); m.set("b", 2);' + sLineBreak +
    'if (m.get("a") !== 1 || m.get("b") !== 2 || m.size !== 2 || !m.has("a")) throw new Error("Map");' + sLineBreak +
    'let seen = 0; m.forEach((v) => { seen += v; }); if (seen !== 3) throw new Error("Map.forEach");' + sLineBreak +
    'const s = new Set([1, 2, 3]); if (!s.has(2) || s.size !== 3) throw new Error("Set");' + sLineBreak +
    'const u = s.union(new Set([3, 4])); if (u.size !== 4 || !u.has(4)) throw new Error("Set.union");' + sLineBreak +
    'const wm = new WeakMap(); const k = {}; wm.set(k, 9); if (wm.get(k) !== 9 || !wm.has(k)) throw new Error("WeakMap");' + sLineBreak +
    'const ws = new WeakSet(); const k2 = {}; ws.add(k2); if (!ws.has(k2)) throw new Error("WeakSet");' + sLineBreak +
    'const target = { tag: 1 }; const wr = new WeakRef(target); if (wr.deref() !== target) throw new Error("WeakRef");' + sLineBreak +
    'const p = Promise.resolve(1); const p2 = p.then((x) => x); if (typeof p2.then !== "function") throw new Error("Promise");' + sLineBreak +
    'const ab = new ArrayBuffer(8); if (ab.byteLength !== 8) throw new Error("ArrayBuffer");' + sLineBreak +
    'const ab2 = ab.slice(0, 4); if (ab2.byteLength !== 4) throw new Error("ArrayBuffer.slice");' + sLineBreak +
    'const nf = new Intl.NumberFormat("en-US"); if (typeof nf.format(1234.5) !== "string") throw new Error("Intl.NumberFormat");' + sLineBreak +
    'const ro = nf.resolvedOptions(); if (typeof ro.locale !== "string") throw new Error("Intl.resolvedOptions");' + sLineBreak +
    'const dur = Temporal.Duration.from({ hours: 2, minutes: 30 });' + sLineBreak +
    'if (dur.hours !== 2 || dur.minutes !== 30) throw new Error("Temporal.Duration");' + sLineBreak +
    'const inst = Temporal.Instant.fromEpochMilliseconds(1000);' + sLineBreak +
    'if (inst.epochMilliseconds !== 1000) throw new Error("Temporal.Instant");';

type
  TRealmReuseTests = class(TTestSuite)
  public
    procedure SetupTests; override;

    procedure TestSharedPrototypeSurvivesRealmTeardownAndCollect;
  end;

procedure TRealmReuseTests.SetupTests;
begin
  Test('shared-prototype types stay correct across realm teardown + GC collect on one thread',
    TestSharedPrototypeSurvivesRealmTeardownAndCollect);
end;

procedure TRealmReuseTests.TestSharedPrototypeSurvivesRealmTeardownAndCollect;
const
  CYCLES = 12;
var
  I, J: Integer;
  Stomp: array of TGocciaObjectValue;
begin
  // Cycle 0 builds and caches each unit's FPrototypeMembers, bound to that
  // realm's host instances, then tears the realm down (RunScript frees the
  // engine), unpinning those hosts. The collect below reclaims them. Cycles 1+
  // hit the `Length(FPrototypeMembers) <> 0` fast path, so they re-apply the
  // cached members — bound to cycle 0's now-freed hosts — to their own fresh
  // prototypes, and then dispatch against them. A use-after-free here throws or
  // crashes; correct results prove the freed host is never dereferenced.
  for I := 0 to CYCLES - 1 do
  begin
    TGocciaEngine.RunScript(SELF_VERIFYING_SCRIPT, '<realm-reuse-' + IntToStr(I) + '>');

    // Reclaim the just-torn-down realm's unpinned host instances...
    TGarbageCollector.Instance.Collect;

    // ...then aggressively reuse the freed blocks, so any later dereference of a
    // freed host reads stomped memory rather than coincidentally-intact data.
    SetLength(Stomp, 4096);
    for J := 0 to High(Stomp) do
      Stomp[J] := TGocciaObjectValue.Create;
    for J := 0 to High(Stomp) do
      Stomp[J] := nil;
    SetLength(Stomp, 0);
    TGarbageCollector.Instance.Collect;
  end;

  // Reaching here without a thrown error or crash across all cycles is the pass
  // condition.
  Expect<Boolean>(True).ToBe(True);
end;

begin
  TGarbageCollector.Initialize;
  PinPrimitiveSingletons;
  try
    TestRunnerProgram.AddSuite(TRealmReuseTests.Create('SharedPrototypeRealmReuse'));
    TestRunnerProgram.Run;
  finally
    TGarbageCollector.Shutdown;
  end;

  ExitCode := TestResultToExitCode;
end.
