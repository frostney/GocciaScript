{ Regression gate for the per-realm prototype rebuild in the TGocciaSharedPrototype
  units (#892 / ADR 0083).

  ~30 value units (Map, Set, WeakMap/WeakSet/WeakRef, FinalizationRegistry,
  Promise, ArrayBuffer, all Intl*, all Temporal*, ...) build their prototype via
  TGocciaSharedPrototype.Create(Self), where Self is the current realm's host
  instance (unpinned when the realm is torn down) whose methods back the
  prototype's members. Those members are now rebuilt per realm. Previously they
  were cached in a cross-realm threadvar, which bound every later realm's
  prototype to the FIRST realm's host; once that realm was destroyed and the GC
  collected, the host was freed and the cached `procedure of object` callbacks
  pointed at a freed instance. ADR 0083 removed that cross-realm cache.

  This test drives the multi-realm path: it runs many independent engines (each
  its own realm) on one thread, forcing a full GC collection and a heap stomp
  between them, then exercises the shared-prototype types in the later realms.
  Each script self-verifies and throws on any discrepancy, so a regression that
  reintroduced a cross-realm cache (binding a later realm to a freed host) would
  surface as a thrown error (caught below) or a hard crash. See docs/adr/0083. }

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
  // Each cycle builds a fresh engine/realm and rebuilds every unit's prototype
  // members bound to that realm's own host, then tears the realm down (RunScript
  // frees the engine) and the collect below reclaims that host. Because members
  // are rebuilt per realm, no later realm is ever bound to an earlier realm's
  // freed host. If a regression reintroduced a cross-realm member cache, cycles
  // 1+ would dispatch against cycle 0's freed host and throw or crash here;
  // correct results across all cycles prove the per-realm rebuild holds.
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
