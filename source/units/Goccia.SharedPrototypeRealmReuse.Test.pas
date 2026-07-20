{ Regression gate for the per-realm prototype rebuild in the TGocciaSharedPrototype
  units (#892 / ADR 0084).

  ~30 value units (Map, Set, WeakMap/WeakSet/WeakRef, FinalizationRegistry,
  Promise, ArrayBuffer, all Intl*, all Temporal*, ...) build their prototype via
  TGocciaSharedPrototype.Create(Self), where Self is the current realm's host
  instance (unpinned when the realm is torn down) whose methods back the
  prototype's members. Those members are now rebuilt per realm. Previously they
  were cached in a cross-realm threadvar, which bound every later realm's
  prototype to the FIRST realm's host; once that realm was destroyed and the GC
  collected, the host was freed and the cached `procedure of object` callbacks
  pointed at a freed instance. ADR 0084 removed that cross-realm cache.

  This test drives the multi-realm path: it runs many independent engines (each
  its own realm) on one thread, forcing a full GC collection and a heap stomp
  between them, then exercises the shared-prototype types in the later realms.
  Each script self-verifies and throws on any discrepancy, so a regression that
  reintroduced a cross-realm cache (binding a later realm to a freed host) would
  surface as a thrown error (caught by the shared test runner) or a hard crash.

  It also covers Goccia.Builtins.GlobalRegExp, a constructor-style builtin (not a
  TGocciaSharedPrototype unit) whose per-realm host is freed to the FPC heap rather
  than the GC heap, and which had the same cross-realm member cache. Its cached
  [Symbol.matchAll] / [Symbol.split] callbacks dereference the host's
  FRegExpConstructor on the SpeciesConstructor fallback (receiver constructor =
  undefined), so this test additionally poisons the FPC heap and forces that path:
  it crashed with an access violation before the per-realm rebuild and passes after.
  See docs/adr/0084. }

program Goccia.SharedPrototypeRealmReuse.Test;

{$I Goccia.inc}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  SysUtils,

  Goccia.Builtins.GlobalRegExp,
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
    'if (inst.epochMilliseconds !== 1000) throw new Error("Temporal.Instant");' + sLineBreak +
    // RegExp is a constructor-style builtin (not TGocciaSharedPrototype) whose
    // per-realm host is freed with its engine; [Symbol.split] / [Symbol.matchAll]
    // read the host's FRegExpConstructor field via SpeciesConstructor, so a stale
    // cross-realm member cache dispatches them against a freed host (ADR 0084).
    'const parts = "a-b-c".split(/-/); if (parts.length !== 3 || parts[1] !== "b") throw new Error("RegExp.split");' + sLineBreak +
    'const ms = [..."a1b2".matchAll(/[0-9]/g)]; if (ms.length !== 2 || ms[0][0] !== "1") throw new Error("RegExp.matchAll");' + sLineBreak +
    // constructor=undefined forces RegExp[Symbol.matchAll] and [Symbol.split] down
    // the SpeciesConstructor fallback, which CONSTRUCTS via the host's
    // FRegExpConstructor field -> dereferences the (stale, freed) host, not just
    // reads it. matchAll and split are the only two callbacks that touch the host,
    // so both fallbacks are exercised.
    'const reC = /[0-9]/g; reC.constructor = undefined;' + sLineBreak +
    'const mc = [...reC[Symbol.matchAll]("a1b2")]; if (mc.length !== 2) throw new Error("RegExp.matchAll species-fallback");' + sLineBreak +
    'const reS = /-/; reS.constructor = undefined;' + sLineBreak +
    'const sp = reS[Symbol.split]("a-b-c"); if (sp.length !== 3 || sp[1] !== "b") throw new Error("RegExp.split species-fallback");' + sLineBreak +
    'const ex = /[a-z]/.exec("9x"); if (!ex || ex[0] !== "x") throw new Error("RegExp.exec");' + sLineBreak +
    'if (!/[0-9]/.test("x5")) throw new Error("RegExp.test");' + sLineBreak +
    'if (/abc/.source !== "abc") throw new Error("RegExp.source");';

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
  // Stomp magnitudes are empirical, not tuned to a threshold: they only need to
  // be large enough to reliably reclaim and overwrite a freed host's block before
  // the next realm dispatches against it. They are deliberately generous (verified
  // to fault pre-fix); reducing them weakens the gate, it does not make it stricter.
  CYCLES = 12;                // independent realms run back-to-back on this thread
  FPC_STOMP_PER_CYCLE = 512;  // same-size FPC-heap allocations to reclaim the freed RegExp host
var
  I, J: Integer;
  Stomp: array of TGocciaObjectValue;
  FpcStomp: array of Pointer;
  FpcStompCount: Integer;
  HostSize: NativeUInt;
begin
  // Each cycle builds a fresh engine/realm and rebuilds every unit's prototype
  // members bound to that realm's own host, then tears the realm down (RunScript
  // frees the engine) and the collect below reclaims that host. Because members
  // are rebuilt per realm, no later realm is ever bound to an earlier realm's
  // freed host. If a regression reintroduced a cross-realm member cache, cycles
  // 1+ would dispatch against cycle 0's freed host and throw or crash here;
  // correct results across all cycles prove the per-realm rebuild holds.
  //
  // Two pools are stomped because the hazard lives in two: the
  // TGocciaSharedPrototype hosts are GC-managed (reclaimed by Collect, so the
  // GC-object stomp reuses their freed blocks), while the RegExp builtin host
  // (TGocciaGlobalRegExp, a plain TObject) is freed to the FPC heap. A same-size
  // FPC-heap stomp, kept allocated across the next realm's run, overwrites that
  // freed host so a stale cross-realm RegExp member cache dereferences garbage
  // (hard crash) instead of coincidentally-intact memory.
  HostSize := TGocciaGlobalRegExp.InstanceSize;
  FpcStompCount := 0;
  SetLength(FpcStomp, CYCLES * FPC_STOMP_PER_CYCLE);
  try
    for I := 0 to CYCLES - 1 do
    begin
      TGocciaEngine.RunScript(SELF_VERIFYING_SCRIPT, '<realm-reuse-' + IntToStr(I) + '>');

      // Claim the just-freed FPC-heap RegExp host block with 0xFF FIRST — before
      // the GC churn below can reuse it — and keep it held, so the NEXT realm
      // dispatching a cached RegExp callback against this freed host reads a
      // poisoned pointer (hard crash) rather than coincidentally-intact memory.
      for J := 1 to FPC_STOMP_PER_CYCLE do
      begin
        GetMem(FpcStomp[FpcStompCount], HostSize);
        FillByte(FpcStomp[FpcStompCount]^, HostSize, $FF);
        Inc(FpcStompCount);
      end;

      // Reclaim the just-torn-down realm's unpinned GC host instances...
      TGarbageCollector.Instance.Collect;

      // ...then aggressively reuse the freed GC blocks, so any later dereference
      // of a freed GC host reads stomped memory rather than intact data. The 4096
      // is generous GC-object churn, empirical like the FPC stomp count above.
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
  finally
    for J := 0 to FpcStompCount - 1 do
      FreeMem(FpcStomp[J]);
  end;
end;

begin
  TGarbageCollector.Initialize;
  PinPrimitiveSingletons;
  try
    TestRunnerProgram.AddSuite(TRealmReuseTests.Create('SharedPrototypeRealmReuse'));
    RunGocciaTests;
  finally
    TGarbageCollector.Shutdown;
  end;

  ExitCode := TestResultToExitCode;
end.
