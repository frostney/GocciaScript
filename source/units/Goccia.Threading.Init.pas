{ Pre-initialises all shared prototypes and class-level singletons before
  worker threads are spawned. Must be called once on the main thread.

  The shared prototypes (class var FShared / FSharedArrayPrototype etc.) are
  lazily created on first object construction. Without pre-initialisation,
  two worker threads could race to initialise the same prototype, causing
  data corruption. Calling this procedure forces all lazy init to complete
  while only the main thread is running. }

unit Goccia.Threading.Init;

{$I Goccia.inc}

interface

uses
  Goccia.Engine,
  Goccia.Executor.Interpreter;

type
  TGocciaEngineInitializer = procedure(const AEngine: TGocciaEngine) of object;

{ Force-creates a throwaway engine to trigger lazy shared prototype
  initialisation for every built-in type. Safe to call multiple times
  (subsequent calls are no-ops once prototypes are populated). }
procedure EnsureSharedPrototypesInitialized(
  const AInitializer: TGocciaEngineInitializer = nil);

{ Materialises the lazy heavyweight globals (#747/#790) on the given engine so
  that their process-global initialisation — Intl loads ICU/CLDR data, Temporal
  loads time-zone data — runs now rather than on first touch. Parallel runners
  must call this on the main-thread warm-up engine before spawning workers:
  otherwise the first worker to touch one of these triggers that shared init
  concurrently with other workers, which surfaces as intermittent failures
  (e.g. ICU locale-data fallbacks). It is deliberately NOT folded into
  EnsureSharedPrototypesInitialized, because single-engine callers (the script
  loader, the bundler) rely on these globals staying lazy for boot time. }
procedure WarmUpSharedLazyGlobals(const AEngine: TGocciaEngine);

implementation

uses
  Classes,

  Goccia.GarbageCollector,
  Goccia.Interpreter,
  Goccia.Scope;

procedure EnsureSharedPrototypesInitialized(
  const AInitializer: TGocciaEngineInitializer);
var
  Source: TStringList;
  Engine: TGocciaEngine;
  Executor: TGocciaInterpreterExecutor;
begin
  Source := TStringList.Create;
  try
    // Create a minimal engine. The constructor registers all built-in
    // types, which triggers lazy shared prototype initialisation for
    // every value type (Array, Object, Map, Set, Promise, etc.).
    Executor := TGocciaInterpreterExecutor.Create;
    try
      Engine := TGocciaEngine.Create('<thread-init>', Source, Executor);
      try
        if Assigned(AInitializer) then
          AInitializer(Engine);
        // Nothing to execute — we only needed the constructor side-effects.
      finally
        Engine.Free;
      end;
    finally
      Executor.Free;
    end;
    // Clean up the throwaway objects.
    if (TGarbageCollector.Instance <> nil) then
      TGarbageCollector.Instance.Collect;
  finally
    Source.Free;
  end;
end;

procedure WarmUpSharedLazyGlobals(const AEngine: TGocciaEngine);
const
  HeavyGlobals: array[0 .. 6] of string = ('Temporal', 'Intl', 'Atomics',
    'Proxy', 'Reflect', 'DisposableStack', 'AsyncDisposableStack');
var
  I: Integer;
begin
  for I := Low(HeavyGlobals) to High(HeavyGlobals) do
    AEngine.Interpreter.GlobalScope.GetValue(HeavyGlobals[I]);
end;

end.
