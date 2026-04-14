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
  Goccia.Engine;

{ Force-creates a throwaway engine to trigger lazy shared prototype
  initialisation for every built-in type. Safe to call multiple times
  (subsequent calls are no-ops once prototypes are populated). }
procedure EnsureSharedPrototypesInitialized(const ABuiltins: TGocciaGlobalBuiltins);

implementation

uses
  Classes,

  Goccia.GarbageCollector;

procedure EnsureSharedPrototypesInitialized(const ABuiltins: TGocciaGlobalBuiltins);
var
  Source: TStringList;
  Engine: TGocciaEngine;
begin
  Source := TStringList.Create;
  try
    // Create a minimal engine. The constructor registers all built-in
    // types, which triggers lazy shared prototype initialisation for
    // every value type (Array, Object, Map, Set, Promise, etc.).
    Engine := TGocciaEngine.Create('<thread-init>', Source, ABuiltins);
    try
      // Nothing to execute — we only needed the constructor side-effects.
    finally
      Engine.Free;
    end;
    // Clean up the throwaway objects.
    if Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.Collect;
  finally
    Source.Free;
  end;
end;

end.
