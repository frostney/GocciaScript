{ Pre-initialises all shared prototypes and class-level singletons before
  worker threads are spawned. Must be called once on the main thread.

  The shared prototypes (class var FShared / FSharedArrayPrototype etc.) are
  lazily created on first object construction. Without pre-initialisation,
  two worker threads could race to initialise the same prototype, causing
  data corruption. Calling this procedure forces all lazy init to complete
  while only the main thread is running.

  The same applies to the lexer keyword table (class var FKeywords) which
  is populated on the first TGocciaLexer.Create call. The throwaway engine
  alone does not trigger lexer construction because it never calls Execute,
  so we explicitly create a throwaway lexer to ensure the keyword table
  exists before workers start. }

unit Goccia.Threading.Init;

{$I Goccia.inc}

interface

uses
  Goccia.Engine,
  Goccia.Runtime;

type
  TGocciaEngineInitializer = procedure(const AEngine: TGocciaEngine);

{ Force-creates a throwaway engine to trigger lazy shared prototype
  initialisation for every built-in type. Safe to call multiple times
  (subsequent calls are no-ops once prototypes are populated). }
procedure EnsureSharedPrototypesInitialized(
  const ARuntimeGlobals: TGocciaRuntimeGlobals = [];
  const AInitializer: TGocciaEngineInitializer = nil);

implementation

uses
  Classes,

  Goccia.GarbageCollector,
  Goccia.Lexer;

procedure EnsureSharedPrototypesInitialized(
  const ARuntimeGlobals: TGocciaRuntimeGlobals;
  const AInitializer: TGocciaEngineInitializer);
var
  Source: TStringList;
  Engine: TGocciaEngine;
  Lexer: TGocciaLexer;
begin
  // Force lexer keyword table initialisation. The class var FKeywords is
  // populated on the first TGocciaLexer.Create call. Without this, two
  // worker threads creating lexers simultaneously would race on
  // InitKeywords, corrupting the shared TOrderedStringMap and causing
  // SIGILL via corrupted hash-table pointers.
  Lexer := TGocciaLexer.Create('', '<thread-init>');
  Lexer.Free;

  Source := TStringList.Create;
  try
    // Create a minimal engine. The constructor registers all built-in
    // types, which triggers lazy shared prototype initialisation for
    // every value type (Array, Object, Map, Set, Promise, etc.).
    Engine := TGocciaEngine.Create('<thread-init>', Source);
    try
      if ARuntimeGlobals <> [] then
        AttachRuntimeExtension(Engine, ARuntimeGlobals);
      if Assigned(AInitializer) then
        AInitializer(Engine);
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
