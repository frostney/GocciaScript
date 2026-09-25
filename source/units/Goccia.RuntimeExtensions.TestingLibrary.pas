unit Goccia.RuntimeExtensions.TestingLibrary;

{$I Goccia.inc}

{ Testing library runtime extension.

  The extension installs two independent halves:

  1. The `goccia:test` module namespace, always registered. This is the half
     every host that attaches a runtime gets — importing it is explicit, adds
     no names to the global object, and costs nothing until an import actually
     resolves (the assertions object is materialized lazily, the way the
     sibling data-format extensions materialize theirs).

  2. The testing globals — `describe`, `test`, `it`, `expect`, the lifecycle
     hooks, `mock`, `spyOn`, `runTests` — plus the private Test262 aliases.
     Only GocciaTestRunner asks for these, matching Vitest's `globals: true`.

  Pass AInjectGlobals=False for the module-only half. The two halves share one
  TGocciaTestAssertions instance and therefore one registry, so a host that
  installs both sees the same suites through either spelling. }

interface

uses
  Goccia.Builtins.Testing.SnapshotFormatting,
  Goccia.Builtins.Testing.Snapshots,
  Goccia.Builtins.TestingLibrary,
  Goccia.Runtime,
  Goccia.RuntimeExtensions.NamespaceModule,
  Goccia.Values.Primitives;

type
  TGocciaTestingLibraryRuntimeExtension = class(TGocciaRuntimeExtension)
  private
    FBuiltinTestAssertions: TGocciaTestAssertions;
    FSnapshotHost: IGocciaSnapshotHost;
    FSnapshotUpdateMode: TGocciaSnapshotUpdateMode;
    FSnapshotFormatter: IGocciaSnapshotFormatter;
    FInjectGlobals: Boolean;
    FTestModule: TGocciaRuntimeNamespaceModuleRegistration;
    procedure EnsureTestAssertions;
    function MaterializeTestModule: TGocciaValue;
  public
    constructor Create(const ASnapshotHost: IGocciaSnapshotHost = nil;
      const ASnapshotUpdateMode: TGocciaSnapshotUpdateMode = sumNew;
      const ASnapshotFormatter: IGocciaSnapshotFormatter = nil;
      const AInjectGlobals: Boolean = True);
    { Registers `goccia:test` and nothing else — no global object property, no
      global scope binding. This is what every non-runner host installs. }
    constructor CreateModuleOnly;
    procedure Attach(const ARuntime: TGocciaRuntimeCore); override;
    procedure Detach; override;
    { Hosts that own stdout — the JSON envelope modes of GocciaTestRunner —
      need to silence the reporter's per-test markers. Exposed the same way
      the console extension exposes its builtin. }
    property BuiltinTestAssertions: TGocciaTestAssertions
      read FBuiltinTestAssertions;
  end;

implementation

constructor TGocciaTestingLibraryRuntimeExtension.Create(
  const ASnapshotHost: IGocciaSnapshotHost;
  const ASnapshotUpdateMode: TGocciaSnapshotUpdateMode;
  const ASnapshotFormatter: IGocciaSnapshotFormatter;
  const AInjectGlobals: Boolean);
begin
  inherited Create;
  FSnapshotHost := ASnapshotHost;
  FSnapshotUpdateMode := ASnapshotUpdateMode;
  FSnapshotFormatter := ASnapshotFormatter;
  FInjectGlobals := AInjectGlobals;
end;

constructor TGocciaTestingLibraryRuntimeExtension.CreateModuleOnly;
begin
  Create(nil, sumNew, nil, False);
end;

procedure TGocciaTestingLibraryRuntimeExtension.Attach(
  const ARuntime: TGocciaRuntimeCore);
begin
  inherited Attach(ARuntime);

  { Globals must exist before the entry script runs, so the globals half is
    built eagerly. The module-only half defers to the namespace factory: a
    loader script that never imports `goccia:test` pays nothing for it. }
  if FInjectGlobals then
  begin
    EnsureTestAssertions;
    Runtime.RegisterRuntimeGlobalName('TestAssertions');
  end;

  { The same helpers the runner exposes as globals are also importable, so a
    suite can name what it uses and so hosts that do not install globals
    still reach the testing API. }
  FTestModule := TGocciaRuntimeNamespaceModuleRegistration.Create(Runtime,
    'goccia:test', MaterializeTestModule);
end;

procedure TGocciaTestingLibraryRuntimeExtension.EnsureTestAssertions;
begin
  if not Assigned(FBuiltinTestAssertions) then
    FBuiltinTestAssertions := TGocciaTestAssertions.Create('TestAssertions',
      Runtime.Engine.Interpreter.GlobalScope, Runtime.Engine.ThrowError,
      FSnapshotHost, FSnapshotUpdateMode, FSnapshotFormatter, FInjectGlobals);
end;

function TGocciaTestingLibraryRuntimeExtension.MaterializeTestModule: TGocciaValue;
begin
  EnsureTestAssertions;
  Result := FBuiltinTestAssertions.BuiltinObject;
end;

procedure TGocciaTestingLibraryRuntimeExtension.Detach;
begin
  FTestModule.Free;
  FTestModule := nil;
  FBuiltinTestAssertions.Free;
  FBuiltinTestAssertions := nil;
  inherited;
end;

end.
