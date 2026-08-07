unit Goccia.RuntimeExtensions.TestingLibrary;

{$I Goccia.inc}

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
    FTestModule: TGocciaRuntimeNamespaceModuleRegistration;
    function MaterializeTestModule: TGocciaValue;
  public
    constructor Create(const ASnapshotHost: IGocciaSnapshotHost = nil;
      const ASnapshotUpdateMode: TGocciaSnapshotUpdateMode = sumNew;
      const ASnapshotFormatter: IGocciaSnapshotFormatter = nil);
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
  const ASnapshotFormatter: IGocciaSnapshotFormatter);
begin
  inherited Create;
  FSnapshotHost := ASnapshotHost;
  FSnapshotUpdateMode := ASnapshotUpdateMode;
  FSnapshotFormatter := ASnapshotFormatter;
end;

procedure TGocciaTestingLibraryRuntimeExtension.Attach(
  const ARuntime: TGocciaRuntimeCore);
begin
  inherited Attach(ARuntime);
  FBuiltinTestAssertions := TGocciaTestAssertions.Create('TestAssertions',
    Runtime.Engine.Interpreter.GlobalScope, Runtime.Engine.ThrowError,
    FSnapshotHost, FSnapshotUpdateMode, FSnapshotFormatter);
  Runtime.RegisterRuntimeGlobalName('TestAssertions');

  { The same helpers the runner exposes as globals are also importable, so a
    suite can name what it uses and so embedders that do not install globals
    still reach the testing API. }
  FTestModule := TGocciaRuntimeNamespaceModuleRegistration.Create(Runtime,
    'goccia:test', MaterializeTestModule);
end;

function TGocciaTestingLibraryRuntimeExtension.MaterializeTestModule: TGocciaValue;
begin
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
