unit Goccia.RuntimeExtensions.TestingLibrary;

{$I Goccia.inc}

interface

uses
  Goccia.Builtins.Testing.SnapshotFormatting,
  Goccia.Builtins.Testing.Snapshots,
  Goccia.Builtins.TestingLibrary,
  Goccia.Runtime;

type
  TGocciaTestingLibraryRuntimeExtension = class(TGocciaRuntimeExtension)
  private
    FBuiltinTestAssertions: TGocciaTestAssertions;
    FSnapshotHost: IGocciaSnapshotHost;
    FSnapshotUpdateMode: TGocciaSnapshotUpdateMode;
    FSnapshotFormatter: IGocciaSnapshotFormatter;
  public
    constructor Create(const ASnapshotHost: IGocciaSnapshotHost = nil;
      const ASnapshotUpdateMode: TGocciaSnapshotUpdateMode = sumNew;
      const ASnapshotFormatter: IGocciaSnapshotFormatter = nil);
    procedure Attach(const ARuntime: TGocciaRuntimeCore); override;
    procedure Detach; override;
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
end;

procedure TGocciaTestingLibraryRuntimeExtension.Detach;
begin
  FBuiltinTestAssertions.Free;
  FBuiltinTestAssertions := nil;
  inherited;
end;

end.
