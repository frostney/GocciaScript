unit Goccia.RuntimeProfiles.TestRunner;

{$I Goccia.inc}

interface

uses
  Goccia.Builtins.Testing.SnapshotFormatting,
  Goccia.Builtins.Testing.Snapshots,
  Goccia.Runtime;

procedure ApplyTestRunnerRuntimeProfile(const ARuntime: TGocciaRuntimeCore);
  overload;
procedure ApplyTestRunnerRuntimeProfile(const ARuntime: TGocciaRuntimeCore;
  const ASnapshotHost: IGocciaSnapshotHost;
  const ASnapshotUpdateMode: TGocciaSnapshotUpdateMode;
  const ASnapshotFormatter: IGocciaSnapshotFormatter = nil); overload;

implementation

uses
  Goccia.RuntimeExtensions.TestingLibrary,
  Goccia.RuntimeProfiles.Loader;

procedure ApplyTestRunnerRuntimeProfile(const ARuntime: TGocciaRuntimeCore);
  overload;
begin
  ApplyTestRunnerRuntimeProfile(ARuntime, nil, sumNew, nil);
end;

procedure ApplyTestRunnerRuntimeProfile(const ARuntime: TGocciaRuntimeCore;
  const ASnapshotHost: IGocciaSnapshotHost;
  const ASnapshotUpdateMode: TGocciaSnapshotUpdateMode;
  const ASnapshotFormatter: IGocciaSnapshotFormatter); overload;
begin
  ApplyLoaderRuntimeProfile(ARuntime);
  ARuntime.Install(TGocciaTestingLibraryRuntimeExtension.Create(
    ASnapshotHost, ASnapshotUpdateMode, ASnapshotFormatter));
end;

end.
