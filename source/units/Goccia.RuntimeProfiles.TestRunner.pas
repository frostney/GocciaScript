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
  const ASnapshotFormatter: IGocciaSnapshotFormatter = nil;
  const AVitestCompat: Boolean = True); overload;

implementation

uses
  Goccia.RuntimeExtensions.TestingLibrary,
  Goccia.RuntimeExtensions.Timers,
  Goccia.RuntimeExtensions.VitestCompat,
  Goccia.RuntimeProfiles.Loader;

procedure ApplyTestRunnerRuntimeProfile(const ARuntime: TGocciaRuntimeCore);
  overload;
begin
  ApplyTestRunnerRuntimeProfile(ARuntime, nil, sumNew, nil, True);
end;

procedure ApplyTestRunnerRuntimeProfile(const ARuntime: TGocciaRuntimeCore;
  const ASnapshotHost: IGocciaSnapshotHost;
  const ASnapshotUpdateMode: TGocciaSnapshotUpdateMode;
  const ASnapshotFormatter: IGocciaSnapshotFormatter;
  const AVitestCompat: Boolean); overload;
begin
  { The loader profile's module-only testing install is suppressed here: the
    runner needs the globals half too, and both halves come from one extension
    so that globals and `goccia:test` drive the same registry. Installing the
    loader's copy as well would register `goccia:test` twice. }
  ApplyLoaderRuntimeProfile(ARuntime, False);
  ARuntime.Install(TGocciaTestingLibraryRuntimeExtension.Create(
    ASnapshotHost, ASnapshotUpdateMode, ASnapshotFormatter, True));
  { The timer globals and `goccia:timers` are runner-only. They are
    deterministic and carry no ambient authority, but a scheduling surface is
    still a surface, and the acceptance target — a Vitest suite draining a
    setTimeout-based scheduler through fake timers — is the runner's.
    WaitForIdle runs extensions in insertion order, so this is installed AFTER
    the testing library: a real-mode timer must drain only once the other
    extensions have gone idle. }
  ARuntime.Install(TGocciaTimersRuntimeExtension.Create);
  { A suite written against Vitest imports from a bare `vitest` specifier,
    which resolves to nothing otherwise. Installed by default so such a suite
    runs unchanged; --no-vitest-compat leaves the specifier unresolvable. }
  if AVitestCompat then
    ARuntime.Install(TGocciaVitestCompatRuntimeExtension.Create);
end;

end.
