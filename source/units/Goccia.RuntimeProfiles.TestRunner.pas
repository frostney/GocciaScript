unit Goccia.RuntimeProfiles.TestRunner;

{$I Goccia.inc}

interface

uses
  Goccia.Runtime;

procedure ApplyTestRunnerRuntimeProfile(const ARuntime: TGocciaRuntimeCore);

implementation

uses
  Goccia.RuntimeExtensions.TestingLibrary,
  Goccia.RuntimeProfiles.Loader;

procedure ApplyTestRunnerRuntimeProfile(const ARuntime: TGocciaRuntimeCore);
begin
  ApplyLoaderRuntimeProfile(ARuntime);
  ARuntime.Install(TGocciaTestingLibraryRuntimeExtension.Create);
end;

end.
