unit Goccia.RuntimeProfiles.TestRunner;

{$I Goccia.inc}

interface

uses
  Goccia.Runtime;

type
  TGocciaTestRunnerRuntimeProfile = class(TGocciaRuntimeProfile)
  public
    class procedure Apply(const ARuntime: TGocciaRuntimeCore); override;
  end;

implementation

uses
  Goccia.RuntimeExtensions.TestingLibrary,
  Goccia.RuntimeProfiles.Loader;

class procedure TGocciaTestRunnerRuntimeProfile.Apply(
  const ARuntime: TGocciaRuntimeCore);
begin
  TGocciaLoaderRuntimeProfile.Apply(ARuntime);
  ARuntime.Install(TGocciaTestingLibraryRuntimeExtension.Create);
end;

end.
