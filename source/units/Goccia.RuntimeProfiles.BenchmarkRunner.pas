unit Goccia.RuntimeProfiles.BenchmarkRunner;

{$I Goccia.inc}

interface

uses
  Goccia.Runtime;

type
  TGocciaBenchmarkRunnerRuntimeProfile = class(TGocciaRuntimeProfile)
  public
    class procedure Apply(const ARuntime: TGocciaRuntimeCore); override;
  end;

implementation

uses
  Goccia.RuntimeExtensions.Benchmark,
  Goccia.RuntimeProfiles.Loader;

class procedure TGocciaBenchmarkRunnerRuntimeProfile.Apply(
  const ARuntime: TGocciaRuntimeCore);
begin
  TGocciaLoaderRuntimeProfile.Apply(ARuntime);
  ARuntime.Install(TGocciaBenchmarkRuntimeExtension.Create);
end;

end.
