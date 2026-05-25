unit Goccia.CLI.EngineSetup;

{$I Goccia.inc}

interface

uses
  Goccia.CLI.Options;

procedure InitializeCoverageIfEnabled(const AOptions: TGocciaCoverageOptions);
procedure ShutdownCoverageIfEnabled(const AOptions: TGocciaCoverageOptions);

procedure InitializeProfilerIfEnabled(const AOptions: TGocciaProfilerOptions);
procedure ShutdownProfilerIfEnabled(const AOptions: TGocciaProfilerOptions);

implementation

uses
  Goccia.Coverage,
  Goccia.Profiler;

procedure InitializeCoverageIfEnabled(const AOptions: TGocciaCoverageOptions);
begin
  if AOptions.Enabled.Present or AOptions.Format.Present or
     AOptions.OutputPath.Present then
  begin
    TGocciaCoverageTracker.Initialize;
    TGocciaCoverageTracker.Instance.Enabled := True;
  end;
end;

procedure ShutdownCoverageIfEnabled(const AOptions: TGocciaCoverageOptions);
begin
  if Assigned(TGocciaCoverageTracker.Instance) then
    TGocciaCoverageTracker.Shutdown;
end;

procedure InitializeProfilerIfEnabled(const AOptions: TGocciaProfilerOptions);
begin
  if AOptions.Mode.Present then
  begin
    TGocciaProfiler.Initialize;
    TGocciaProfiler.Instance.Enabled := True;

    case AOptions.Mode.Value of
      Goccia.CLI.Options.pmOpcodes:
        TGocciaProfiler.Instance.Mode := [Goccia.Profiler.pmOpcodes];
      Goccia.CLI.Options.pmFunctions:
        TGocciaProfiler.Instance.Mode := [Goccia.Profiler.pmFunctions];
      Goccia.CLI.Options.pmAll:
        TGocciaProfiler.Instance.Mode := [Goccia.Profiler.pmOpcodes,
          Goccia.Profiler.pmFunctions];
    end;
  end;
end;

procedure ShutdownProfilerIfEnabled(const AOptions: TGocciaProfilerOptions);
begin
  if Assigned(TGocciaProfiler.Instance) then
    TGocciaProfiler.Shutdown;
end;

end.
