unit Goccia.CLI.Application;

{$I Goccia.inc}

interface

uses
  Classes,
  SysUtils,

  Goccia.Application,
  Goccia.CLI.Options;

type
  TGocciaCLIApplicationClass = class of TGocciaCLIApplication;

  TGocciaCLIApplication = class(TGocciaApplication)
  private
    FUsageLine: string;
    FHelp: TGocciaFlagOption;
    FEngineOptions: TGocciaEngineOptions;
    FCoverageOptions: TGocciaCoverageOptions;
    FProfilerOptions: TGocciaProfilerOptions;
    FOwnedOptions: TGocciaOptionBaseList;
    FAllOptions: TGocciaOptionArray;
    procedure BuildAllOptions;
    procedure InitializeSingletons;
    procedure ShutdownSingletons;
  protected
    procedure Configure; virtual; abstract;
    procedure Execute; override;
    procedure ExecuteWithPaths(const APaths: TStringList); virtual; abstract;
    procedure Validate; virtual;
    procedure AfterExecute; virtual;
    function AddEngineOptions: TGocciaEngineOptions;
    function AddCoverageOptions: TGocciaCoverageOptions;
    function AddProfilerOptions: TGocciaProfilerOptions;
    function AddFlag(const AName, AHelp: string): TGocciaFlagOption;
    function AddString(const AName, AHelp: string): TGocciaStringOption;
    function AddInteger(const AName, AHelp: string): TGocciaIntegerOption;
    function AddRepeatable(const AName, AHelp: string): TGocciaRepeatableOption;
    function Add(const AOption: TGocciaOptionBase): TGocciaOptionBase;
    property EngineOptions: TGocciaEngineOptions read FEngineOptions;
    property CoverageOptions: TGocciaCoverageOptions read FCoverageOptions;
    property ProfilerOptions: TGocciaProfilerOptions read FProfilerOptions;
  public
    constructor Create(const AName, AUsageLine: string);
    destructor Destroy; override;
    class function RunCLI(const AClass: TGocciaCLIApplicationClass;
      const AName, AUsageLine: string): Integer;
  end;

implementation

uses
  Goccia.CLI.EngineSetup,
  Goccia.CLI.Help,
  Goccia.CLI.Parser,
  Goccia.Coverage,
  Goccia.Profiler;

{ TGocciaCLIApplication }

constructor TGocciaCLIApplication.Create(const AName, AUsageLine: string);
begin
  inherited Create(AName);
  FUsageLine := AUsageLine;
  FOwnedOptions := TGocciaOptionBaseList.Create(True);
  FEngineOptions := nil;
  FCoverageOptions := nil;
  FProfilerOptions := nil;
  FHelp := nil;
end;

destructor TGocciaCLIApplication.Destroy;
begin
  FOwnedOptions.Free;
  FEngineOptions.Free;
  FCoverageOptions.Free;
  FProfilerOptions.Free;
  FHelp.Free;
  inherited Destroy;
end;

procedure TGocciaCLIApplication.BuildAllOptions;
var
  Combined: array of TGocciaOptionArray;
  Count, I: Integer;
begin
  Count := 0;
  SetLength(Combined, 4);

  if Assigned(FEngineOptions) then
  begin
    Combined[Count] := FEngineOptions.Options;
    Inc(Count);
  end;

  if Assigned(FCoverageOptions) then
  begin
    Combined[Count] := FCoverageOptions.Options;
    Inc(Count);
  end;

  if Assigned(FProfilerOptions) then
  begin
    Combined[Count] := FProfilerOptions.Options;
    Inc(Count);
  end;

  if FOwnedOptions.Count > 0 then
  begin
    SetLength(Combined[Count], FOwnedOptions.Count);
    for I := 0 to FOwnedOptions.Count - 1 do
      Combined[Count][I] := FOwnedOptions[I];
    Inc(Count);
  end;

  SetLength(Combined, Count);
  FAllOptions := ConcatOptions(Combined);

  SetLength(FAllOptions, Length(FAllOptions) + 1);
  FAllOptions[High(FAllOptions)] := FHelp;
end;

function TGocciaCLIApplication.AddEngineOptions: TGocciaEngineOptions;
begin
  FEngineOptions := TGocciaEngineOptions.Create;
  Result := FEngineOptions;
end;

function TGocciaCLIApplication.AddCoverageOptions: TGocciaCoverageOptions;
begin
  FCoverageOptions := TGocciaCoverageOptions.Create;
  Result := FCoverageOptions;
end;

function TGocciaCLIApplication.AddProfilerOptions: TGocciaProfilerOptions;
begin
  FProfilerOptions := TGocciaProfilerOptions.Create;
  Result := FProfilerOptions;
end;

function TGocciaCLIApplication.AddFlag(const AName, AHelp: string): TGocciaFlagOption;
begin
  Result := TGocciaFlagOption.Create(AName, AHelp);
  FOwnedOptions.Add(Result);
end;

function TGocciaCLIApplication.AddString(const AName, AHelp: string): TGocciaStringOption;
begin
  Result := TGocciaStringOption.Create(AName, AHelp);
  FOwnedOptions.Add(Result);
end;

function TGocciaCLIApplication.AddInteger(const AName, AHelp: string): TGocciaIntegerOption;
begin
  Result := TGocciaIntegerOption.Create(AName, AHelp);
  FOwnedOptions.Add(Result);
end;

function TGocciaCLIApplication.AddRepeatable(const AName, AHelp: string): TGocciaRepeatableOption;
begin
  Result := TGocciaRepeatableOption.Create(AName, AHelp);
  FOwnedOptions.Add(Result);
end;

function TGocciaCLIApplication.Add(const AOption: TGocciaOptionBase): TGocciaOptionBase;
begin
  FOwnedOptions.Add(AOption);
  Result := AOption;
end;

procedure TGocciaCLIApplication.Validate;
begin
  // Override point for subclasses
end;

procedure TGocciaCLIApplication.AfterExecute;
begin
  // Override point for subclasses
end;

procedure TGocciaCLIApplication.InitializeSingletons;
begin
  if Assigned(FCoverageOptions) and FCoverageOptions.Enabled.Present then
    InitializeCoverageIfEnabled(FCoverageOptions);
  if Assigned(FProfilerOptions) then
    InitializeProfilerIfEnabled(FProfilerOptions);
end;

procedure TGocciaCLIApplication.ShutdownSingletons;
begin
  if Assigned(FProfilerOptions) and Assigned(TGocciaProfiler.Instance) then
    TGocciaProfiler.Shutdown;
  if Assigned(FCoverageOptions) and Assigned(TGocciaCoverageTracker.Instance) then
    TGocciaCoverageTracker.Shutdown;
end;

procedure TGocciaCLIApplication.Execute;
var
  Paths: TStringList;
  HelpText: string;
begin
  Configure;

  FHelp := TGocciaFlagOption.Create('help', 'Show this help message');
  FHelp.ShortName := 'h';

  BuildAllOptions;

  Paths := ParseCommandLine(FAllOptions);
  try
    if FHelp.Present then
    begin
      HelpText := GenerateHelpText(Name, FUsageLine, FAllOptions);
      Write(HelpText);
      Exit;
    end;

    Validate;

    InitializeSingletons;
    try
      ExecuteWithPaths(Paths);
      AfterExecute;
    finally
      ShutdownSingletons;
    end;
  finally
    Paths.Free;
  end;
end;

class function TGocciaCLIApplication.RunCLI(
  const AClass: TGocciaCLIApplicationClass;
  const AName, AUsageLine: string): Integer;
var
  App: TGocciaCLIApplication;
begin
  App := AClass.Create(AName, AUsageLine);
  try
    Result := App.Run;
  finally
    App.Free;
  end;
end;

end.
