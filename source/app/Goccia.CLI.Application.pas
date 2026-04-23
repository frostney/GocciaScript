unit Goccia.CLI.Application;

{$I Goccia.inc}

interface

uses
  Classes,
  SysUtils,

  CLI.ConfigFile,
  CLI.Options,

  Goccia.Application,
  Goccia.Engine,
  Goccia.Engine.Backend,
  Goccia.Executor;

type
  TGocciaCLIApplication = class(TGocciaApplication)
  private
    FHelp: TGocciaFlagOption;
    FJobs: TGocciaIntegerOption;
    FLog: TGocciaStringOption;
    FLogFileHandle: TextFile;
    FLogLock: TRTLCriticalSection;
    FLogFileOpen: Boolean;
    FEngineOptions: TGocciaEngineOptions;
    FCoverageOptions: TGocciaCoverageOptions;
    FProfilerOptions: TGocciaProfilerOptions;
    FOwnedOptions: TGocciaOptionBaseList;
    FAllOptions: TGocciaOptionArray;
    procedure BuildAllOptions;
    procedure InitializeSingletons;
    procedure ShutdownSingletons;
    procedure HandleConsoleLog(const AMethod, ALine: string);
    procedure OpenLogFile;
    procedure CloseLogFile;
  protected
    procedure Configure; virtual; abstract;
    function UsageLine: string; virtual; abstract;
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
    function EffectiveBuiltins: TGocciaGlobalBuiltins;
    { Discover the nearest goccia.json/json5/toml for a file and
      return its parsed entries.  Returns an empty array when no
      config is found.  Thread-safe: does not mutate shared state. }
    function DiscoverFileConfig(
      const AFileName: string): TConfigEntryArray;
    function CreateEngine(const AFileName: string;
      const ASource: TStringList): TGocciaEngine; overload;
    function CreateEngine(const AFileName: string;
      const ASource: TStringList;
      const AExecutor: TGocciaExecutor): TGocciaEngine; overload;
    { Returns the effective job count: --jobs value, or ProcessorCount,
      capped to AFileCount. Returns 1 when parallelism is not desired. }
    function GetJobCount(const AFileCount: Integer): Integer;
    property EngineOptions: TGocciaEngineOptions read FEngineOptions;
    property CoverageOptions: TGocciaCoverageOptions read FCoverageOptions;
    property ProfilerOptions: TGocciaProfilerOptions read FProfilerOptions;
  public
    constructor Create(const AName: string); override;
    destructor Destroy; override;
  end;

implementation

uses
  Math,

  CLI.Parser,

  Goccia.Builtins.Console,
  Goccia.CLI.EngineSetup,
  Goccia.CLI.Help,
  Goccia.Coverage,
  Goccia.FileExtensions,
  Goccia.GarbageCollector,
  Goccia.JSON,
  Goccia.JSON5,
  Goccia.Modules.Configuration,
  Goccia.Profiler,
  Goccia.StackLimit,
  Goccia.Timeout,
  Goccia.TOML,
  Goccia.Values.ArrayValue,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

const
  CONFIG_BASE_NAME = 'goccia';
  CONFIG_EXTENSIONS: array[0..2] of string = (EXT_TOML, EXT_JSON5, EXT_JSON);

{ ── Config file bridge parsers ─────────────────────────────── }

{ Extract top-level key-value pairs from a TGocciaObjectValue. }
function ExtractObjectEntries(
  const AObject: TGocciaObjectValue): TConfigEntryArray;
var
  Keys: TArray<string>;
  I, J, Count: Integer;
  Key, ElementValue: string;
  Value: TGocciaValue;
  Arr: TGocciaArrayValue;
begin
  Keys := AObject.GetOwnPropertyKeys;
  SetLength(Result, Length(Keys) * 2);
  Count := 0;

  for I := 0 to High(Keys) do
  begin
    Key := Keys[I];
    Value := AObject.GetProperty(Key);
    if not Assigned(Value) then
      Continue;

    if Value is TGocciaStringLiteralValue then
    begin
      if Count >= Length(Result) then
        SetLength(Result, Length(Result) * 2 + 1);
      Result[Count].Key := Key;
      Result[Count].Value := TGocciaStringLiteralValue(Value).Value;
      Inc(Count);
    end
    else if Value is TGocciaNumberLiteralValue then
    begin
      if Count >= Length(Result) then
        SetLength(Result, Length(Result) * 2 + 1);
      Result[Count].Key := Key;
      Result[Count].Value := TGocciaNumberLiteralValue(Value)
        .ToStringLiteral.Value;
      Inc(Count);
    end
    else if Value is TGocciaBooleanLiteralValue then
    begin
      if Count >= Length(Result) then
        SetLength(Result, Length(Result) * 2 + 1);
      Result[Count].Key := Key;
      if TGocciaBooleanLiteralValue(Value).Value then
        Result[Count].Value := 'true'
      else
        Result[Count].Value := 'false';
      Inc(Count);
    end
    else if Value is TGocciaArrayValue then
    begin
      Arr := TGocciaArrayValue(Value);
      for J := 0 to Arr.GetLength - 1 do
      begin
        Value := Arr.GetElement(J);
        if Value is TGocciaStringLiteralValue then
          ElementValue := TGocciaStringLiteralValue(Value).Value
        else if Value is TGocciaNumberLiteralValue then
          ElementValue := TGocciaNumberLiteralValue(Value)
            .ToStringLiteral.Value
        else if Value is TGocciaBooleanLiteralValue then
          ElementValue := BoolToStr(
            TGocciaBooleanLiteralValue(Value).Value, 'true', 'false')
        else
          Continue;
        if Count >= Length(Result) then
          SetLength(Result, Length(Result) * 2 + 1);
        Result[Count].Key := Key;
        Result[Count].Value := ElementValue;
        Inc(Count);
      end;
    end;
    { Objects and other types are silently skipped. }
  end;

  SetLength(Result, Count);
end;

function ParseJSON5Config(const AContent: string): TConfigEntryArray;
var
  Parser: TGocciaJSON5Parser;
  Parsed: TGocciaValue;
begin
  SetLength(Result, 0);
  Parser := TGocciaJSON5Parser.Create;
  try
    Parsed := Parser.Parse(AContent);
    if Assigned(TGarbageCollector.Instance) and Assigned(Parsed) then
      TGarbageCollector.Instance.AddTempRoot(Parsed);
    try
      if Parsed is TGocciaObjectValue then
        Result := ExtractObjectEntries(TGocciaObjectValue(Parsed));
    finally
      if Assigned(TGarbageCollector.Instance) and Assigned(Parsed) then
        TGarbageCollector.Instance.RemoveTempRoot(Parsed);
    end;
  finally
    Parser.Free;
  end;
end;

function ParseTOMLConfig(const AContent: string): TConfigEntryArray;
var
  Parser: TGocciaTOMLParser;
  Parsed: TGocciaObjectValue;
begin
  SetLength(Result, 0);
  Parser := TGocciaTOMLParser.Create;
  try
    Parsed := Parser.Parse(AContent);
    if Assigned(TGarbageCollector.Instance) and Assigned(Parsed) then
      TGarbageCollector.Instance.AddTempRoot(Parsed);
    try
      if Assigned(Parsed) then
        Result := ExtractObjectEntries(Parsed);
    finally
      if Assigned(TGarbageCollector.Instance) and Assigned(Parsed) then
        TGarbageCollector.Instance.RemoveTempRoot(Parsed);
    end;
  finally
    Parser.Free;
  end;
end;

var
  GConfigParsersRegistered: Boolean = False;

procedure EnsureConfigParsersRegistered;
begin
  if GConfigParsersRegistered then
    Exit;
  RegisterConfigParser(EXT_JSON5, @ParseJSON5Config);
  RegisterConfigParser(EXT_TOML, @ParseTOMLConfig);
  GConfigParsersRegistered := True;
end;

{ FPC 3.2.2 GetCPUCount uses wrong _SC_NPROCESSORS_ONLN constants on
  macOS (expects Linux 84, actual macOS value is 58) and may also fail
  on some Linux configurations.  Call sysconf / sysctlbyname directly
  with the correct per-OS constant so we always detect all cores. }

{$IFDEF UNIX}
function libc_sysconf(Name: Integer): Int64; cdecl; external 'c' name 'sysconf';
{$ENDIF}

function GetProcessorCount: Integer;
{$IFDEF UNIX}
const
  {$IFDEF DARWIN}
  SC_NPROCESSORS_ONLN = 58;
  {$ELSE}
  SC_NPROCESSORS_ONLN = 84;   { Linux }
  {$ENDIF}
var
  N: Int64;
{$ENDIF}
begin
  {$IFDEF UNIX}
  N := libc_sysconf(SC_NPROCESSORS_ONLN);
  if N > 0 then
    Result := Integer(N)
  else
    Result := 1;
  {$ELSE}
  Result := TThread.ProcessorCount;
  if Result < 1 then
    Result := 1;
  {$ENDIF}
end;

{ TGocciaCLIApplication }

constructor TGocciaCLIApplication.Create(const AName: string);
begin
  inherited Create(AName);
  FOwnedOptions := TGocciaOptionBaseList.Create(True);
  FEngineOptions := nil;
  FCoverageOptions := nil;
  FProfilerOptions := nil;
  FHelp := nil;
  FJobs := nil;
  FLog := nil;
  FLogFileOpen := False;
end;

destructor TGocciaCLIApplication.Destroy;
begin
  CloseLogFile;
  FOwnedOptions.Free;
  FEngineOptions.Free;
  FCoverageOptions.Free;
  FProfilerOptions.Free;
  FHelp.Free;
  FJobs.Free;
  FLog.Free;
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

function TGocciaCLIApplication.EffectiveBuiltins: TGocciaGlobalBuiltins;
begin
  Result := GlobalBuiltins;
  if Assigned(FEngineOptions) and FEngineOptions.UnsafeFFI.Present then
    Include(Result, ggFFI);
end;

function TGocciaCLIApplication.DiscoverFileConfig(
  const AFileName: string): TConfigEntryArray;
var
  StartDir, ConfigPath: string;
begin
  SetLength(Result, 0);
  if AFileName = '' then
    Exit;
  EnsureConfigParsersRegistered;
  StartDir := ExtractFilePath(ExpandFileName(AFileName));
  if StartDir = '' then
    StartDir := GetCurrentDir;
  ConfigPath := DiscoverConfigFile(StartDir,
    [CONFIG_BASE_NAME], CONFIG_EXTENSIONS);
  if ConfigPath <> '' then
    Result := ParseConfigFile(ConfigPath);
end;

{ Apply per-file config entries to the engine.
  Priority: CLI flag > per-file config > root config > default.
  FromCommandLine distinguishes CLI-set options from root-config-set options
  so that a per-file config can override a root-level config value. }
procedure ApplyFileConfigToEngine(const AEngine: TGocciaEngine;
  const AEngineOptions: TGocciaEngineOptions;
  const AFileConfig: TConfigEntryArray);
var
  ValueStr: string;
  MemoryLimit: Int64;
  GC: TGarbageCollector;
begin
  if not Assigned(AEngineOptions) then
    Exit;

  { ASI: CLI flag > per-file config > root config > default (false) }
  AEngine.ASIEnabled := ResolveFlagOption(
    AEngineOptions.ASI, AFileConfig, 'asi');

  { compat-var: CLI flag > per-file config > root config > default (false) }
  AEngine.VarEnabled := ResolveFlagOption(
    AEngineOptions.CompatVar, AFileConfig, 'compat-var');

  { compat-function: CLI flag > per-file config > root config > default (false) }
  AEngine.FunctionEnabled := ResolveFlagOption(
    AEngineOptions.CompatFunction, AFileConfig, 'compat-function');

  { unsafe-function-constructor: CLI flag > per-file config > root config > default (false) }
  AEngine.FunctionConstructor.Enabled := ResolveFlagOption(
    AEngineOptions.UnsafeFunctionConstructor, AFileConfig, 'unsafe-function-constructor');

  { max-memory: CLI > per-file config > root config > system default.
    Always set explicitly so a previous file's per-file override does
    not leak into subsequent files (GC.MaxBytes is process-global). }
  GC := TGarbageCollector.Instance;
  if Assigned(GC) then
  begin
    if AEngineOptions.MaxMemory.FromCommandLine then
      GC.MaxBytes := AEngineOptions.MaxMemory.Value
    else if FindConfigEntry(AFileConfig, 'max-memory', ValueStr) then
    begin
      if not TryStrToInt64(ValueStr, MemoryLimit) then
        raise Exception.CreateFmt(
          'Invalid max-memory value in config: %s', [ValueStr]);
      GC.MaxBytes := MemoryLimit;
    end
    else if AEngineOptions.MaxMemory.Present then
      GC.MaxBytes := AEngineOptions.MaxMemory.Value
    else
      GC.MaxBytes := GC.SuggestedMaxBytes;
  end;
end;

function TGocciaCLIApplication.CreateEngine(const AFileName: string;
  const ASource: TStringList): TGocciaEngine;
var
  FileConfig: TConfigEntryArray;
begin
  Result := TGocciaEngine.Create(AFileName, ASource, EffectiveBuiltins);
  try
    FileConfig := DiscoverFileConfig(AFileName);
    if Assigned(FEngineOptions) then
    begin
      ConfigureModuleResolver(Result.Resolver, AFileName,
        FEngineOptions.ImportMap.ValueOr(''), FEngineOptions.Aliases.Values);
      ApplyFileConfigToEngine(Result, FEngineOptions, FileConfig);
    end;
    if FLogFileOpen then
      Result.BuiltinConsole.LogCallback := HandleConsoleLog;
  except
    Result.Free;
    raise;
  end;
end;

function TGocciaCLIApplication.CreateEngine(const AFileName: string;
  const ASource: TStringList; const AExecutor: TGocciaExecutor): TGocciaEngine;
var
  FileConfig: TConfigEntryArray;
begin
  Result := TGocciaEngine.Create(AFileName, ASource, EffectiveBuiltins,
    AExecutor);
  try
    FileConfig := DiscoverFileConfig(AFileName);
    if Assigned(FEngineOptions) then
    begin
      ConfigureModuleResolver(Result.Resolver, AFileName,
        FEngineOptions.ImportMap.ValueOr(''), FEngineOptions.Aliases.Values);
      ApplyFileConfigToEngine(Result, FEngineOptions, FileConfig);
    end;
    if FLogFileOpen then
      Result.BuiltinConsole.LogCallback := HandleConsoleLog;
  except
    Result.Free;
    raise;
  end;
end;

procedure TGocciaCLIApplication.HandleConsoleLog(const AMethod, ALine: string);
begin
  EnterCriticalSection(FLogLock);
  try
    WriteLn(FLogFileHandle, '[' + AMethod + '] ' + ALine);
  finally
    LeaveCriticalSection(FLogLock);
  end;
end;

procedure TGocciaCLIApplication.OpenLogFile;
begin
  if FLogFileOpen then
    Exit;
  InitCriticalSection(FLogLock);
  try
    AssignFile(FLogFileHandle, FLog.Value);
    Rewrite(FLogFileHandle);
    FLogFileOpen := True;
  except
    DoneCriticalSection(FLogLock);
    raise;
  end;
end;

procedure TGocciaCLIApplication.CloseLogFile;
begin
  if not FLogFileOpen then
    Exit;
  try
    CloseFile(FLogFileHandle);
  finally
    FLogFileOpen := False;
    DoneCriticalSection(FLogLock);
  end;
end;

procedure TGocciaCLIApplication.Validate;
begin
  // Override point for subclasses
end;

procedure TGocciaCLIApplication.AfterExecute;
begin
  // Override point for subclasses
end;

function TGocciaCLIApplication.GetJobCount(const AFileCount: Integer): Integer;
begin
  if AFileCount <= 1 then
    Exit(1);
  if Assigned(FJobs) and FJobs.Present then
    Result := Max(1, FJobs.Value)
  else
    Result := GetProcessorCount;
  Result := Min(Result, AFileCount);
end;

procedure TGocciaCLIApplication.InitializeSingletons;
begin
  SetMaxStackDepth(DEFAULT_MAX_STACK_DEPTH);
  if Assigned(FEngineOptions) then
    SetMaxStackDepth(Max(0, FEngineOptions.StackSize.ValueOr(DEFAULT_MAX_STACK_DEPTH)));
  if Assigned(FCoverageOptions) then
    InitializeCoverageIfEnabled(FCoverageOptions);
  if Assigned(FProfilerOptions) then
    InitializeProfilerIfEnabled(FProfilerOptions);
end;

procedure TGocciaCLIApplication.ShutdownSingletons;
begin
  if Assigned(FProfilerOptions) then
    ShutdownProfilerIfEnabled(FProfilerOptions);
  if Assigned(FCoverageOptions) then
    ShutdownCoverageIfEnabled(FCoverageOptions);
end;

function ResolveConfigStartDirectory(const APaths: TStringList): string;
var
  FirstPath: string;
begin
  if APaths.Count > 0 then
  begin
    FirstPath := ExpandFileName(APaths[0]);
    if DirectoryExists(FirstPath) then
      Exit(FirstPath);
    if FileExists(FirstPath) or (ExtractFilePath(FirstPath) <> '') then
      Exit(ExtractFilePath(FirstPath));
  end;
  Result := GetCurrentDir;
end;

procedure TGocciaCLIApplication.Execute;
var
  Paths: TStringList;
  HelpText, ConfigPath, ConfigStartDir: string;
  I: Integer;
begin
  Configure;

  FHelp := TGocciaFlagOption.Create('help', 'Show this help message');
  FHelp.ShortName := 'h';

  FJobs := TGocciaIntegerOption.Create('jobs', 'Number of parallel worker threads');
  FJobs.ShortName := 'j';

  FLog := TGocciaStringOption.Create('log', 'Write console output to a log file');

  BuildAllOptions;
  // Append --jobs and --log after BuildAllOptions so they appear in help
  SetLength(FAllOptions, Length(FAllOptions) + 2);
  FAllOptions[High(FAllOptions) - 1] := FJobs;
  FAllOptions[High(FAllOptions)] := FLog;

  { Parse CLI first so we know the entry file path. }
  Paths := ParseCommandLine(FAllOptions);
  try
    if FHelp.Present then
    begin
      HelpText := GenerateHelpText(Name, UsageLine, FAllOptions);
      Write(HelpText);
      Exit;
    end;

    { Snapshot CLI origin: any option Present at this point was set
      by the command line.  ApplyConfigFile below may set additional
      options, but those will not be marked FromCommandLine. }
    for I := 0 to High(FAllOptions) do
      if FAllOptions[I].Present then
        FAllOptions[I].MarkFromCommandLine;

    { Discover config file starting from the entry file's directory.
      Apply as defaults — options already set by CLI are skipped. }
    EnsureConfigParsersRegistered;
    ConfigStartDir := ResolveConfigStartDirectory(Paths);
    ConfigPath := DiscoverConfigFile(ConfigStartDir,
      [CONFIG_BASE_NAME], CONFIG_EXTENSIONS);
    if ConfigPath <> '' then
      ApplyConfigFile(ConfigPath, FAllOptions);

    Validate;

    if FLog.Present then
      OpenLogFile;

    InitializeSingletons;
    try
      ExecuteWithPaths(Paths);
      AfterExecute;
    finally
      ShutdownSingletons;
      CloseLogFile;
    end;
  finally
    Paths.Free;
  end;
end;

end.
