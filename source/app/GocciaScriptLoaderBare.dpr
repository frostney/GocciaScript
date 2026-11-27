program GocciaScriptLoaderBare;

{ GocciaScriptLoaderBare: the core engine without a runtime, from a file
  or stdin. The old loader's command line (compatibility flags, source type
  and name, --print, the limits, --profile) over a TGocciaEngine whose host is the
  bare one (TGocciaEngine.Bare): print and Goccia but no console, timers, web APIs
  or host modules, and no module content provider, so an import that finds its
  file fails with ERR_MODULE_LOADING_UNSUPPORTED as the old loader's does.

  The frame bound is the engine's own unless --max-stack gives one: a bound keeps the
  JIT off (Goccia.VM SetMaxFrames), and the engine refuses native recursion before the
  native stack runs out whatever the bound. }

{$I Goccia.inc}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes,
  StrUtils,
  SysUtils,

  CLI.Options,
  CLI.Parser,
  CLI.Units,
  TextSemantics,

  Goccia.CLI.FileEngine,
  Goccia.CLI.Options,
  Goccia.CLI.Stdin,
  Goccia.FileExtensions,
  Goccia.Modules.Resolver,
  Goccia.Profiler,
  Goccia.Profiler.Report,
  Goccia.ScriptLoader.Input,
  Goccia.SourcePipeline,
  Goccia.Terminal.Colors,
  Goccia.TextFiles,

  Goccia.Core,
  Goccia.Embed,
  Goccia.EngineFault{$IFDEF GOCCIA_FAULT_INJECTION},
  Goccia.FaultInjection{$ENDIF};

const
  BARE_PROGRAM_NAME = 'GocciaScriptLoaderBare';
  { Bare defaults its input to stdin like GocciaRunner, so it takes the same
    no-argument rule; GocciaREPL is the right pointer for anyone who typed the
    command expecting an interactive prompt. }
  BARE_STDIN_USAGE = suStdinDefaultWithREPL;
  BARE_HONORED_SETTINGS: TGocciaHonoredSettings = [grsTimeout, grsMaxMemory,
    grsMaxInstructions, grsMaxStack];
  BARE_BOOLEAN_FLAGS: array[0..9] of string = ('--compat-label',
    '--compat-for-in-loop', '--experimental-js-module-source',
    '--warning-unsupported-features', '--strict-types',
    '--unsafe-function-constructor', '--unsafe-shadowrealm', '--deterministic',
    '--print', '--help');
  FATAL_PREFIX = 'Fatal error: ';

type
  TBareOptions = record
    Compatibility: TGocciaCompatibilityFlags;
    LabelStatementsEnabled: Boolean;
    ForInLoopsEnabled: Boolean;
    ExperimentalJSModuleSourceEnabled: Boolean;
    WarningUnsupportedFeatures: Boolean;
    StrictTypes: Boolean;
    UnsafeFunctionConstructor: Boolean;
    UnsafeShadowRealm: Boolean;
    Deterministic: Boolean;
    Print: Boolean;
    SourceType: TGocciaSourceType;
    SourceTypeExplicit: Boolean;
    FileName: string;
    FileNameExplicit: Boolean;
    SourceName: string;
    TimeoutMs: Integer;
    MaxMemoryBytes: Int64;
    MaxInstructions: Int64;
    StackSize: Integer;
    StackSizeSet: Boolean;
    ProfileModePresent: Boolean;
    ProfileMode: Goccia.CLI.Options.TGocciaProfileMode;
    ProfileOutputPath: string;
  end;

  { parse warnings (--warning-unsupported-features) on stderr, as the old
    loader's source pipeline prints them }
  TBareWarnings = class
  public
    FileName: string;
    procedure Warn(const AMessage, ASuggestion: UnicodeString;
      ALine, AColumn: Integer);
  end;

procedure TBareWarnings.Warn(const AMessage, ASuggestion: UnicodeString;
  ALine, AColumn: Integer);
begin
  WriteLn(ErrOutput, Format('Warning: %s', [UTF8Encode(AMessage)]));
  if ASuggestion <> '' then
    WriteLn(ErrOutput, Format('  Suggestion: %s',
      [UTF8Encode(ASuggestion)]));
  WriteLn(ErrOutput, Format('  --> %s:%d:%d', [FileName, ALine, AColumn]));
end;

procedure PrintUsage(var AOut: Text);
var
  Descriptor: TGocciaCompatibilityFlagDescriptor;
  Flag: TGocciaCompatibility;
begin
  WriteLn(AOut, 'Usage: GocciaScriptLoaderBare [file|-] [options]');
  WriteLn(AOut);
  WriteLn(AOut, 'Options:');
  for Flag := Low(TGocciaCompatibility) to High(TGocciaCompatibility) do
  begin
    Descriptor := CompatibilityFlagDescriptor(Flag);
    WriteLn(AOut, Format('  --%-28s %s', [Descriptor.OptionName,
      Descriptor.HelpText]));
  end;
  WriteLn(AOut, '  --strict-types                Enforce type annotations at runtime');
  WriteLn(AOut, '  --warning-unsupported-features');
  WriteLn(AOut, '                                Warn and recover for unsupported/default-disabled syntax');
  WriteLn(AOut, '  --source-type=script|module   Load entry as script source or module source (default: module if the');
  WriteLn(AOut, '                                source has a top-level import or export; .mjs and .mts infer module)');
  WriteLn(AOut, '  --source-name=PATH            Name stdin source as PATH for diagnostics and module resolution');
  WriteLn(AOut, '  --unsafe-function-constructor Enable dynamic Function constructor');
  WriteLn(AOut, '  --unsafe-shadowrealm          Enable the ShadowRealm constructor');
  WriteLn(AOut, '  --deterministic               Use fixed script-visible time, UTC, and seeded randomness');
  WriteLn(AOut, '  --print                       Print the script''s last value (incl. undefined)');
  WriteLn(AOut, '  --timeout=DURATION            Per-file cooperative timeout: 500ms, 5s, 2m, or plain milliseconds');
  WriteLn(AOut, '  --max-memory=BYTES            GC heap limit: 64MiB, 1GiB, or plain bytes (RangeError on exceed)');
  WriteLn(AOut, '  --max-instructions=N          Maximum bytecode steps before aborting');
  WriteLn(AOut, '  --max-stack=N                 Maximum call stack depth (default: the engine''s; 0 = no limit)');
  WriteLn(AOut, '  --profile=opcodes|functions|all');
  WriteLn(AOut, '                                Enable bytecode VM profiling');
  WriteLn(AOut, '  --profile-output=PATH         Write profiler JSON to PATH');
  WriteLn(AOut, '  --help                        Show this help');
  WriteLn(AOut);
  Write(AOut, StdinUsageNote(BARE_PROGRAM_NAME, BARE_STDIN_USAGE));
end;

procedure ParseSourceType(const AValue: string; var AOptions: TBareOptions);
begin
  if AValue = 'script' then
    AOptions.SourceType := stScript
  else if AValue = 'module' then
    AOptions.SourceType := stModule
  else
    raise Exception.Create('Invalid --source-type value: ' + AValue);
  AOptions.SourceTypeExplicit := True;
end;

procedure RaiseInvalidValue(const AName, AValue, AError: string);
begin
  raise TParseError.CreateFmt('Invalid value for --%s: %s (%s)',
    [AName, AValue, AError]);
end;

procedure ParseTimeout(const AValue: string; var AOptions: TBareOptions);
var
  Milliseconds: Int64;
  ErrorText: string;
begin
  if not TryParseDurationMilliseconds(AValue, Milliseconds, ErrorText) then
    RaiseInvalidValue('timeout', AValue, ErrorText);
  AOptions.TimeoutMs := Integer(Milliseconds);
end;

procedure ParseMaxMemory(const AValue: string; var AOptions: TBareOptions);
var
  ErrorText: string;
begin
  if not TryParseByteSize(AValue, AOptions.MaxMemoryBytes, ErrorText) then
    RaiseInvalidValue('max-memory', AValue, ErrorText);
end;

procedure ParseMaxInstructions(const AValue: string;
  var AOptions: TBareOptions);
var
  ErrorText: string;
begin
  if not TryParseNonNegativeCount(AValue, AOptions.MaxInstructions,
     ErrorText) then
    RaiseInvalidValue('max-instructions', AValue, ErrorText);
end;

procedure ParseMaxStack(const AValue: string; var AOptions: TBareOptions);
var
  Count: Int64;
  ErrorText: string;
begin
  if not TryParseNonNegativeCount(AValue, Count, ErrorText) then
    RaiseInvalidValue('max-stack', AValue, ErrorText);
  if Count > High(Integer) then
    RaiseInvalidValue('max-stack', AValue, TOO_LARGE_ERROR);
  AOptions.StackSize := Integer(Count);
  AOptions.StackSizeSet := True;
end;

procedure ParseProfileMode(const AValue: string; var AOptions: TBareOptions);
begin
  if AValue = 'opcodes' then
    AOptions.ProfileMode := Goccia.CLI.Options.pmOpcodes
  else if AValue = 'functions' then
    AOptions.ProfileMode := Goccia.CLI.Options.pmFunctions
  else if AValue = 'all' then
    AOptions.ProfileMode := Goccia.CLI.Options.pmAll
  else
    raise Exception.Create('Invalid --profile value: ' + AValue);
  AOptions.ProfileModePresent := True;
end;

{ A boolean flag given a value (`--print=false`) is a usage error, as on the
  shared-application binaries. }
procedure RejectFlagValue(const AArgument: string);
var
  Name: string;
  EqualPos, I: Integer;
  Flag: TGocciaCompatibility;
  IsFlag: Boolean;
begin
  EqualPos := Pos('=', AArgument);
  if (Copy(AArgument, 1, 2) <> '--') or (EqualPos = 0) then
    Exit;
  Name := Copy(AArgument, 1, EqualPos - 1);
  IsFlag := False;
  for I := Low(BARE_BOOLEAN_FLAGS) to High(BARE_BOOLEAN_FLAGS) do
    if Name = BARE_BOOLEAN_FLAGS[I] then
      IsFlag := True;
  for Flag := Low(TGocciaCompatibility) to High(TGocciaCompatibility) do
    if Name = '--' + CompatibilityFlagDescriptor(Flag).OptionName then
      IsFlag := True;
  if IsFlag then
    raise TCLIUsageError.CreateFmt(
      '%s does not take a value; got "%s". Omit the flag to leave it off',
      [Name, Copy(AArgument, EqualPos + 1, MaxInt)]);
end;

procedure ParseOptions(out AOptions: TBareOptions);
var
  Arg: string;
  Arguments: TCommandLineArguments;
  I: Integer;
begin
  AOptions := Default(TBareOptions);
  AOptions.SourceType := stDetect;
  AOptions.FileName := STDIN_PATH_MARKER;
  AOptions.ProfileMode := Goccia.CLI.Options.pmAll;

  Arguments := GetCommandLineArguments;
  for I := 0 to High(Arguments) do
  begin
    Arg := Arguments[I];
    RejectFlagValue(Arg);
    RejectUnsupportedSettingArgument(Arg, BARE_PROGRAM_NAME,
      BARE_HONORED_SETTINGS);
    if Arg = '--help' then
    begin
      PrintUsage(Output);
      Halt(0);
    end
    else if Arg = '--compat-label' then
      AOptions.LabelStatementsEnabled := True
    else if Arg = '--compat-for-in-loop' then
      AOptions.ForInLoopsEnabled := True
    else if Arg = '--experimental-js-module-source' then
      AOptions.ExperimentalJSModuleSourceEnabled := True
    else if TryApplyCompatibilityFlagArg(Arg, AOptions.Compatibility) then
    begin
      { handled by source compatibility flag registry }
    end
    else if Arg = '--warning-unsupported-features' then
      AOptions.WarningUnsupportedFeatures := True
    else if Arg = '--strict-types' then
      AOptions.StrictTypes := True
    else if Arg = '--unsafe-function-constructor' then
      AOptions.UnsafeFunctionConstructor := True
    else if Arg = '--unsafe-shadowrealm' then
      AOptions.UnsafeShadowRealm := True
    else if Arg = '--deterministic' then
      AOptions.Deterministic := True
    else if Arg = '--print' then
      AOptions.Print := True
    else if (Arg = '--mode') or StartsStr('--mode=', Arg) then
      RejectRemovedExecutionModeArgument(Arg)
    else if StartsStr('--source-type=', Arg) then
      ParseSourceType(Copy(Arg, Length('--source-type=') + 1, MaxInt),
        AOptions)
    else if StartsStr('--source-name=', Arg) then
      AOptions.SourceName := Copy(Arg, Length('--source-name=') + 1,
        MaxInt)
    else if StartsStr('--timeout=', Arg) then
      ParseTimeout(Copy(Arg, Length('--timeout=') + 1, MaxInt), AOptions)
    else if StartsStr('--max-memory=', Arg) then
      ParseMaxMemory(Copy(Arg, Length('--max-memory=') + 1, MaxInt),
        AOptions)
    else if StartsStr('--max-instructions=', Arg) then
      ParseMaxInstructions(Copy(Arg, Length('--max-instructions=') + 1,
        MaxInt), AOptions)
    else if StartsStr('--max-stack=', Arg) then
      ParseMaxStack(Copy(Arg, Length('--max-stack=') + 1, MaxInt),
        AOptions)
    else if (Arg = '--stack-size') or StartsStr('--stack-size=', Arg) then
      raise TCLIUsageError.CreateFmt(
        '--stack-size was removed in GocciaScript %s; use --max-stack instead',
        [OPTIONS_REMOVED_IN_VERSION])
    else if TryHandleCapabilityArgument(Arg, BARE_PROGRAM_NAME, []) then
      { Bare engines grant nothing, so a deny changes nothing. }
    else if StartsStr('--profile=', Arg) then
      ParseProfileMode(Copy(Arg, Length('--profile=') + 1, MaxInt),
        AOptions)
    else if StartsStr('--profile-output=', Arg) then
      AOptions.ProfileOutputPath :=
        Copy(Arg, Length('--profile-output=') + 1, MaxInt)
    else if StartsStr('--', Arg) or
       ((Length(Arg) = 2) and (Arg[1] = '-') and (Arg[2] <> '-')) then
      raise Exception.Create('Unknown option: ' + Arg)
    else if not AOptions.FileNameExplicit then
    begin
      AOptions.FileName := Arg;
      AOptions.FileNameExplicit := True;
    end
    else
      raise Exception.Create('Unexpected argument: ' + Arg);
  end;

  if not AOptions.SourceTypeExplicit and
     IsModuleSourceFileName(AOptions.FileName) then
    AOptions.SourceType := stModule;
  if (AOptions.ProfileOutputPath <> '') and
     not AOptions.ProfileModePresent then
    raise Exception.Create(
      '--profile-output requires --profile=opcodes|functions|all');
end;

function ReadBareSource(const AFileName: string): TStringList;
begin
  if IsStdinPath(AFileName) then
    Result := ReadSourceFromText(Input)
  else
    Result := CreateFileTextLines(ReadUTF8FileText(AFileName));
end;

{ the language profile the options select (GocciaScript's recommended one with
  the compatibility flags and unsafe options turned on) }
function BareProfile(const AOptions: TBareOptions): TLangProfile;
begin
  Result := RecommendedProfile;
  Result.Asi := cfASI in AOptions.Compatibility;
  Result.VarDecl := cfVar in AOptions.Compatibility;
  Result.FuncDecl := cfFunction in AOptions.Compatibility;
  Result.ForLoop := cfTraditionalFor in AOptions.Compatibility;
  Result.WhileLoop := cfWhileLoops in AOptions.Compatibility;
  Result.LooseEq := cfLooseEquality in AOptions.Compatibility;
  Result.NonStrict := cfNonStrictMode in AOptions.Compatibility;
  Result.ArgumentsObject := cfArgumentsObject in AOptions.Compatibility;
  Result.Labels := AOptions.LabelStatementsEnabled;
  Result.ForIn := AOptions.ForInLoopsEnabled;
  Result.AnnexB := cfAnnexB in AOptions.Compatibility;
  Result.WarningUnsupported := AOptions.WarningUnsupportedFeatures;
  Result.StrictTypes := AOptions.StrictTypes;
  Result.FunctionCtor := AOptions.UnsafeFunctionConstructor;
  Result.ShadowRealm := AOptions.UnsafeShadowRealm;
end;

procedure ConfigureEngine(const AEngine: TGocciaEngine;
  const AOptions: TBareOptions);
begin
  AEngine.Bare := True;
  AEngine.ExposeTimers := False;
  AEngine.Profile := BareProfile(AOptions);
  case AOptions.SourceType of
    stScript:
      AEngine.SourceKind := gskScript;
    stModule:
      AEngine.SourceKind := gskModule;
  else
    AEngine.SourceKind := gskDetect;
  end;
  AEngine.JSModuleSources := AOptions.ExperimentalJSModuleSourceEnabled;
  AEngine.Deterministic := AOptions.Deterministic;
  AEngine.TimeoutMs := AOptions.TimeoutMs;
  AEngine.MaxInstructions := AOptions.MaxInstructions;
  AEngine.MaxMemory := AOptions.MaxMemoryBytes;
  if AOptions.StackSizeSet then
    AEngine.MaxFrames := AOptions.StackSize;
  if AOptions.Print then
    AEngine.ResultForm := grfString;
end;

procedure InitializeProfiler(const AEngine: TGocciaEngine;
  const AOptions: TBareOptions);
begin
  if not AOptions.ProfileModePresent then
    Exit;
  TGocciaProfiler.Initialize;
  TGocciaProfiler.Instance.Enabled := True;
  case AOptions.ProfileMode of
    Goccia.CLI.Options.pmOpcodes:
      TGocciaProfiler.Instance.Mode := [Goccia.Profiler.pmOpcodes];
    Goccia.CLI.Options.pmFunctions:
      TGocciaProfiler.Instance.Mode := [Goccia.Profiler.pmFunctions];
    Goccia.CLI.Options.pmAll:
      TGocciaProfiler.Instance.Mode := [Goccia.Profiler.pmOpcodes,
        Goccia.Profiler.pmFunctions];
  end;
  TGocciaProfiler.Instance.BeginEntry;
  AEngine.ProfileOpcodes :=
    Goccia.Profiler.pmOpcodes in TGocciaProfiler.Instance.Mode;
  AEngine.ProfileFunctions :=
    Goccia.Profiler.pmFunctions in TGocciaProfiler.Instance.Mode;
end;

procedure WriteProfilerReport(const AOptions: TBareOptions);
begin
  if not AOptions.ProfileModePresent or
     (AOptions.ProfileOutputPath = '') or
     not Assigned(TGocciaProfiler.Instance) then
    Exit;
  WriteProfileJSON(TGocciaProfiler.Instance, AOptions.ProfileOutputPath);
end;

{ a failed run on stderr: the error with its code frame, as GocciaRunner
  words it; a spent limit as the old loader's "Error: ..." line }
procedure ReportFailure(const ADisplayName: string;
  const AResult: TGocciaRunResult; const ASource: TStringList;
  const AEngine: TGocciaEngine; const AOptions: TBareOptions);
var
  Text: string;
begin
  Text := FailureText(ADisplayName, AResult, ASource, AEngine, '', nil, True,
    True, IsColorTerminal, AOptions.TimeoutMs, AOptions.MaxInstructions);
  if (AResult.Aborted <> gaNone) and StartsStr(FATAL_PREFIX, Text) then
    Text := 'Error: ' + Copy(Text, Length(FATAL_PREFIX) + 1, MaxInt);
  WriteLn(ErrOutput, TrimRight(Text));
end;

function RunBare(const AOptions: TBareOptions): Integer;
var
  DisplayName: string;
  Engine: TGocciaEngine;
  Resolver: TGocciaModuleResolver;
  RunResult: TGocciaRunResult;
  Source: TStringList;
  Warnings: TBareWarnings;
begin
  Result := 0;
  Source := ReadBareSource(AOptions.FileName);
  Warnings := TBareWarnings.Create;
  try
    if AOptions.SourceName <> '' then
      DisplayName := AOptions.SourceName
    else if IsStdinPath(AOptions.FileName) then
      DisplayName := STDIN_FILE_NAME
    else
      DisplayName := AOptions.FileName;
    Warnings.FileName := DisplayName;
    { the old engine's default resolution (relative and absolute paths; a bare
      specifier is refused), against the entry's directory }
    Resolver := TGocciaModuleResolver.Create(
      ExtractFilePath(ExpandFileName(DisplayName)));
    Engine := TGocciaEngine.Create;
    try
      ConfigureEngine(Engine, AOptions);
      Engine.Resolver := Resolver;
      Engine.OnWarning := Warnings.Warn;
      InitializeProfiler(Engine, AOptions);
      RunResult := Engine.Run(DisplayName, StringListToEngineSourceText(Source));
      if RunResult.Ok then
      begin
        if RunResult.HasResult then
          WriteLn(UTF8Encode(RunResult.ResultText));
      end
      else
      begin
        ReportFailure(DisplayName, RunResult, Source, Engine, AOptions);
        Result := 1;
      end;
    finally
      Engine.Free;
      Resolver.Free;
    end;
  finally
    try
      WriteProfilerReport(AOptions);
    finally
      if Assigned(TGocciaProfiler.Instance) then
        TGocciaProfiler.Shutdown;
      Warnings.Free;
      Source.Free;
    end;
  end;
end;

var
  Options: TBareOptions;
  SetupError: UnicodeString;
  SnapshotEngine: TGocciaEngine;
begin
  { the build's prelude snapshot stage (build.pas sets GOCCIA_SNAPSHOT_OUT): the
    bare host's preludes compiled once and written for the embedding build }
  if GetEnvironmentVariable('GOCCIA_SNAPSHOT_OUT') <> '' then
  begin
    try
      SetupError := '';
      SnapshotEngine := TGocciaEngine.Create;
      SnapshotEngine.Bare := True;
      if not SnapshotEngine.PrepareHost(SetupError) then
      begin
        WriteLn(ErrOutput, 'Error: ', UTF8Encode(SetupError));
        Halt(1);
      end;
    except
      on E: Exception do
      begin
        if IsEngineFault(E) then
          AbortOnEngineFault(BARE_PROGRAM_NAME, E);
        raise;
      end;
    end;
    Halt(0);
  end;
  Options := Default(TBareOptions);
  try
    {$IFDEF GOCCIA_FAULT_INJECTION}
    InjectFaultAt('startup');
    {$ENDIF}
    ParseOptions(Options);
    { clig.dev no-argument rule — see Goccia.CLI.Stdin.  Bare has its own
      argument parser, so the shared TGocciaCLIApplication gate does not
      cover it and the same decision is applied here. }
    if DecideStdinInput(Options.FileNameExplicit,
      Options.FileNameExplicit and IsStdinPath(Options.FileName),
      IsInputTerminal) = sdShowUsage then
    begin
      PrintUsage(ErrOutput);
      WriteLn(ErrOutput);
      Write(ErrOutput, NoInputAtTerminalMessage(BARE_PROGRAM_NAME,
        BARE_STDIN_USAGE));
      ExitCode := EXIT_CODE_USAGE;
    end
    else
      ExitCode := RunBare(Options);
  except
    on E: TCLIUsageError do
    begin
      WriteLn(ErrOutput, 'Error: ', E.Message);
      ExitCode := EXIT_CODE_USAGE;
    end;
    on E: Exception do
    begin
      { the bare loader runs its file on the main thread: a fault in it, or outside it, lands here (ADR 0199) }
      if IsEngineFault(E) then
        if Options.FileName <> '' then
          AbortOnEngineFault(Options.FileName, E)
        else
          AbortOnEngineFault(BARE_PROGRAM_NAME, E);
      WriteLn(ErrOutput, 'Error: ', E.Message);
      ExitCode := 1;
    end;
  end;
end.
