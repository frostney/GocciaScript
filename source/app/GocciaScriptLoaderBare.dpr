program GocciaScriptLoaderBare;

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

  Goccia.Arguments.Collection,
  Goccia.Builtins.GlobalShadowRealm,
  Goccia.CLI.Options,
  Goccia.CLI.Stdin,
  Goccia.Engine,
  Goccia.Error,
  Goccia.Error.Detail,
  Goccia.Executor,
  Goccia.Executor.Bytecode,
  Goccia.Executor.Interpreter,
  Goccia.FileExtensions,
  Goccia.GarbageCollector,
  Goccia.InstructionLimit,
  Goccia.Modules.Resolver,
  Goccia.Profiler,
  Goccia.Profiler.Report,
  Goccia.ScriptLoader.Input,
  Goccia.SourcePipeline,
  Goccia.StackLimit,
  Goccia.Terminal.Colors,
  Goccia.TextFiles,
  Goccia.Timeout,
  Goccia.Values.Error,
  Goccia.Values.NativeFunction,
  Goccia.Values.Primitives,
  Goccia.VM.Exception;

const
  BARE_PRINT_GLOBAL_NAME = 'print';
  BARE_PROGRAM_NAME = 'GocciaScriptLoaderBare';
  { Bare defaults its input to stdin like GocciaScriptLoader, so it takes
    the same no-argument rule; GocciaREPL is the right pointer for anyone
    who typed the command expecting an interactive prompt. }
  BARE_STDIN_USAGE = suStdinDefaultWithREPL;
  BARE_DEFAULT_MAX_STACK_DEPTH = 10000;

type
  TBarePrintHost = class
  public
    function Print(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
  end;

  TBareExecutionMode = (bemInterpreted, bemBytecode);

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
    Mode: TBareExecutionMode;
    SourceType: TGocciaSourceType;
    SourceTypeExplicit: Boolean;
    FileName: string;
    FileNameExplicit: Boolean;
    SourceName: string;
    TimeoutMs: Integer;
    MaxMemoryBytes: Int64;
    MaxInstructions: Int64;
    StackSize: Integer;
    ProfileModePresent: Boolean;
    ProfileMode: Goccia.CLI.Options.TGocciaProfileMode;
    ProfileOutputPath: string;
  end;

function TBarePrintHost.Print(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  I: Integer;
  Line: string;
begin
  Line := '';
  for I := 0 to AArgs.Length - 1 do
  begin
    if I > 0 then
      Line := Line + ' ';
    Line := Line + AArgs.GetElement(I).ToStringLiteral.Value;
  end;
  WriteLn(Line);
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
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
  WriteLn(AOut, '  --mode=interpreted|bytecode   Execution mode (default: interpreted)');
  WriteLn(AOut, '  --source-type=script|module   Load entry as script source or module source (.mjs infers module)');
  WriteLn(AOut, '  --source-name=PATH            Name stdin source as PATH for diagnostics and module resolution');
  WriteLn(AOut, '  --unsafe-function-constructor Enable dynamic Function constructor');
  WriteLn(AOut, '  --unsafe-shadowrealm          Enable the ShadowRealm constructor');
  WriteLn(AOut, '  --deterministic               Use fixed script-visible time, UTC, and seeded randomness');
  WriteLn(AOut, '  --print                       Print the script''s last value (incl. undefined)');
  WriteLn(AOut, '  --timeout=DURATION            Per-file cooperative timeout: 500ms, 5s, 2m, or plain milliseconds');
  WriteLn(AOut, '  --max-memory=BYTES            GC heap limit: 64MiB, 1GiB, or plain bytes (RangeError on exceed)');
  WriteLn(AOut, '  --max-instructions=N          Maximum bytecode steps before aborting');
  WriteLn(AOut, '  --max-stack=N                 Maximum call stack depth (default: 10000; 0 = no limit)');
  WriteLn(AOut, '  --profile=opcodes|functions|all');
  WriteLn(AOut, '                                Enable bytecode VM profiling (forces bytecode mode)');
  WriteLn(AOut, '  --profile-output=PATH         Write profiler JSON to PATH');
  WriteLn(AOut, '  --help                        Show this help');
  WriteLn(AOut);
  Write(AOut, StdinUsageNote(BARE_PROGRAM_NAME, BARE_STDIN_USAGE));
end;

procedure ParseMode(const AValue: string; var AOptions: TBareOptions);
begin
  if AValue = 'interpreted' then
    AOptions.Mode := bemInterpreted
  else if AValue = 'bytecode' then
    AOptions.Mode := bemBytecode
  else
    raise Exception.Create('Invalid --mode value: ' + AValue);
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
  AOptions.Mode := bemBytecode;
end;

procedure InitializeProfiler(const AOptions: TBareOptions);
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
end;

procedure WriteProfilerReport(const AOptions: TBareOptions);
begin
  if not AOptions.ProfileModePresent or
     (AOptions.ProfileOutputPath = '') or
     not Assigned(TGocciaProfiler.Instance) then
    Exit;
  WriteProfileJSON(TGocciaProfiler.Instance, AOptions.ProfileOutputPath);
end;

const
  BARE_HONORED_SETTINGS: TGocciaHonoredSettings = [grsTimeout, grsMaxMemory,
    grsMaxInstructions, grsMaxStack];
  BARE_BOOLEAN_FLAGS: array[0..9] of string = ('--compat-label',
    '--compat-for-in-loop', '--experimental-js-module-source',
    '--warning-unsupported-features', '--strict-types',
    '--unsafe-function-constructor', '--unsafe-shadowrealm', '--deterministic',
    '--print', '--help');

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
  AOptions.Compatibility := [];
  AOptions.LabelStatementsEnabled := False;
  AOptions.ForInLoopsEnabled := False;
  AOptions.ExperimentalJSModuleSourceEnabled := False;
  AOptions.WarningUnsupportedFeatures := False;
  AOptions.StrictTypes := False;
  AOptions.UnsafeFunctionConstructor := False;
  AOptions.UnsafeShadowRealm := False;
  AOptions.Deterministic := False;
  AOptions.Print := False;
  AOptions.Mode := bemInterpreted;
  AOptions.SourceType := stScript;
  AOptions.SourceTypeExplicit := False;
  AOptions.FileName := STDIN_PATH_MARKER;
  AOptions.FileNameExplicit := False;
  AOptions.SourceName := '';
  AOptions.TimeoutMs := 0;
  AOptions.MaxMemoryBytes := 0;
  AOptions.MaxInstructions := 0;
  AOptions.StackSize := BARE_DEFAULT_MAX_STACK_DEPTH;
  AOptions.ProfileModePresent := False;
  AOptions.ProfileMode := Goccia.CLI.Options.pmAll;
  AOptions.ProfileOutputPath := '';

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
    else if StartsStr('--mode=', Arg) then
      ParseMode(Copy(Arg, Length('--mode=') + 1, MaxInt), AOptions)
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
    else if StartsStr('--', Arg) then
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
  if AOptions.ProfileModePresent then
    AOptions.Mode := bemBytecode;
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

procedure ConfigureEngine(const AEngine: TGocciaEngine;
  const AExecutor: TGocciaExecutor; const AOptions: TBareOptions);
begin
  if AOptions.Deterministic then
    AEngine.HostEnvironment.UseDeterministicProfile;
  AEngine.Compatibility := AOptions.Compatibility;
  AEngine.LabelStatementsEnabled := AOptions.LabelStatementsEnabled;
  AEngine.ForInLoopsEnabled := AOptions.ForInLoopsEnabled;
  AEngine.ExperimentalJSModuleSourceEnabled :=
    AOptions.ExperimentalJSModuleSourceEnabled;
  AEngine.WarningUnsupportedFeatures := AOptions.WarningUnsupportedFeatures;
  AEngine.StrictTypes := AOptions.StrictTypes;
  AEngine.SourceType := AOptions.SourceType;
  AEngine.FunctionConstructor.Enabled := AOptions.UnsafeFunctionConstructor;
  if AOptions.UnsafeShadowRealm then
    EnableShadowRealm(AEngine);
  if AExecutor is TGocciaBytecodeExecutor then
    TGocciaBytecodeExecutor(AExecutor).GlobalBackedTopLevel :=
      AOptions.SourceType = stScript;
end;

procedure RegisterBareGlobals(const AEngine: TGocciaEngine;
  const APrintHost: TBarePrintHost);
begin
  AEngine.RegisterGlobal(BARE_PRINT_GLOBAL_NAME,
    TGocciaNativeFunctionValue.CreateWithoutPrototype(APrintHost.Print,
      BARE_PRINT_GLOBAL_NAME, -1));
  AEngine.RefreshGlobalThis;
end;

procedure PrintResult(const AResult: TGocciaValue; const APrint: Boolean);
begin
  if APrint and Assigned(AResult) then
    WriteLn(AResult.ToStringLiteral.Value);
end;

function CreateExecutorForMode(
  const AMode: TBareExecutionMode): TGocciaExecutor;
begin
  case AMode of
    bemInterpreted: Result := TGocciaInterpreterExecutor.Create;
    bemBytecode: Result := TGocciaBytecodeExecutor.Create;
  end;
end;

function RunBare(const AOptions: TBareOptions): Integer;
var
  DisplayName: string;
  Engine: TGocciaEngine;
  Executor: TGocciaExecutor;
  GC: TGarbageCollector;
  PrintHost: TBarePrintHost;
  ScriptResult: TGocciaScriptResult;
  Source: TStringList;
begin
  Result := 0;
  Source := ReadBareSource(AOptions.FileName);
  try
    InitializeProfiler(AOptions);
    if AOptions.SourceName <> '' then
      DisplayName := AOptions.SourceName
    else if IsStdinPath(AOptions.FileName) then
      DisplayName := STDIN_FILE_NAME
    else
      DisplayName := AOptions.FileName;

    StartExecutionTimeout(AOptions.TimeoutMs);
    StartInstructionLimit(AOptions.MaxInstructions);
    SetMaxStackDepth(AOptions.StackSize);
    PrintHost := TBarePrintHost.Create;
    try
      Executor := CreateExecutorForMode(AOptions.Mode);
      try
        Engine := TGocciaEngine.Create(DisplayName, Source, Executor);
        try
          ConfigureEngine(Engine, Executor, AOptions);
          GC := TGarbageCollector.Instance;
          if Assigned(GC) and (AOptions.MaxMemoryBytes > 0) then
            GC.MaxBytes := AOptions.MaxMemoryBytes;
          RegisterBareGlobals(Engine, PrintHost);
          try
            ScriptResult := Engine.Execute;
            PrintResult(ScriptResult.Result, AOptions.Print);
          except
            on E: EGocciaBytecodeThrow do
            begin
              WriteLn(ErrOutput, FormatThrowDetail(E.ThrownValue,
                DisplayName, Source, IsColorTerminal,
                Engine.ModuleLoader.DiagnosticScope.Principal,
                E.Suggestion));
              Result := 1;
            end;
            on E: TGocciaThrowValue do
            begin
              WriteLn(ErrOutput, FormatThrowDetail(E.Value, DisplayName,
                Source, IsColorTerminal,
                Engine.ModuleLoader.DiagnosticScope.Principal,
                E.Suggestion));
              Result := 1;
            end;
          end;
        finally
          Engine.Free;
        end;
      finally
        Executor.Free;
      end;
    finally
      PrintHost.Free;
    end;
  finally
    try
      WriteProfilerReport(AOptions);
    finally
      if Assigned(TGocciaProfiler.Instance) then
        TGocciaProfiler.Shutdown;
      Source.Free;
    end;
  end;
end;

var
  Options: TBareOptions;
begin
  Options := Default(TBareOptions);
  try
    ParseOptions(Options);
    { clig.dev no-argument rule — see Goccia.CLI.Stdin.  Bare has its
      own argument parser, so the shared TGocciaCLIApplication gate
      does not cover it and the same decision is applied here. }
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
    on E: TGocciaTimeoutError do
    begin
      WriteLn(ErrOutput, 'Error: ', E.Message);
      ExitCode := 1;
    end;
    on E: TGocciaError do
    begin
      WriteLn(ErrOutput, FormatHostErrorDiagnostic(E, IsColorTerminal));
      ExitCode := 1;
    end;
    on E: Exception do
    begin
      WriteLn(ErrOutput, 'Error: ', E.Message);
      ExitCode := 1;
    end;
  end;
end.
